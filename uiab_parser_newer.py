"""
NYUIAB Appeal Decision Parser  —  v2
=====================================
Run:
    python uiab_parser_v2.py /path/to/folder/   # all PDFs in folder
    python uiab_parser_v2.py case.pdf            # single file

Outputs two CSVs next to the input:
    uiab_outcomes_v2.csv          — one row per AB number
    uiab_outcomes_quality_v2.csv  — parsing quality / QA flags

Key improvements over v1
------------------------
1.  BUG FIX – DECISION: anchor regex.
    Original used `\bDECISION:\b` which never matches because `\b` after `:` requires
    the next char to be a word char, but the pattern ends at `:`.
    Fixed to `\bDECISION:` (no trailing `\b`).

2.  BUG FIX – remand/rescind cases with no DECISION: block.
    When the Board issues ORDERED paragraphs (remand decisions), there is no
    "DECISION:" heading — the existing fallback grabbed the last 120 lines of the
    entire doc, which includes the header.  Now we detect the ORDERED pattern
    explicitly and treat it as a remand without requiring a DECISION: block.

3.  BUG FIX – 613297 "who appealed" wrong.
    The employer appealing *the judge's decision* is a common phrase but
    sometimes the doc only says "employer appealed"; claimant is the main
    appellant but that phrase appears later.  Now we scan only the intro block
    (~3 000 chars) and prioritise commissioner > employer > claimant, with
    a dedicated check for "claimant appealed" / "claimant applied".

4.  NEW FIELD – who_appealed (claimant / employer / commissioner / unknown).

5.  NEW FIELD – decision_mode (ordered_remand / standard).
    Helps downstream analysis distinguish true remands from ordinary decisions.

6.  NEW FIELD – procedural_remand flag (0/1).
    Set when the Board's action is a rescind+remand (no merits reached).

7.  IMPROVED – multi-case paragraph scoping.
    The old scoping code was invoked only in the DECISION block, but the block
    detection itself was broken (bug #1).  With the anchor fixed the block is
    now correctly trimmed and scoping works as intended.

8.  IMPROVED – employer case detection.
    When the *employer* (not the claimant) initiated the original hearing and
    the initial determination favoured the claimant, the benefits sentence may
    say "allowed" even though the employer is appealing.  We record this
    correctly; no logic change needed because the parser already reads the
    last benefits sentence from the Board's actual decision.

9.  IMPROVED – "continued in effect" multi-word action.
    The existing ACTION_REGEX already had this, but it was only matched inside
    the DECISION block (which was never found due to bug #1).  Now fixed.

10. IMPROVED – issue_type field.
    Rough categorisation extracted from the intro:
    voluntary_quit | misconduct | availability | capability | misrepresentation |
    overpayment | employer_contributions | other
"""

from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import pandas as pd


# ============================================================
# 1.  TEXT EXTRACTION
# ============================================================

def read_pdf_text(path: Path) -> str:
    """Extract text with PyMuPDF (fitz)."""
    import fitz
    doc = fitz.open(str(path))
    out = []
    for page in doc:
        t = page.get_text("text") or ""
        if len(t.strip()) < 50:
            blocks = page.get_text("blocks") or []
            block_text = " ".join(
                b[4] for b in blocks if len(b) > 4 and isinstance(b[4], str)
            )
            if len(block_text.strip()) > len(t.strip()):
                t = block_text
        out.append(t)
    return "\n".join(out).replace("\r", "")


def normalize(s: str) -> str:
    s = s.replace("\u2019", "'").replace("\u201c", '"').replace("\u201d", '"')
    s = s.replace("\xa0", " ")
    s = re.sub(r"[ \t]+", " ", s)
    s = re.sub(r"[ \t]+\n", "\n", s)
    s = re.sub(r"\n{3,}", "\n\n", s)
    return s.strip()


# ============================================================
# 2.  HEADER FIELDS
# ============================================================

RE_MAILED  = re.compile(r"^\s*Mailed\s+and\s+Filed:\s*(.+?)\s*$", re.I | re.M)
RE_AB_NO   = re.compile(r"Appeal\s+Board\s+No\.\s*([0-9]{5,6}(?:\s*[A-Z])?)\s*$", re.I | re.M)
RE_PRESENT = re.compile(r"^\s*PRESENT:\s*(.+?)\s*$", re.I | re.M)
RE_ALJ_CASE = re.compile(
    r"\bA\.?\s*L\.?\s*J\.?\s*Case\s*No\.?\s*([0-9]{2,3}\s*-\s*[0-9]{4,6}|[0-9]{2,3}-[0-9]{4,6})\b",
    re.I,
)


def safe_first(m: Optional[re.Match]) -> str:
    return m.group(1).strip() if m else ""


def extract_header_fields(text: str) -> dict:
    return {
        "mailed_and_filed_date": safe_first(RE_MAILED.search(text)),
        "appeal_board_no":       safe_first(RE_AB_NO.search(text)),
        "board_member":          safe_first(RE_PRESENT.search(text)),
    }


def extract_alj_case_nos(text: str) -> str:
    found = {m.group(1).replace(" ", "") for m in RE_ALJ_CASE.finditer(text)}
    return ";".join(sorted(found))


# ============================================================
# 3.  WHO APPEALED
# ============================================================

RE_COMMISSIONER_APPEALS = re.compile(
    r"\bthe\s+commissioner\s+of\s+labor\s+appeals\b", re.I
)
RE_EMPLOYER_APPEALS = re.compile(
    r"\bthe\s+employer\s+appeals\b"
    r"|\bemployer\s+appealed\s+the\s+Judge\b"
    r"|\bemployer\s+appealed\s+the\s+decision\b",
    re.I,
)
RE_CLAIMANT_APPEALS = re.compile(
    r"\bthe\s+claimant\s+appeals\b"
    r"|\bclaimant\s+appealed\s+the\b"
    r"|\bclaimant\s+applied\s+to\s+the\s+Appeal\s+Board\b",
    re.I,
)
# Board acting on its own motion (§ 534 / § 620(3)) — treat as claimant-initiated
RE_BOARD_OWN_MOTION = re.compile(
    r"\bAppeal\s+Board,\s+on\s+its\s+motion\b"
    r"|\bBoard,\s+on\s+its\s+motion\b",
    re.I,
)


def extract_who_appealed(text: str) -> str:
    """
    Scan only the intro block (first ~3 000 chars after the header).
    Priority: commissioner > employer > claimant > board_own_motion.
    """
    intro = text[:3000]
    if RE_COMMISSIONER_APPEALS.search(intro):
        return "commissioner"
    if RE_EMPLOYER_APPEALS.search(intro):
        return "employer"
    if RE_CLAIMANT_APPEALS.search(intro):
        return "claimant"
    if RE_BOARD_OWN_MOTION.search(intro):
        return "claimant"  # board's own motion reopens on claimant's behalf
    return "unknown"


# ============================================================
# 4.  ISSUE TYPE (rough categorisation)
# ============================================================

ISSUE_PATTERNS: List[Tuple[str, re.Pattern]] = [
    ("voluntary_quit",         re.compile(r"\bvoluntarily?\s+separated\b|\bvoluntary\s+quit\b", re.I)),
    ("misconduct",             re.compile(r"\bmisconduct\s+in\s+connection\b|\bdisqualif\w+.*misconduct\b", re.I)),
    ("availability",           re.compile(r"\bnot\s+available\s+for\s+employment\b", re.I)),
    ("capability",             re.compile(r"\bnot\s+capable\s+of\s+work\b", re.I)),
    ("misrepresentation",      re.compile(r"\bwillful\s+misrepresentation\b", re.I)),
    ("employer_contributions", re.compile(r"\bcontributions\s+due\b|\bfraud\s+penalty\b.*\bLabor\s+Law\s+§\s*570\b", re.I)),
    ("overpayment",            re.compile(r"\boverpayment\b", re.I)),
]


def extract_issue_type(text: str) -> str:
    intro = text[:4000]
    found = []
    for label, pat in ISSUE_PATTERNS:
        if pat.search(intro):
            found.append(label)
    # employer_contributions takes precedence (it's a distinct case type)
    if "employer_contributions" in found:
        return "employer_contributions"
    if not found:
        return "other"
    # return the first match (order matters — see list above)
    return found[0]


# ============================================================
# 5.  DECISION BLOCK DETECTION
# ============================================================

# FIX #1: was `\bDECISION:\b` — trailing \b never matches after ':'
RE_DECISION_ANCHOR = re.compile(r"\bDECISION:", re.I)

# FIX #2: remand cases use ORDERED paragraphs with no DECISION: heading
RE_ORDERED_REMAND  = re.compile(r"\bORDERED\b.*?\bremanded\b", re.I | re.S)
RE_ORDERED_RESCIND = re.compile(r"\bORDERED\b.*?\brescinded\b", re.I | re.S)


def last_decision_block(text: str) -> Tuple[str, str]:
    """
    Returns (block_text, decision_mode).
    decision_mode: 'standard' | 'ordered_remand'

    Some cases (e.g. 628202) have a DECISION: heading AND ORDERED remand
    paragraphs later in the same block.  We detect this by checking whether
    the DECISION block itself contains ORDERED+rescinded/remanded language.
    """
    # Try standard DECISION: heading
    last = None
    for m in RE_DECISION_ANCHOR.finditer(text):
        last = m.start()

    if last is not None:
        block = text[last:].strip()
        # If the block also contains ORDERED remand/rescind → treat as remand
        if RE_ORDERED_REMAND.search(block) or RE_ORDERED_RESCIND.search(block):
            return block, "ordered_remand"
        return block, "standard"

    # No DECISION: heading — check for ORDERED remand pattern in full text
    if RE_ORDERED_REMAND.search(text) or RE_ORDERED_RESCIND.search(text):
        m = re.search(r"\bORDERED\b", text, re.I)
        if m:
            return text[m.start():].strip(), "ordered_remand"

    # Ultimate fallback: last 120 lines
    lines = text.splitlines()
    return "\n".join(lines[-120:]).strip(), "standard"


# ============================================================
# 6.  PARAGRAPH RECONSTRUCTION & CASE SCOPING
# ============================================================

RE_AB_CASE_MENTION = re.compile(
    r"\bAppeal\s+Board\s+No(?:s)?\.?\s*([0-9A-Z,\sand\-]+)",
    re.I,
)
RE_ANY_AB_NUMBER = re.compile(r"\b([0-9]{5,6}[A-Z]?)\b")


def reconstruct_paragraphs(block: str) -> List[str]:
    raw_paras = re.split(r"\n\s*\n+", block.strip())
    paras: List[str] = []
    for para in raw_paras:
        lines = [ln.strip() for ln in para.splitlines() if ln.strip()]
        if not lines:
            continue
        joined = re.sub(r"\s+", " ", " ".join(lines)).strip()
        if joined:
            paras.append(joined)

    # If still one giant para, try to split on sentence boundaries
    if len(paras) == 1:
        text = paras[0]
        text = re.sub(
            r"(?<=[\.\?!])\s+(?=(The\s+claimant|In\s+Appeal\s+Board\s+No|The\s+Commissioner|DECISION:|ORDERED))",
            "\n",
            text,
            flags=re.I,
        )
        paras = [p.strip() for p in text.split("\n") if p.strip()]

    return paras


def extract_paragraph_case_numbers(paragraph: str) -> Set[str]:
    nums: Set[str] = set()
    for m in RE_AB_CASE_MENTION.finditer(paragraph):
        chunk = m.group(1)
        nums.update(RE_ANY_AB_NUMBER.findall(chunk))
    return nums


def paragraph_applies_to_case(paragraph: str, current_case: str) -> bool:
    """
    Returns True if this paragraph is about our case specifically,
    or if it makes no case-specific mention at all (applies to all).
    """
    nums = extract_paragraph_case_numbers(paragraph)
    if not nums:
        return True
    return current_case in nums


# ============================================================
# 7.  OUTCOME EXTRACTION
# ============================================================

ACTION_MAP = {
    "affirmed":            "upheld",
    "sustained":           "upheld",
    "continued in effect": "upheld",
    "modified":            "modified",
    "amended":             "modified",
    "reversed":            "overruled",
    "rescinded":           "overruled",   # in DECISION context, rescind = overrule
    "overruled":           "overruled",
    "vacated":             "overruled",
    "dismissed":           "overruled",
    "remanded":            "remanded",
    "referred":            "remanded",
}
ACTION_PATTERNS = sorted(ACTION_MAP.keys(), key=len, reverse=True)
ACTION_REGEX = re.compile(
    r"\b(" + "|".join(re.escape(x) for x in ACTION_PATTERNS) + r")\b",
    re.I,
)
RE_INITIAL_ACTION = re.compile(
    r"\b(affirmed|sustained|modified|amended|reversed|rescinded|overruled|vacated|dismissed)\b",
    re.I,
)

# Benefits outcome — strict sentence patterns
RE_BEN_ALLOWED = re.compile(r"\bThe\s+claimant\s+is\s+allowed\s+benefits\b", re.I)
RE_BEN_DENIED  = re.compile(r"\bThe\s+claimant\s+is\s+denied\s+benefits\b",  re.I)


def extract_benefits_outcome(block: str) -> Tuple[str, str]:
    """
    Scan the full decision block for the last benefits sentence.
    Returns (outcome, evidence_snippet).
    """
    text = re.sub(r"\s+", " ", block).strip()
    hits: List[Tuple[int, str, str]] = []
    for m in RE_BEN_ALLOWED.finditer(text):
        hits.append((m.start(), "allowed", m.group(0).strip()))
    for m in RE_BEN_DENIED.finditer(text):
        hits.append((m.start(), "denied", m.group(0).strip()))
    if not hits:
        return "", ""
    hits.sort(key=lambda x: x[0])
    _, outcome, evidence = hits[-1]
    return outcome, evidence


def split_into_sentences(text: str) -> List[str]:
    text = re.sub(r"\s+", " ", text).strip()
    return [s.strip() for s in re.split(r"(?<=\.)\s+", text) if s.strip()]


def action_bucket_for_initial(paragraph: str) -> str:
    """Extract action word attached to 'initial determination' in the same sentence."""
    for sentence in split_into_sentences(paragraph):
        if re.search(r"\binitial\s+determination(?:s)?\b", sentence, re.I):
            m = RE_INITIAL_ACTION.search(sentence)
            if m:
                return ACTION_MAP.get(m.group(1).lower(), "")
    return ""


def first_action_bucket(paragraph: str) -> str:
    for m in ACTION_REGEX.finditer(paragraph):
        bucket = ACTION_MAP.get(m.group(1).lower(), "")
        if bucket:
            return bucket
    return ""


def outcome_flags(value: str, allowed: List[str]) -> Dict[str, int]:
    return {k: int(value == k) for k in allowed}


# ============================================================
# 8.  MAIN PARSE FUNCTION
# ============================================================

def parse_pdf(path: Path) -> dict:
    raw  = read_pdf_text(path)
    txt  = normalize(raw)

    hdr       = extract_header_fields(txt)
    alj_cases = extract_alj_case_nos(txt)
    ab_no     = hdr["appeal_board_no"]

    who_appealed = extract_who_appealed(txt)
    issue_type   = extract_issue_type(txt)

    block, decision_mode = last_decision_block(txt)

    # Remand via ORDERED paragraphs — no merits decision, no benefits sentence
    procedural_remand = int(decision_mode == "ordered_remand")

    paragraphs = reconstruct_paragraphs(block)

    # Benefits from full block (last sentence wins)
    benefits, evidence_benefits = extract_benefits_outcome(block)

    alj_outcome     = ""
    initial_outcome = ""
    evidence_alj    = ""
    evidence_initial = ""

    relevant_paragraphs: List[str] = []
    ignored_paragraphs: List[str]  = []

    for para in paragraphs:
        applies = paragraph_applies_to_case(para, ab_no)
        if not applies:
            ignored_paragraphs.append(para)
            continue
        relevant_paragraphs.append(para)

        # ALJ outcome
        if alj_outcome == "" and re.search(r"\bAdministrative\s+Law\s+Judge\b", para, re.I):
            bucket = first_action_bucket(para)
            if bucket in {"upheld", "modified", "overruled", "remanded"}:
                alj_outcome   = bucket
                evidence_alj  = para

        # Initial determination outcome
        if initial_outcome == "" and re.search(r"\binitial\s+determination(?:s)?\b", para, re.I):
            bucket = action_bucket_for_initial(para)
            if bucket in {"upheld", "modified", "overruled"}:
                initial_outcome   = bucket
                evidence_initial  = para

    # Override: ORDERED-remand cases → force remand outcome regardless of text
    if procedural_remand:
        if alj_outcome == "":
            alj_outcome = "remanded"
        if initial_outcome == "":
            initial_outcome = "remanded"

    alj_flags  = outcome_flags(alj_outcome,     ["upheld", "modified", "overruled", "remanded"])
    init_flags = outcome_flags(initial_outcome,  ["upheld", "modified", "overruled"])

    return {
        # Header
        **hdr,
        "alj_case_nos":   alj_cases,
        "current_case":   ab_no,

        # New fields
        "who_appealed":      who_appealed,
        "issue_type":        issue_type,
        "decision_mode":     decision_mode,
        "procedural_remand": procedural_remand,

        # Benefits
        "benefits_outcome": benefits,
        "benefits_allowed": int(benefits == "allowed"),
        "benefits_denied":  int(benefits == "denied"),

        # ALJ
        "alj_outcome":   alj_outcome,
        "alj_upheld":    alj_flags["upheld"],
        "alj_modified":  alj_flags["modified"],
        "alj_overruled": alj_flags["overruled"],
        "alj_remanded":  alj_flags["remanded"],

        # Initial determination
        "initial_outcome":   initial_outcome,
        "initial_upheld":    init_flags["upheld"],
        "initial_modified":  init_flags["modified"],
        "initial_overruled": init_flags["overruled"],

        # Evidence / debug
        "evidence_benefits_paragraph":  evidence_benefits,
        "evidence_alj_paragraph":       evidence_alj,
        "evidence_initial_paragraph":   evidence_initial,

        # Counts / previews
        "decision_paragraph_count":              len(paragraphs),
        "relevant_paragraph_count":              len(relevant_paragraphs),
        "ignored_case_specific_paragraph_count": len(ignored_paragraphs),
        "relevant_paragraphs_preview":           " || ".join(relevant_paragraphs[:5])[:1200],
        "ignored_case_specific_preview":         " || ".join(ignored_paragraphs[:5])[:1200],
        "decision_block_preview":                " ".join(block.split())[:700],
        "text_chars":                            len(txt),
    }


# ============================================================
# 9.  ENTRY POINT
# ============================================================

def main() -> None:
    in_path = Path(sys.argv[1]) if len(sys.argv) > 1 else Path.cwd()
    if not in_path.exists():
        raise SystemExit(f"Not found: {in_path}")

    if in_path.is_file() and in_path.suffix.lower() == ".pdf":
        base_dir = in_path.parent
        pdfs = [in_path]
    else:
        base_dir = in_path
        pdfs = sorted(base_dir.rglob("*.pdf"))

    if not pdfs:
        raise SystemExit(f"No PDFs found under: {base_dir}")

    rows: List[dict] = []
    quality: List[dict] = []

    for p in pdfs:
        try:
            row = parse_pdf(p)
            rows.append(row)
            quality.append({
                "file":                  p.name,
                "appeal_board_no":       row["appeal_board_no"],
                "current_case":          row["current_case"],
                "decision_mode":         row["decision_mode"],
                "who_appealed":          row["who_appealed"],
                "issue_type":            row["issue_type"],
                "procedural_remand":     row["procedural_remand"],
                "missing_ab_no":         int(row["appeal_board_no"] == ""),
                "missing_mailed":        int(row["mailed_and_filed_date"] == ""),
                "missing_present":       int(row["board_member"] == ""),
                "benefits_outcome":      row["benefits_outcome"],
                "alj_outcome":           row["alj_outcome"],
                "initial_outcome":       row["initial_outcome"],
                "decision_paragraph_count":   row["decision_paragraph_count"],
                "relevant_paragraph_count":   row["relevant_paragraph_count"],
                "ignored_count":              row["ignored_case_specific_paragraph_count"],
                "text_chars":            row["text_chars"],
                "error": "",
            })
        except Exception as e:
            import traceback
            quality.append({
                "file": p.name,
                "appeal_board_no": "",
                "current_case": "",
                "error": traceback.format_exc()[-400:],
            })

    df = pd.DataFrame(rows)
    if not df.empty and "appeal_board_no" in df.columns:
        df["_sort"] = pd.to_numeric(
            df["appeal_board_no"].str.extract(r"(\d+)", expand=False),
            errors="coerce",
        )
        df = df.sort_values("_sort", na_position="last").drop(columns=["_sort"])

    out_csv     = base_dir / "uiab_outcomes_v2.csv"
    out_quality = base_dir / "uiab_outcomes_quality_v2.csv"

    df.to_csv(out_csv, index=False, encoding="utf-8-sig")
    pd.DataFrame(quality).to_csv(out_quality, index=False, encoding="utf-8-sig")

    print(f"Wrote {len(rows)} rows → {out_csv}")
    print(f"Wrote quality log   → {out_quality}")


if __name__ == "__main__":
    main()
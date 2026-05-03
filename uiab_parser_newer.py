"""
NYUIAB Appeal Decision Parser  —  v3
=====================================
Run:
    # Full run
    python uiab_parser_v3.py /path/to/folder/

    # Single file
    python uiab_parser_v3.py case.pdf

    # Test set — first N files
    python uiab_parser_v3.py /path/to/folder/ --test 50

    # Test set — random sample
    python uiab_parser_v3.py /path/to/folder/ --sample 100

    # Disable LLM (regex only, fast)
    python uiab_parser_v3.py /path/to/folder/ --no-llm

    # Tune workers (default 4)
    python uiab_parser_v3.py /path/to/folder/ --workers 6

Outputs:
    uiab_outcomes_v3.csv
    uiab_outcomes_quality_v3.csv
"""

from __future__ import annotations

import argparse
import json
import random
import re
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import pandas as pd
import requests


# ============================================================
# 1.  TEXT EXTRACTION
# ============================================================

def read_pdf_text(path: Path) -> str:
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

RE_MAILED   = re.compile(r"^\s*Mailed\s+and\s+Filed:\s*(.+?)\s*$", re.I | re.M)
RE_AB_NO    = re.compile(r"Appeal\s+Board\s+No\.\s*([0-9]{5,6}(?:\s*[A-Z])?)\s*$", re.I | re.M)
RE_PRESENT  = re.compile(r"^\s*PRESENT:\s*(.+?)\s*$", re.I | re.M)
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

RE_COMMISSIONER_APPEALS = re.compile(r"\bthe\s+commissioner\s+of\s+labor\s+appeals\b", re.I)
RE_EMPLOYER_APPEALS     = re.compile(
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
RE_BOARD_OWN_MOTION = re.compile(
    r"\bAppeal\s+Board,\s+on\s+its\s+motion\b|\bBoard,\s+on\s+its\s+motion\b",
    re.I,
)


def extract_who_appealed(text: str) -> str:
    intro = text[:3000]
    if RE_COMMISSIONER_APPEALS.search(intro): return "commissioner"
    if RE_EMPLOYER_APPEALS.search(intro):     return "employer"
    if RE_CLAIMANT_APPEALS.search(intro):     return "claimant"
    if RE_BOARD_OWN_MOTION.search(intro):     return "claimant"
    return "unknown"


# ============================================================
# 4.  ISSUE TYPE  (regex — returns ALL matches)
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

VALID_LABELS = {label for label, _ in ISSUE_PATTERNS} | {"other"}


def regex_issue_types(text: str) -> List[str]:
    intro = text[:4000]
    found = [label for label, pat in ISSUE_PATTERNS if pat.search(intro)]
    return found if found else ["other"]


# ============================================================
# 5.  ISSUE TYPE  (LLM)
# ============================================================

PROMPT_TEMPLATE = """\
You are classifying New York unemployment insurance appeal decisions.

A case may involve MORE THAN ONE issue. Return ALL that apply from this list:
- voluntary_quit         (claimant left job without good cause)
- misconduct             (claimant fired for misconduct)
- availability           (claimant not available for work)
- capability             (claimant not physically/mentally capable of work)
- misrepresentation      (claimant made false statements to collect benefits)
- overpayment            (claimant received benefits they weren't entitled to)
- employer_contributions (employer dispute over UI tax contributions)
- other                  (only if truly none of the above apply)

Reply with ONLY a JSON array of labels, e.g.:
["misconduct"]
["voluntary_quit", "overpayment"]

No explanation, no extra text, just the JSON array.

DOCUMENT:
{intro}"""


def parse_llm_labels(raw: str) -> Optional[List[str]]:
    raw = raw.strip()
    raw = re.sub(r"^```(?:json)?\s*", "", raw, flags=re.I)
    raw = re.sub(r"\s*```$", "", raw).strip()

    try:
        parsed = json.loads(raw)
        if isinstance(parsed, list):
            labels = [str(x).strip().lower().replace(" ", "_") for x in parsed]
            valid  = [l for l in labels if l in VALID_LABELS]
            return valid if valid else ["other"]
        if isinstance(parsed, str):
            raw = parsed
    except json.JSONDecodeError:
        pass

    # Comma-separated plain text fallback
    candidates = [x.strip().lower().replace(" ", "_") for x in raw.split(",")]
    valid = [c for c in candidates if c in VALID_LABELS]
    if valid:
        return valid

    cleaned = raw.lower().replace(" ", "_").strip(".,;:\"'")
    if cleaned in VALID_LABELS:
        return [cleaned]

    return None   # unparseable


def llm_issue_types(
    text: str,
    model: str = "mistral",
    timeout: int = 30,
) -> List[str]:
    prompt = PROMPT_TEMPLATE.format(intro=text[:4000])
    try:
        r = requests.post(
            "http://localhost:11434/api/generate",
            json={"model": model, "prompt": prompt, "stream": False},
            timeout=timeout,
        )
        r.raise_for_status()
        labels = parse_llm_labels(r.json()["response"].strip())
        if labels is None:
            print(f"[WARN] Unparseable LLM response, using regex fallback")
            return regex_issue_types(text)
        return labels
    except requests.exceptions.ConnectionError:
        print("[WARN] Ollama not running — using regex")
        return regex_issue_types(text)
    except Exception as e:
        print(f"[WARN] LLM error ({e}) — using regex")
        return regex_issue_types(text)


def build_issue_fields(
    text: str,
    use_llm: bool = True,
    model: str = "mistral",
) -> dict:
    """
    Returns flat dict of issue columns for both regex and LLM.
    Binary flag columns use the LLM labels as primary source.
    """
    regex_labels = set(regex_issue_types(text))
    llm_labels   = set(llm_issue_types(text, model=model)) if use_llm else regex_labels

    in_both       = sorted(regex_labels & llm_labels)
    only_in_llm   = sorted(llm_labels   - regex_labels)
    only_in_regex = sorted(regex_labels - llm_labels)

    primary = llm_labels if use_llm else regex_labels

    flags = {
        f"issue_{label}": int(label in primary)
        for label in sorted(VALID_LABELS)
    }

    return {
        "issue_types_llm":      "|".join(sorted(llm_labels))   if use_llm else "",
        "issue_types_regex":    "|".join(sorted(regex_labels)),
        "issue_types_in_both":  "|".join(in_both)              if use_llm else "",
        "issue_only_in_llm":    "|".join(only_in_llm)          if use_llm else "",
        "issue_only_in_regex":  "|".join(only_in_regex)        if use_llm else "",
        "issue_count_llm":      len(llm_labels)                if use_llm else 0,
        "issue_count_regex":    len(regex_labels),
        **flags,
    }


# ============================================================
# 6.  DECISION BLOCK
# ============================================================

RE_DECISION_ANCHOR = re.compile(r"\bDECISION:", re.I)
RE_ORDERED_REMAND  = re.compile(r"\bORDERED\b.*?\bremanded\b",  re.I | re.S)
RE_ORDERED_RESCIND = re.compile(r"\bORDERED\b.*?\brescinded\b", re.I | re.S)


def last_decision_block(text: str) -> Tuple[str, str]:
    last = None
    for m in RE_DECISION_ANCHOR.finditer(text):
        last = m.start()

    if last is not None:
        block = text[last:].strip()
        if RE_ORDERED_REMAND.search(block) or RE_ORDERED_RESCIND.search(block):
            return block, "ordered_remand"
        return block, "standard"

    if RE_ORDERED_REMAND.search(text) or RE_ORDERED_RESCIND.search(text):
        m = re.search(r"\bORDERED\b", text, re.I)
        if m:
            return text[m.start():].strip(), "ordered_remand"

    lines = text.splitlines()
    return "\n".join(lines[-120:]).strip(), "standard"


# ============================================================
# 7.  PARAGRAPHS & OUTCOME EXTRACTION
# ============================================================

RE_AB_CASE_MENTION = re.compile(r"\bAppeal\s+Board\s+No(?:s)?\.?\s*([0-9A-Z,\sand\-]+)", re.I)
RE_ANY_AB_NUMBER   = re.compile(r"\b([0-9]{5,6}[A-Z]?)\b")


def reconstruct_paragraphs(block: str) -> List[str]:
    raw_paras = re.split(r"\n\s*\n+", block.strip())
    paras: List[str] = []
    for para in raw_paras:
        lines  = [ln.strip() for ln in para.splitlines() if ln.strip()]
        joined = re.sub(r"\s+", " ", " ".join(lines)).strip()
        if joined:
            paras.append(joined)
    if len(paras) == 1:
        text = paras[0]
        text = re.sub(
            r"(?<=[\.\?!])\s+(?=(The\s+claimant|In\s+Appeal\s+Board\s+No|The\s+Commissioner|DECISION:|ORDERED))",
            "\n", text, flags=re.I,
        )
        paras = [p.strip() for p in text.split("\n") if p.strip()]
    return paras


def paragraph_applies_to_case(paragraph: str, current_case: str) -> bool:
    nums: Set[str] = set()
    for m in RE_AB_CASE_MENTION.finditer(paragraph):
        nums.update(RE_ANY_AB_NUMBER.findall(m.group(1)))
    return (not nums) or (current_case in nums)


ACTION_MAP = {
    "affirmed":            "upheld",
    "sustained":           "upheld",
    "continued in effect": "upheld",
    "modified":            "modified",
    "amended":             "modified",
    "reversed":            "overruled",
    "rescinded":           "overruled",
    "overruled":           "overruled",
    "vacated":             "overruled",
    "dismissed":           "overruled",
    "remanded":            "remanded",
    "referred":            "remanded",
}
ACTION_PATTERNS = sorted(ACTION_MAP.keys(), key=len, reverse=True)
ACTION_REGEX    = re.compile(
    r"\b(" + "|".join(re.escape(x) for x in ACTION_PATTERNS) + r")\b", re.I
)
RE_INITIAL_ACTION = re.compile(
    r"\b(affirmed|sustained|modified|amended|reversed|rescinded|overruled|vacated|dismissed)\b",
    re.I,
)
RE_BEN_ALLOWED = re.compile(r"\bThe\s+claimant\s+is\s+allowed\s+benefits\b", re.I)
RE_BEN_DENIED  = re.compile(r"\bThe\s+claimant\s+is\s+denied\s+benefits\b",  re.I)


def extract_benefits_outcome(block: str) -> Tuple[str, str]:
    text = re.sub(r"\s+", " ", block).strip()
    hits = []
    for m in RE_BEN_ALLOWED.finditer(text):
        hits.append((m.start(), "allowed", m.group(0).strip()))
    for m in RE_BEN_DENIED.finditer(text):
        hits.append((m.start(), "denied", m.group(0).strip()))
    if not hits:
        return "", ""
    _, outcome, evidence = max(hits, key=lambda x: x[0])
    return outcome, evidence


def split_into_sentences(text: str) -> List[str]:
    text = re.sub(r"\s+", " ", text).strip()
    return [s.strip() for s in re.split(r"(?<=\.)\s+", text) if s.strip()]


def action_bucket_for_initial(paragraph: str) -> str:
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
# 8.  PARSE ONE PDF
# ============================================================

def parse_pdf(path: Path, use_llm: bool = True, model: str = "mistral") -> dict:
    raw = read_pdf_text(path)
    txt = normalize(raw)

    hdr       = extract_header_fields(txt)
    alj_cases = extract_alj_case_nos(txt)
    ab_no     = hdr["appeal_board_no"]

    who_appealed = extract_who_appealed(txt)
    issue_fields = build_issue_fields(txt, use_llm=use_llm, model=model)

    block, decision_mode = last_decision_block(txt)
    procedural_remand    = int(decision_mode == "ordered_remand")
    paragraphs           = reconstruct_paragraphs(block)
    benefits, evidence_benefits = extract_benefits_outcome(block)

    alj_outcome = initial_outcome = evidence_alj = evidence_initial = ""
    relevant_paragraphs: List[str] = []
    ignored_paragraphs:  List[str] = []

    for para in paragraphs:
        if not paragraph_applies_to_case(para, ab_no):
            ignored_paragraphs.append(para)
            continue
        relevant_paragraphs.append(para)

        if alj_outcome == "" and re.search(r"\bAdministrative\s+Law\s+Judge\b", para, re.I):
            bucket = first_action_bucket(para)
            if bucket in {"upheld", "modified", "overruled", "remanded"}:
                alj_outcome, evidence_alj = bucket, para

        if initial_outcome == "" and re.search(r"\binitial\s+determination(?:s)?\b", para, re.I):
            bucket = action_bucket_for_initial(para)
            if bucket in {"upheld", "modified", "overruled"}:
                initial_outcome, evidence_initial = bucket, para

    if procedural_remand:
        if alj_outcome    == "": alj_outcome    = "remanded"
        if initial_outcome == "": initial_outcome = "remanded"

    alj_flags  = outcome_flags(alj_outcome,    ["upheld", "modified", "overruled", "remanded"])
    init_flags = outcome_flags(initial_outcome, ["upheld", "modified", "overruled"])

    return {
        **hdr,
        "alj_case_nos":   alj_cases,
        "current_case":   ab_no,
        "who_appealed":   who_appealed,
        "decision_mode":  decision_mode,
        "procedural_remand": procedural_remand,
        **issue_fields,
        "benefits_outcome": benefits,
        "benefits_allowed": int(benefits == "allowed"),
        "benefits_denied":  int(benefits == "denied"),
        "alj_outcome":   alj_outcome,
        "alj_upheld":    alj_flags["upheld"],
        "alj_modified":  alj_flags["modified"],
        "alj_overruled": alj_flags["overruled"],
        "alj_remanded":  alj_flags["remanded"],
        "initial_outcome":   initial_outcome,
        "initial_upheld":    init_flags["upheld"],
        "initial_modified":  init_flags["modified"],
        "initial_overruled": init_flags["overruled"],
        "evidence_benefits_paragraph":  evidence_benefits,
        "evidence_alj_paragraph":       evidence_alj,
        "evidence_initial_paragraph":   evidence_initial,
        "decision_paragraph_count":              len(paragraphs),
        "relevant_paragraph_count":              len(relevant_paragraphs),
        "ignored_case_specific_paragraph_count": len(ignored_paragraphs),
        "relevant_paragraphs_preview":           " || ".join(relevant_paragraphs[:5])[:1200],
        "ignored_case_specific_preview":         " || ".join(ignored_paragraphs[:5])[:1200],
        "decision_block_preview":                " ".join(block.split())[:700],
        "text_chars":                            len(txt),
    }


# ============================================================
# 9.  BATCH RUNNER
# ============================================================

def process_batch(
    pdfs: List[Path],
    use_llm:     bool = True,
    model:       str  = "mistral",
    max_workers: int  = 4,
) -> Tuple[List[dict], List[dict]]:
    rows:    List[dict] = []
    quality: List[dict] = []
    total    = len(pdfs)
    results  = {}

    def process_one(path: Path) -> Tuple[str, dict]:
        return path.name, parse_pdf(path, use_llm=use_llm, model=model)

    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = {executor.submit(process_one, p): p for p in pdfs}
        done = 0
        for future in as_completed(futures):
            done += 1
            path = futures[future]
            try:
                _, row = future.result()
                rows.append(row)
                quality.append({
                    "file":              path.name,
                    "appeal_board_no":   row["appeal_board_no"],
                    "decision_mode":     row["decision_mode"],
                    "who_appealed":      row["who_appealed"],
                    "issue_types_llm":   row.get("issue_types_llm", ""),
                    "issue_types_regex": row["issue_types_regex"],
                    "procedural_remand": row["procedural_remand"],
                    "benefits_outcome":  row["benefits_outcome"],
                    "alj_outcome":       row["alj_outcome"],
                    "initial_outcome":   row["initial_outcome"],
                    "missing_ab_no":     int(row["appeal_board_no"] == ""),
                    "text_chars":        row["text_chars"],
                    "error": "",
                })
            except Exception:
                import traceback
                quality.append({
                    "file":  path.name,
                    "error": traceback.format_exc()[-400:],
                })

            if done % 50 == 0 or done == total:
                print(f"  [{done}/{total}] done")

    return rows, quality


# ============================================================
# 10.  ENTRY POINT
# ============================================================

def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("input",      type=Path, help="PDF file or folder")
    parser.add_argument("--test",     type=int,  default=0,     help="Use first N files only")
    parser.add_argument("--sample",   type=int,  default=0,     help="Use random N files")
    parser.add_argument("--workers",  type=int,  default=4,     help="Parallel workers (default 4)")
    parser.add_argument("--model",    type=str,  default="mistral", help="Ollama model name")
    parser.add_argument("--no-llm",   action="store_true",      help="Skip LLM, regex only")
    args = parser.parse_args()

    in_path = args.input
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

    # Test set selection
    if args.sample > 0:
        pdfs = random.sample(pdfs, min(args.sample, len(pdfs)))
        print(f"Random sample: {len(pdfs)} files")
    elif args.test > 0:
        pdfs = pdfs[:args.test]
        print(f"Test set: first {len(pdfs)} files")
    else:
        print(f"Full run: {len(pdfs)} files")

    use_llm = not args.no_llm
    suffix  = "_test" if (args.test or args.sample) else ""

    print(f"LLM: {'enabled (' + args.model + ')' if use_llm else 'disabled (regex only)'}")
    print(f"Workers: {args.workers}")
    print()

    rows, quality = process_batch(
        pdfs,
        use_llm=use_llm,
        model=args.model,
        max_workers=args.workers,
    )

    df = pd.DataFrame(rows)
    if not df.empty and "appeal_board_no" in df.columns:
        df["_sort"] = pd.to_numeric(
            df["appeal_board_no"].str.extract(r"(\d+)", expand=False),
            errors="coerce",
        )
        df = df.sort_values("_sort", na_position="last").drop(columns=["_sort"])

    out_csv     = base_dir / f"uiab_outcomes_v3{suffix}.csv"
    out_quality = base_dir / f"uiab_outcomes_quality_v3{suffix}.csv"

    df.to_csv(out_csv, index=False, encoding="utf-8-sig")
    pd.DataFrame(quality).to_csv(out_quality, index=False, encoding="utf-8-sig")

    print(f"\nWrote {len(rows)} rows → {out_csv}")
    print(f"Wrote quality log   → {out_quality}")


if __name__ == "__main__":
    main()
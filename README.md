# UIAB Appeal Outcomes Analysis

A two-stage pipeline for extracting and analyzing New York Unemployment Insurance Appeal Board (UIAB) decisions.

## Overview

**Stage 1 — Parse** (`uiab_parser_newer.py`): Ingests a folder of UIAB decision PDFs and extracts structured fields via regex and an optional local LLM (Ollama/Mistral). Outputs two CSVs.

**Stage 2 — Analyze** (`NLP4.R`): Loads the parsed CSV and produces descriptive stats, charts, and statistical models examining judge-level variation in claimant outcomes.

> ⚠️ **Experimental:** The LLM-assisted issue classification (via Ollama) is experimental. Results may be inconsistent depending on the model used and document formatting. Always cross-check LLM-assigned labels against the `issue_types_regex` column in the quality log. Use `--no-llm` for fully deterministic output.

---

## Stage 1: PDF Parser

**Dependencies:** `pymupdf` (`fitz`), `pandas`, `requests`, and a running [Ollama](https://ollama.com) instance (for LLM mode).

**Usage:**
```
python uiab_parser_newer.py /path/to/pdfs/
python uiab_parser_newer.py case.pdf
python uiab_parser_newer.py /path/to/pdfs/ --test 50
python uiab_parser_newer.py /path/to/pdfs/ --sample 100
python uiab_parser_newer.py /path/to/pdfs/ --no-llm
python uiab_parser_newer.py /path/to/pdfs/ --workers 8
```

**Extracted fields per case:**

| Field | Description |
|---|---|
| `appeal_board_no` | Case identifier |
| `mailed_and_filed_date` | Decision date |
| `board_member` | Presiding judge(s) |
| `who_appealed` | claimant / employer / commissioner |
| `issue_type` | voluntary_quit, misconduct, availability, etc. |
| `benefits_outcome` | allowed / denied |
| `alj_outcome` | upheld / overruled / modified / remanded |
| `initial_outcome` | upheld / overruled / modified |
| `procedural_remand` | Binary flag |

**Outputs:**
- `uiab_outcomes_v3.csv` — one row per case
- `uiab_outcomes_quality_v3.csv` — parsing quality log with error traces

---

## Stage 2: R Analysis

**Dependencies:** `readr`, `dplyr`, `tidyr`, `ggplot2`, `lubridate`, `broom`, `forcats`, `scales`, `rlang`

Update the data path at the top of `NLP4.R` before running:
```r
path <- "path/to/uiab_outcomes_v2.csv"
```

**What it produces:**

- Outcome distributions by year, judge, appellant, and issue type
- Stacked bar charts of benefits / ALJ / initial outcomes
- Judge-level summary stats (allow rates, SE, overrule rates)
- ANOVA + Bonferroni-corrected pairwise tests for judge effects
- Logistic regression (3 models) with judge, year, appellant, issue type
- Forest plot of per-judge odds ratios
- Faceted judge × year trend plots
- Remand rate breakdowns
- Random validation sample (n=30, seed=123)

---

## Data Flow

```
PDF folder → uiab_parser_newer.py → uiab_outcomes_v3.csv → NLP4.R → plots + stats
```

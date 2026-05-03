import os, re, time, sys, requests
from urllib.parse import urljoin, urlparse

BASE = "https://uiappeals.ny.gov"
LISTING_TPL = BASE + "/taxonomy/term/76?page={page}"

# --- configure here ---
START_PAGE = 1000
END_PAGE   = 5402   # inclusive
OUT_DIR    = r"C:\Users\ngkan\Downloads\NLP Project"
SLEEP_BETWEEN_PAGES = 0.005
SLEEP_BETWEEN_DOWNLOADS = 0.005
TIMEOUT = 25
# ----------------------

os.makedirs(OUT_DIR, exist_ok=True)

UA = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                    "AppleWebKit/537.36 (KHTML, like Gecko) "
                    "Chrome/120 Safari/537.36"}

# Regex to find decision links in the listing HTML
RX_DECISION = re.compile(
    r'href=[\'"](?P<href>/decisions/appeal-board-no-\d+(?:-[0-9a-z]+)?)[\'"]',
    re.I
)

def filename_from_url(url: str) -> str:
    name = os.path.basename(urlparse(url).path)
    return name if name else "download.pdf"

def download_decision(decision_url: str):
    """Request the /decisions/... URL; if it serves/redirects to a PDF, save it."""
    try:
        r = requests.get(decision_url, headers=UA, timeout=TIMEOUT, allow_redirects=True)
        r.raise_for_status()
        ctype = r.headers.get("content-type", "").lower()

        # If it's a PDF (usually is), save with the final URL's basename
        if "pdf" in ctype or r.url.lower().endswith(".pdf"):
            fname = filename_from_url(r.url)
            out_path = os.path.join(OUT_DIR, fname)
            if os.path.exists(out_path) and os.path.getsize(out_path) > 0:
                print(f"  [skip] {fname} (exists)")
                return
            with open(out_path, "wb") as f:
                f.write(r.content)
            print(f"  [saved] {fname}")
        else:
            # Not a PDF — you can log and skip
            print(f"  [warn] non-PDF at {decision_url} (final: {r.url}, type: {ctype or 'unknown'})")
    except Exception as e:
        print(f"  [fail] {decision_url}: {e}")

def main():
    # Optional: allow start/end via CLI: python grab_uiab_decisions_simple.py 11 20
    global START_PAGE, END_PAGE
    if len(sys.argv) >= 2:
        START_PAGE = int(sys.argv[1])
    if len(sys.argv) >= 3:
        END_PAGE = int(sys.argv[2])

    total = 0
    for p in range(START_PAGE, END_PAGE + 1):
        url = LISTING_TPL.format(page=p)
        print(f"[PAGE] {p} -> {url}")
        try:
            html = requests.get(url, headers=UA, timeout=TIMEOUT).text
        except Exception as e:
            print(f"  [warn] failed to fetch page {p}: {e}")
            time.sleep(SLEEP_BETWEEN_PAGES)
            continue

        # Collect decision links from the HTML
        links = [urljoin(BASE, m.group("href")) for m in RX_DECISION.finditer(html)]
        links = sorted(set(links))
        print(f"  [info] found {len(links)} decision links")

        # Download each decision
        for durl in links:
            download_decision(durl)
            total += 1
            time.sleep(SLEEP_BETWEEN_DOWNLOADS)

        time.sleep(SLEEP_BETWEEN_PAGES)

    print(f"[DONE] attempted {total} decisions. Files in:\n  {OUT_DIR}")

if __name__ == "__main__":
    main()

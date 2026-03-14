#!/usr/bin/env bash
set -euo pipefail

# ── Configuration ──────────────────────────────────────────────────────
SRC_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TARGET_DIR="$HOME/Documents/mpst-artifact"

ANON_NAME="anonymous5738"
ANON_EMAIL="anonymous5738@users.noreply.github.com"

PII_PATTERNS=(
  "Kai Pischke"
  "kai\.pischke"
  "cs\.ox\.ac\.uk"
  "kai-pischke"
  "kaihke"
)

# Files where PII is expected (scrubbed in Phase 3) or part of this script
EXCLUDE_FROM_SCAN="scripts/publish-artifact.sh|package.yaml|mpst.cabal"

# ── Phase 1: PII scan ─────────────────────────────────────────────────
echo "==> Phase 1: Scanning tracked files for PII..."
cd "$SRC_DIR"

pii_found=0
for pattern in "${PII_PATTERNS[@]}"; do
  while IFS= read -r match; do
    [ -z "$match" ] && continue
    echo "  PII match: $match"
    pii_found=1
  done < <(git ls-files -z \
    | xargs -0 grep -inH "$pattern" 2>/dev/null \
    | grep -Ev "^(${EXCLUDE_FROM_SCAN}):" || true)
done

if [ "$pii_found" -eq 1 ]; then
  echo "ERROR: Unexpected PII found. Fix the above files before publishing."
  exit 1
fi
echo "  No unexpected PII found."

# ── Phase 2: Copy ─────────────────────────────────────────────────────
echo "==> Phase 2: Syncing to $TARGET_DIR..."
mkdir -p "$TARGET_DIR"

rsync -a --delete \
  --exclude='.git/' \
  --exclude='.stack-work/' \
  --exclude='dist-newstyle/' \
  --exclude='.claude/' \
  --exclude='docs/plans/' \
  --exclude='test/' \
  --exclude='cabal.project.freeze' \
  --exclude='*.png' \
  --exclude='scripts/' \
  "$SRC_DIR/" "$TARGET_DIR/"

echo "  Done."

# ── Phase 3: Scrub PII ────────────────────────────────────────────────
echo "==> Phase 3: Scrubbing PII from copied files..."

# package.yaml: replace author/maintainer lines
sed -i '' 's/^author: .*/author: Anonymous/' "$TARGET_DIR/package.yaml"
sed -i '' 's/^maintainer: .*/maintainer: Anonymous/' "$TARGET_DIR/package.yaml"

# package.yaml: remove the tests stanza (from "tests:" to end of file)
sed -i '' '/^tests:/,$d' "$TARGET_DIR/package.yaml"

# mpst.cabal: replace author/maintainer lines
sed -i '' 's/^author: .*/author: Anonymous/' "$TARGET_DIR/mpst.cabal"
sed -i '' 's/^maintainer: .*/maintainer: Anonymous/' "$TARGET_DIR/mpst.cabal"

# mpst.cabal: remove the test-suite stanza (from "test-suite" to end of file)
sed -i '' '/^test-suite /,$d' "$TARGET_DIR/mpst.cabal"

echo "  Done."

# ── Phase 4: Git init (first run only) ────────────────────────────────
if [ ! -d "$TARGET_DIR/.git" ]; then
  echo "==> Phase 4: Initialising git repo..."
  cd "$TARGET_DIR"
  git init -b main
  git config user.name "$ANON_NAME"
  git config user.email "$ANON_EMAIL"
  echo ""
  echo "  Repository initialised. Please add the remote manually:"
  echo "    cd $TARGET_DIR"
  echo "    git remote add origin <YOUR_ANONYMOUS_REPO_URL>"
  echo ""
  echo "  Then re-run this script to commit and push."
  exit 0
fi

# ── Phase 5: Commit & push ────────────────────────────────────────────
echo "==> Phase 5: Committing and pushing..."
cd "$TARGET_DIR"

git add -A
if git diff --cached --quiet; then
  echo "  No changes to commit."
  exit 0
fi

git commit -m "Update artifact $(date +%Y-%m-%d)"
git push origin main

echo "==> Done. Artifact published."

#!/usr/bin/env bash

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TM_FILE="$REPO_ROOT/editors/vscode/syntaxes/anvyx.tmLanguage.json"
SCM_FILE="$REPO_ROOT/editors/nvim/queries/highlights.scm"

LEXER=$(sed -n '/pub enum Keyword {/,/^    }/p' "$REPO_ROOT/crates/frontend/src/lexer.rs" \
  | grep -oE '"[a-z]+"' \
  | tr -d '"' \
  | sort -u)

CONTEXTUAL=$(printf '%s\n' \
  cast computed contract dyn embed escaping from init inline op rep rev shared slice step \
  | sort -u)

words_from_tm() {
  grep -oE '\\\\b[(]?[a-z|]+[)]?\\\\b' "$1" \
    | sed 's/\\\\b//g; s/(//g; s/)//g' \
    | tr '|' '\n' \
    | sort -u
}

contextual_from() {
  grep -Fx -f <(printf '%s\n' "$CONTEXTUAL") || true
}

lexer_words_from() {
  comm -23 <(printf '%s\n' "$1") <(printf '%s\n' "$CONTEXTUAL")
}

TM=$(words_from_tm "$TM_FILE" | grep -vxE 'self')
TM_CONTEXTUAL=$(printf '%s\n' "$TM" | contextual_from)
TM_LEXER=$(lexer_words_from "$TM")

SCM_GRAMMAR_HANDLED='int|float|bool|string|void|any|pub'
SCM=$(grep -oE '"[a-z]+"' "$SCM_FILE" \
  | tr -d '"' \
  | grep -vxE 'self' \
  | sort -u)
SCM_CONTEXTUAL=$(printf '%s\n' "$SCM" | contextual_from)
SCM_LEXER=$(lexer_words_from "$SCM")
LEXER_FOR_SCM=$(printf '%s\n' "$LEXER" | grep -vxE "$SCM_GRAMMAR_HANDLED")

ERRORS=0

check_diff() {
  local label="$1"
  local expected="$2"
  local actual="$3"
  local drift

  drift=$(diff <(printf '%s\n' "$expected") <(printf '%s\n' "$actual") || true)
  if [ -z "$drift" ]; then
    return
  fi

  echo "DRIFT: $label"
  echo "$drift"
  echo ""
  ERRORS=1
}

check_diff "lexer.rs <-> anvyx.tmLanguage.json lexer keywords" "$LEXER" "$TM_LEXER"
check_diff "contextual parser words <-> anvyx.tmLanguage.json" "$CONTEXTUAL" "$TM_CONTEXTUAL"
check_diff "lexer.rs <-> highlights.scm lexer keywords" "$LEXER_FOR_SCM" "$SCM_LEXER"
check_diff "contextual parser words <-> highlights.scm" "$CONTEXTUAL" "$SCM_CONTEXTUAL"

if [ "$ERRORS" -eq 0 ]; then
  COUNT=$(printf '%s\n' "$LEXER" | wc -l | tr -d ' ')
  CONTEXTUAL_COUNT=$(printf '%s\n' "$CONTEXTUAL" | wc -l | tr -d ' ')
  echo "No keyword drift detected ($COUNT lexer keywords, $CONTEXTUAL_COUNT contextual words in sync)."
fi
exit "$ERRORS"

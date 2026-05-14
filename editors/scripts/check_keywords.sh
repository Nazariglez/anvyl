#!/usr/bin/env bash

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"

LEXER=$(sed -n '/^fn map_ident_to_token/,/^}/p' "$REPO_ROOT/crates/frontend/src/lexer.rs" \
  | grep -oE '"[a-z]+" =>' \
  | grep -oE '"[a-z]+"' \
  | tr -d '"' \
  | sort -u)

TM=$(grep -oE '\\\\b\(?[a-z|]+\)?\\\\b' \
       "$REPO_ROOT/editors/vscode/syntaxes/anvyx.tmLanguage.json" \
  | sed 's/\\\\b//g; s/(//g; s/)//g' \
  | tr '|' '\n' \
  | grep -vxE 'self' \
  | sort -u)

SCM_GRAMMAR_HANDLED='int|float|bool|string|void|any|pub'

# ignore self, from, op, and slice here because they are only keywords in specific contexts.
SCM=$(grep -oE '"[a-z]+"' "$REPO_ROOT/editors/nvim/queries/highlights.scm" \
  | tr -d '"' \
  | grep -vxE 'self|from|op|slice' \
  | sort -u)

LEXER_FOR_SCM=$(echo "$LEXER" | grep -vxE "$SCM_GRAMMAR_HANDLED")

ERRORS=0

DIFF_TM=$(diff <(echo "$LEXER") <(echo "$TM") || true)
if [ -n "$DIFF_TM" ]; then
  echo "DRIFT: lexer.rs <-> anvyx.tmLanguage.json"
  echo "$DIFF_TM"
  echo ""
  ERRORS=1
fi

DIFF_SCM=$(diff <(echo "$LEXER_FOR_SCM") <(echo "$SCM") || true)
if [ -n "$DIFF_SCM" ]; then
  echo "DRIFT: lexer.rs <-> highlights.scm"
  echo "$DIFF_SCM"
  echo ""
  ERRORS=1
fi

if [ "$ERRORS" -eq 0 ]; then
  COUNT=$(echo "$LEXER" | wc -l | tr -d ' ')
  echo "No keyword drift detected ($COUNT keywords in sync)."
fi
exit "$ERRORS"

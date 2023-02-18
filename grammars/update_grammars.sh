#!/bin/bash
set -x

declare -A grammars
grammars[tree-sitter-commonlisp]="https://raw.githubusercontent.com/theHamsta/tree-sitter-commonlisp/master/src/grammar.json"

for name in "${!grammars[@]}"
do
  url="${grammars[$name]}"
  wget -O "${name}.json" "$url"
done

#!/bin/sh
# Runs every tests/*.in through Main.hs and compares with the matching .out
cd "$(dirname "$0")/.." || exit 1
status=0
for input in tests/*.in; do
  expected="${input%.in}.out"
  if runghc Main.hs < "$input" | diff -u "$expected" - > /dev/null; then
    echo "OK    $input"
  else
    echo "FAIL  $input"
    runghc Main.hs < "$input" | diff -u "$expected" -
    status=1
  fi
done
exit $status

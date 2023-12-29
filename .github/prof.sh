#!/bin/sh

PROF_FILE="$1"

if [ -f "$PROF_FILE" ]; then
	printf "# profile: elspsed time\n# ------------\n"
	awk '/^[0-9]+\.[0-9]+/{print $1, $14}' "$PROF_FILE" \
		| sort -k1n,1nr
	printf "# profile: gc count\n# ------------\n"
	awk '/^[0-9]+\.[0-9]+/{print $2, $14}' "$PROF_FILE" \
		| sort -k1n,1nr
fi

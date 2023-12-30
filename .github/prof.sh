#!/bin/sh

PROF_ROOT="$(cd -- $(dirname -- $0) && pwd)"
PROF_FILE="$1"

if [ -f "$PROF_FILE" ]; then
	printf "# profile: elspsed time\n# ------------\n"
	awk -f ${PROF_ROOT}/elapsed.awk "$PROF_FILE" \
			| sort -k1n,1nr -k2n,2nr -k3n,3nr
	printf "# profile: elapsed real time\n# ------------\n"
	awk -f ${PROF_ROOT}/elapsed.awk "$PROF_FILE" \
		| sort -k4n,4nr -k1n,1nr -k2n,2nr
	printf "# profile: gc count\n# ------------\n"
	awk -f ${PROF_ROOT}/elapsed.awk "$PROF_FILE" \
		| sort -k2n,2nr -k1n,1nr -k3n,3nr
	printf "# profile: gc elapsed time\n# ------------\n"
	awk -f ${PROF_ROOT}/elapsed.awk "$PROF_FILE" \
		| sort -k3n,3nr -k1n,1nr -k2n,2nr
	printf "# profile: serial\n# ------------\n"
	awk -f ${PROF_ROOT}/elapsed.awk "$PROF_FILE" \
		| sort -k5n -k1n,1nr -k2n,2nr
fi

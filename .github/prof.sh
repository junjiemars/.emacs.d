#!/bin/sh

PROF_ROOT="$(cd -- $(dirname -- $0) && pwd)"
PROF_FILE="$1"

if [ -f "$PROF_FILE" ]; then
  printf "# profile: elspsed time\n# ------------\n"
  awk -f ${PROF_ROOT}/elapsed.awk "$PROF_FILE" \
      | sort -k1nr -k2nr -k3nr
  printf "# profile: elapsed real time\n# ------------\n"
  awk -f ${PROF_ROOT}/elapsed.awk "$PROF_FILE" \
    | sort -k4nr -k1nr -k2nr
  printf "# profile: gc count\n# ------------\n"
  awk -f ${PROF_ROOT}/elapsed.awk "$PROF_FILE" \
    | sort -k2nr -k1nr -k3nr
  printf "# profile: gc elapsed time\n# ------------\n"
  awk -f ${PROF_ROOT}/elapsed.awk "$PROF_FILE" \
    | sort -k3nr -k1nr -k2nr
  printf "# profile: resource\n# ------------\n"
  awk -f ${PROF_ROOT}/res.awk "$PROF_FILE"
  printf "# profile: summary\n# ------------\n"
  awk -f ${PROF_ROOT}/sum.awk "$PROF_FILE"
fi

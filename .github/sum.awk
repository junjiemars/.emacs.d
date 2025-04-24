#!/usr/bin/awk -f
#
# output: elapsed time, gc count, and gc elpased.
#

/^[0-9]+\.[0-9]+/ {
    elapsed_time += $1
    gc_count += $2
    gc_elapsed += $3
}

END {
    printf "%10.6f %4d %10.6f\n", elapsed_time, gc_count, gc_elapsed
}

# eof

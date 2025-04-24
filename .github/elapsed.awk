#!/usr/bin/awk -f
#
# format output:
# 1. elapsed time
# 2. gc count
# 3. gc elapsed time
# 4. elapsed real time
# 5. sn
# 6. filename
#

function basename(path) {
    sub(/.*\//, "", path)
    return path
}

/^[0-9]+\.[0-9]+/ {
    filename = basename($14)
    elapsed_real = $1 - $3
    printf "%10.6f %4d %10.6f %10.6f %3d %s\n",
        $1, $2, $3, elapsed_real, NR, filename
}

# eof

#!/usr/bin/awk -f
#
# format output: gc count, all resource fields
#

function basename(path) {
		sub(".*/", "", path)
		return path
}

/^[0-9]+\.[0-9]+/ {
		filename = basename($14);
		printf "%02d %2d %d %d %d %d %d %d %d %d %s\n",
				NR, $2, $5, $6, $7, $8, $9, $10, $11, $12, filename
}

# eof

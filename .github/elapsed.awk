#!/usr/bin/awk -f
#
# format output: elapsed time, gc count, gc elapsed time, sn, filename
#

function basename(path) {
		sub(".*/", "", path)
		return path
}

/^[0-9]+\.[0-9]+/ {
		filename = basename($14);
		elapsed_real = $1 - $3;
		printf "%10.6f %4d %10.6f %10.6f %03d %s\n",
				$1, $2, $3, elapsed_real, NR, filename
}

# eof

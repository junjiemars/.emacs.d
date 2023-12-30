#!/usr/bin/awk -f
#
# output: booting elapsed time
#

/^[0-9]+\.[0-9]+/ && /boot\.el/ {
		printf "%10.6f %4d %10.6f boot\n", $1, $2, $3
}

/^[0-9]+\.[0-9]+/ && !/boot\.el/ {
		elapsed_time += $1;
		gc_count += $2;
		gc_elapsed += $3;
}

END {
		printf "%10.6f %4d %10.6f booted\n", elapsed_time, gc_count, gc_elapsed
}

# eof

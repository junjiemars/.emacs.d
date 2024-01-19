#!/usr/bin/awk -f
#
# format output: gc count, all resource fields
#

function basename(path) {
		sub(".*/", "", path)
		return path
}

/^[0-9]+\.[0-9]+/ {
		pure_bytes_used = $5 - (pure_bytes_used ? pure_bytes_used:$5);
		cons_cells_consed = $6 - (cons_cells_consed ? cons_cells_consed:$6);
		floats_consed = $7 - (floats_consed ? floats_consed:$7);
		vector_cells_consed = $8 - (vector_cells_consed ? vector_cells_consed:$8);
		symbols_consed = $9 - (symbols_consed ? symbols_consed:$9);
		string_chars_consed = $10 - (string_chars_consed ? string_chars_consed:$10);
		intervals_consed = $11 - (intervals_consed ? intervals_consed:$11);
		strings_consed = $12 - (strings_consed ? strings_consed:$12);
		filename = basename($14);

		printf "%3d %2d %2d %8d %4d %8d %4d %8d %2d %6d %s\n",
				NR, $2,
				pure_bytes_used,
				cons_cells_consed,
				floats_consed,
				vector_cells_consed,
				symbols_consed,
				string_chars_consed,
				intervals_consed,
				strings_consed,
				filename

		pure_bytes_used = $5;
		cons_cells_consed = $6;
		floats_consed = $7;
		vector_cells_consed = $8;
		symbols_consed = $9;
		string_chars_consed = $10;
		intervals_consed = $11;
		strings_consed = $12;
}

# eof

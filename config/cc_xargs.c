/* make xargs `(cc*-make-xargs-bin)' on Windows */
/* required by `(emacs-home* "config/cc.el")' */
/* from `(nore-emacs)' */
#include <stdio.h>
int
main (int argc, char **argv)
{
  int ch;
  while (EOF != (ch = fgetc (stdin)))
    {
      fputc (ch, stdout);
    }
  if (ferror (stdin))
    {
      perror ("read failed from stdin");
      return 1;
    }
  return 0;
}

static char junk[] = "\n@(#) LIBU77 VERSION 19980709\n";

char __G77_LIBU77_VERSION__[] = "3.2.1 20021009 (prerelease)";

#include <stdio.h>

void
g77__uvers__ ()
{
  fprintf (stderr, "__G77_LIBU77_VERSION__: %s", __G77_LIBU77_VERSION__);
  fputs (junk, stderr);
}

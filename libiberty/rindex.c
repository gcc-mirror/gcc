/* Stub implementation of (obsolete) rindex(). */

extern char *strrchr ();

char *
rindex (s, c)
  char *s;
  int c;
{
  return strrchr (s, c);
}

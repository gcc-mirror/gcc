/* Stub implementation of (obsolete) index(). */

extern char * strchr();

char *
index (s, c)
  char *s;
  int c;
{
  return strchr (s, c);
}

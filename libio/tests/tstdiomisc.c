#ifndef STDIO_H
#define STDIO_H <iostdio.h>
#endif
#include STDIO_H

void
t1 ()
{
  int n = -1;
  sscanf ("abc  ", "abc %n", &n);
  printf ("t1: count=%d\n", n);
}

void
t2 ()
{
  int n;
  long N;
  int retval;
#define SCAN(INPUT, FORMAT, VAR) \
  VAR = -1; \
  retval = sscanf (INPUT, FORMAT,  &VAR); \
  printf ("sscanf (\"%s\", \"%s\", &x) => %d, x = %ld\n", \
	  INPUT, FORMAT, retval, (long)VAR);

  SCAN ("12345", "%ld", N);
  SCAN ("12345", "%llllld", N);
  SCAN ("12345", "%LLLLLd", N);
  SCAN ("test ", "%*s%n",  n);
  SCAN ("test ",   "%2*s%n",  n);
  SCAN ("12 ",   "%l2d",  n);
  SCAN ("12 ",   "%2ld",  N);
}

int
main ()
{
  t1 ();
  t2 ();

  fflush (stdout);
  return 0;
}

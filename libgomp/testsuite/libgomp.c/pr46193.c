/* { dg-do run } */
/* { dg-additional-options "-ftree-parallelize-loops=2" } */

extern void abort (void);

char *
foo (int count, char **list)
{
  char *minaddr = list[0];
  int i;

  for (i = 0; i < count; i++)
    {
      char *addr = list[i];
      if (addr < minaddr)
	minaddr = addr;
    }

  return minaddr;
}

char *
foo2 (int count, char **list)
{
  char *maxaddr = list[0];
  int i;

  for (i = 0; i < count; i++)
    {
      char *addr = list[i];
      if (addr > maxaddr)
	maxaddr = addr;
    }

  return maxaddr;
}

#define N 5

static void
init (char **list)
{
  int i;
  for (i = 0; i < N; ++i)
    list[i] = (char *)&list[i];
}

int
main (void)
{
  char *list[N];
  char * res;

  init (list);

  res = foo (N, list);

  if (res != (char *)&list[0])
    abort ();

  res = foo2 (N, list);

  if (res != (char *)&list[N-1])
    abort ();

  return 0;
}

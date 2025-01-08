/* { dg-do run } */
/* { dg-additional-options -O3 } */

/* Test late resolution of metadirectives with dynamic selectors
   in "declare simd" functions.  All the variants do the same thing;
   the purpose of this test is to ensure that the "condition" predicates
   are all called, and in the correct order.  */

static int pcount = 0;

static int __attribute__ ((noinline))
ptrue (int n)
{
  pcount++;
  if (pcount != n)
    __builtin_abort ();
  return 1;
}

static int __attribute__ ((noinline))
pfalse (int n)
{
  pcount++;
  if (pcount != n)
    __builtin_abort ();
  return 0;
}

#define N 256

#pragma omp declare simd
void
f (int a[])
{
  int i;

#pragma omp metadirective                                               \
  when (construct={simd}:                                               \
	nothing)							\
  when (user={condition(score (100): pfalse (1))}:			\
	nothing)							\
  when (user={condition(score (90): pfalse (2))}:			\
	nothing)							\
  when (user={condition(score (70): (ptrue (5) && pfalse (6)))}:	\
	nothing)							\
  when (user={condition(score (80): (pfalse (3) || pfalse (4)))}:	\
	nothing)							\
  when (user={condition(score (60):					\
			(ptrue (7) ? pfalse (8) : ptrue (8)))}:		\
	nothing)							\
  otherwise (nothing)
  for (i = 0; i < N; i++)
    a[i] += i;
}

int a[N];

int
main (void)
{
  f (a);
  for (int i = 0; i < N; i++)
    if (a[i] != i)
      return 1;
  return 0;
}

#include <stdlib.h>

/* Test mapping chained indirect struct accesses, mixed in different ways.  */

typedef struct {
  int *a;
  int b;
  int *c;
} str1;

typedef struct {
  int d;
  int *e;
  str1 *f;
} str2;

typedef struct {
  int g;
  int h;
  str2 *s2;
} str3;

typedef struct {
  str3 m;
  str3 n;
} str4;

void
zero_arrays (str4 *s, int N)
{
  for (int i = 0; i < N; i++)
    {
      s->m.s2->e[i] = 0;
      s->m.s2->f->a[i] = 0;
      s->m.s2->f->c[i] = 0;
      s->n.s2->e[i] = 0;
      s->n.s2->f->a[i] = 0;
      s->n.s2->f->c[i] = 0;
    }
}

void
alloc_s2 (str2 **s, int N)
{
  (*s) = (str2 *) malloc (sizeof (str2));
  (*s)->f = (str1 *) malloc (sizeof (str1));
  (*s)->e = (int *) malloc (sizeof (int) * N);
  (*s)->f->a = (int *) malloc (sizeof (int) * N);
  (*s)->f->c = (int *) malloc (sizeof (int) * N);
}

int main (int argc, char* argv[])
{
  const int N = 1024;
  str4 p, *q;
  int i;

  alloc_s2 (&p.m.s2, N);
  alloc_s2 (&p.n.s2, N);
  q = (str4 *) malloc (sizeof (str4));
  alloc_s2 (&q->m.s2, N);
  alloc_s2 (&q->n.s2, N);

  zero_arrays (&p, N);

  for (int i = 0; i < 99; i++)
    {
#pragma acc enter data copyin(p.m.s2[:1])
#pragma acc parallel loop copy(p.m.s2->e[:N])
      for (int j = 0; j < N; j++)
	p.m.s2->e[j]++;
#pragma acc exit data delete(p.m.s2[:1])
    }

  for (i = 0; i < N; i++)
    if (p.m.s2->e[i] != 99)
      abort ();

  zero_arrays (&p, N);

  for (int i = 0; i < 99; i++)
    {
#pragma acc enter data copyin(p.m.s2[:1])
#pragma acc enter data copyin(p.m.s2->f[:1])
#pragma acc parallel loop copy(p.m.s2->f->a[:N]) copy(p.m.s2->f->c[:N])
	for (int j = 0; j < N; j++)
	  {
	    p.m.s2->f->a[j]++;
	    p.m.s2->f->c[j]++;
	  }
#pragma acc exit data delete(p.m.s2->f[:1])
#pragma acc exit data delete(p.m.s2[:1])
    }

  for (i = 0; i < N; i++)
    if (p.m.s2->f->a[i] != 99 || p.m.s2->f->c[i] != 99)
      abort ();

  zero_arrays (&p, N);

  for (int i = 0; i < 99; i++)
    {
#pragma acc enter data copyin(p.m.s2[:1]) copyin(p.n.s2[:1])
#pragma acc enter data copyin(p.m.s2->f[:1]) copyin(p.n.s2->f[:1])
#pragma acc parallel loop copy(p.m.s2->f->a[:N]) copy(p.m.s2->f->c[:N]) \
			  copy(p.n.s2->f->a[:N]) copy(p.n.s2->f->c[:N])
	for (int j = 0; j < N; j++)
	  {
	    p.m.s2->f->a[j]++;
	    p.m.s2->f->c[j]++;
	    p.n.s2->f->a[j]++;
	    p.n.s2->f->c[j]++;
	  }
#pragma acc exit data delete(p.m.s2->f[:1]) delete(p.n.s2->f[:1])
#pragma acc exit data delete(p.m.s2[:1]) delete(p.n.s2[:1])
    }

  for (i = 0; i < N; i++)
    if (p.m.s2->f->a[i] != 99 || p.m.s2->f->c[i] != 99
	|| p.n.s2->f->a[i] != 99 || p.n.s2->f->c[i] != 99)
      abort ();

  zero_arrays (&p, N);

  for (int i = 0; i < 99; i++)
    {
#pragma acc enter data copyin(p.m.s2[:1]) copyin(p.n.s2[:1])
#pragma acc enter data copyin(p.n.s2->e[:N]) copyin(p.n.s2->f[:1]) \
		       copyin(p.m.s2->f[:1])
#pragma acc parallel loop copy(p.m.s2->f->a[:N]) copy(p.n.s2->f->a[:N])
	for (int j = 0; j < N; j++)
	  {
	    p.m.s2->f->a[j]++;
	    p.n.s2->f->a[j]++;
	    p.n.s2->e[j]++;
	  }
#pragma acc exit data delete(p.m.s2->f[:1]) delete(p.n.s2->f[:1]) \
		      copyout(p.n.s2->e[:N])
#pragma acc exit data delete(p.m.s2[:1]) delete(p.n.s2[:1])
    }

  for (i = 0; i < N; i++)
    if (p.m.s2->f->a[i] != 99 || p.n.s2->f->a[i] != 99
	|| p.n.s2->e[i] != 99)
      abort ();

  zero_arrays (q, N);

  for (int i = 0; i < 99; i++)
    {
#pragma acc enter data copyin(q->m.s2[:1])
#pragma acc parallel loop copy(q->m.s2->e[:N])
      for (int j = 0; j < N; j++)
	q->m.s2->e[j]++;
#pragma acc exit data delete(q->m.s2[:1])
    }

  for (i = 0; i < N; i++)
    if (q->m.s2->e[i] != 99)
      abort ();

  zero_arrays (q, N);

  for (int i = 0; i < 99; i++)
    {
#pragma acc enter data copyin(q->m.s2[:1])
#pragma acc enter data copyin(q->m.s2->f[:1])
#pragma acc parallel loop copy(q->m.s2->f->a[:N]) copy(q->m.s2->f->c[:N])
	for (int j = 0; j < N; j++)
	  {
	    q->m.s2->f->a[j]++;
	    q->m.s2->f->c[j]++;
	  }
#pragma acc exit data delete(q->m.s2->f[:1])
#pragma acc exit data delete(q->m.s2[:1])
    }

  for (i = 0; i < N; i++)
    if (q->m.s2->f->a[i] != 99 || q->m.s2->f->c[i] != 99)
      abort ();

  zero_arrays (q, N);

  for (int i = 0; i < 99; i++)
    {
#pragma acc enter data copyin(q->m.s2[:1]) copyin(q->n.s2[:1])
#pragma acc enter data copyin(q->m.s2->f[:1]) copyin(q->n.s2->f[:1])
#pragma acc parallel loop copy(q->m.s2->f->a[:N]) copy(q->m.s2->f->c[:N]) \
			  copy(q->n.s2->f->a[:N]) copy(q->n.s2->f->c[:N])
	for (int j = 0; j < N; j++)
	  {
	    q->m.s2->f->a[j]++;
	    q->m.s2->f->c[j]++;
	    q->n.s2->f->a[j]++;
	    q->n.s2->f->c[j]++;
	  }
#pragma acc exit data delete(q->m.s2->f[:1]) delete(q->n.s2->f[:1])
#pragma acc exit data delete(q->m.s2[:1]) delete(q->n.s2[:1])
    }

  for (i = 0; i < N; i++)
    if (q->m.s2->f->a[i] != 99 || q->m.s2->f->c[i] != 99
	|| q->n.s2->f->a[i] != 99 || q->n.s2->f->c[i] != 99)
      abort ();

  zero_arrays (q, N);

  for (int i = 0; i < 99; i++)
    {
#pragma acc enter data copyin(q->m.s2[:1]) copyin(q->n.s2[:1])
#pragma acc enter data copyin(q->n.s2->e[:N]) copyin(q->m.s2->f[:1]) \
		       copyin(q->n.s2->f[:1])
#pragma acc parallel loop copy(q->m.s2->f->a[:N]) copy(q->n.s2->f->a[:N])
	for (int j = 0; j < N; j++)
	  {
	    q->m.s2->f->a[j]++;
	    q->n.s2->f->a[j]++;
	    q->n.s2->e[j]++;
	  }
#pragma acc exit data delete(q->m.s2->f[:1]) delete(q->n.s2->f[:1]) \
		      copyout(q->n.s2->e[:N])
#pragma acc exit data delete(q->m.s2[:1]) delete(q->n.s2[:1])
    }

  for (i = 0; i < N; i++)
    if (q->m.s2->f->a[i] != 99 || q->n.s2->f->a[i] != 99
	|| q->n.s2->e[i] != 99)
      abort ();

  return 0;
}

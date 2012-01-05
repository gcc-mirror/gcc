/* Copyright (c) 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime library test suite.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include <upc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BLKSIZE 3
#define FACTOR 33 
#define N (FACTOR * BLKSIZE)
#define NT (N * THREADS)

shared           int a[NT];
shared [BLKSIZE] int ablocked[NT];

char *
sptr(char *buf, shared void *p)
{
  sprintf (buf, "(0x%0lx,%ld,%ld)",
    (long)upc_addrfield(p), (long)upc_threadof(p), (long)upc_phaseof(p));
  return buf;
}

enum cmp_op
{ EQ_OP, NE_OP, GT_OP, GE_OP, LT_OP, LE_OP,
  FIRST_OP = EQ_OP,
  LAST_OP = LE_OP
};
const char * const op_name_tbl[] =
  {"==", "!=", ">", ">=", "<", "<="};

enum pkind {phaseless, phased};
const char * const pkind_name_tbl[] =
  {"phaseless", "phased"};

void
test_compare (enum pkind kind, enum cmp_op op,
              int t0, int t1, int j, int k)
{
  const char * const kind_s = pkind_name_tbl[kind];
  shared[BLKSIZE] int *pp0, *pp1;
  shared int *p0, *p1;
  int bs = (kind == phased) ? BLKSIZE : 1;
  /* calculate the index of item 'j' on thread 't0'  */
  int jj = ((j / bs) * THREADS + t0) * bs + (j % bs);
  /* calculate the index of item 'k' on thread 't1'  */
  int kk = ((k / bs) * THREADS + t1) * bs + (k % bs);
  int diff = (jj - kk);
  int expected, got;
  if (jj < 0 || jj >= NT) abort ();
  if (kk < 0 || kk >= NT) abort ();
  switch (op)
    {
    case EQ_OP: expected = (diff == 0); break;
    case NE_OP: expected = (diff != 0); break;
    case GT_OP: expected = (diff >  0); break;
    case GE_OP: expected = (diff >= 0); break;
    case LT_OP: expected = (diff <  0); break;
    case LE_OP: expected = (diff <= 0); break;
    default: abort();
    }
  if (kind == phased)
    {
      pp0 = &ablocked[jj]; pp1 = &ablocked[kk];
      switch (op)
	{
	case EQ_OP: got = (pp0 == pp1); break;
	case NE_OP: got = (pp0 != pp1); break;
	case GT_OP: got = (pp0 >  pp1); break;
	case GE_OP: got = (pp0 >= pp1); break;
	case LT_OP: got = (pp0 <  pp1); break;
	case LE_OP: got = (pp0 <= pp1); break;
	default: abort ();
	}
      if (got != expected)
	{
	  char b1[100], b2[100];
	  const char * const op_s   = op_name_tbl[op];
	  const char * const p0_s   = sptr(b1, pp0);
	  const char * const p1_s   = sptr(b2, pp1);
	  fprintf (stderr, "test26: Error: thread %d: %s PTS comparison "
	           "%s failed.\n"
	           "        t0=%d t1=%d j=%d k=%d jj=%d kk=%d "
	           "p0=%s p1=%s expected=%d got=%d\n",
	           MYTHREAD, kind_s, op_s, t0, t1, j, k, jj, kk,
	           p0_s, p1_s, expected, got);
	  abort ();
	}
    }
  else
    {
      p0 = &a[jj]; p1 = &a[kk];
      switch (op)
	{
	case EQ_OP: got = (p0 == p1); break;
	case NE_OP: got = (p0 != p1); break;
	case GT_OP: got = (p0 >  p1); break;
	case GE_OP: got = (p0 >= p1); break;
	case LT_OP: got = (p0 <  p1); break;
	case LE_OP: got = (p0 <= p1); break;
	default: abort ();
	}
      if (got != expected)
	{
	  char b1[100], b2[100];
	  const char * const op_s   = op_name_tbl[op];
	  const char * const p0_s   = sptr(b1, p0);
	  const char * const p1_s   = sptr(b2, p1);
	  fprintf (stderr, "test26: Error: thread %d: %s "
	           "PTS comparison %s failed.\n"
	           "        t0=%d t1=%d j=%d k=%d jj=%d kk=%d "
		   "p0=%s p1=%s expected=%d got=%d\n",
	           MYTHREAD, kind_s, op_s, t0, t1, j, k, jj, kk,
		   p0_s, p1_s, expected, got);
	  abort ();
	}
    }
}

void
test26 ()
{
  enum pkind kind;
  enum cmp_op op;
  int t0, t1, j, k;
  t0 = MYTHREAD;
  for (kind = phaseless; kind <= phased; ++kind)
    {
      for (op = FIRST_OP; op <= LAST_OP; ++op)
        {
	  int tmax = (THREADS > 128) ? 128 : THREADS;
	  for (t1 = 0; t1 < tmax; ++t1)
	    {
	      for (j = 0; j < N; ++j)
		{
		  for (k = 0; k < N; ++k)
		    {
		      test_compare (kind, op, t0, t1, j, k);
		    }
		}
	    }
	}
     }
}


int
main ()
{
  test26 ();
  upc_barrier;
  if (!MYTHREAD)
    {
      printf ("test26: test pointer-to-shared comparison - passed.\n");
    }
  return 0;
}

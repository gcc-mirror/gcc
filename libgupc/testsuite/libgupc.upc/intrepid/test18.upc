/* Copyright (c) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
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

#include <upc_strict.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

#if !defined(__GNUC__) && !defined(__attribute__)
#define __attribute__(X) 
#endif

shared [5] int a_blk5[10*THREADS];
shared [5] int *ptr_to_blk5;
shared [3] int *ptr_to_blk3;
shared [5] int * shared shared_ptr_to_shared;
shared int shared_int;
int local_int = 99;
shared int * ptr_to_shared_int;
upc_lock_t *lock;
typedef shared [3] int a_3[3];
a_3 a_3_by_3[3*THREADS];
shared [] int a_indef[10];
shared [*] int a_star_10[10*THREADS];
/* below, unused, but caused compilation error in the past. */
shared [] int* shared shared_a_indef_ptr[THREADS] __attribute__ ((unused));
int * shared shared_ptr_to_local_int;
/* bug 39: can't statically initialize pointer-to-shared with NULL */
shared int * shared_ptr_null_init = 0;
/* bug 62: pointer to shared incomplete type with layout specifier
   not deferred to type completion */
shared [10] struct inc_struct *ptr_to_incomplete;
struct inc_struct { int x; int y; };
/* bug 95: cast of pointer to shared const misclassified as an error */
struct struct95
  {
    int field[10];
  };
shared struct struct95 shared_struct;
/* bug 96: access to 2-dim shared volatile element crashes compiler. */
shared volatile int shared_vol2[10][THREADS];
/* bug 100: local pointer set to null pointer to shared. */
double localData;
double *localPtr;
 
/* bug 103: ICE: codegen (AMD64) - fail while evaluating shared array element
   actual parameters */
shared double save_err[100][4][THREADS];

/* gimple bug. */
shared int gimple_arr[THREADS];

/* BUPC bug53 - de-reference via shared pointer to local.*/
int * shared bupc_bug53_p;

/* BUPC bug206 - de-reference via local pointer
   to pointer to unblocked shared.  */
typedef shared [] int *sintptr;
shared [] int bupc_206_A[10];
sintptr bupc_206_S;

#undef abs
#define abs(x) ((x) < 0 ? -(x) : (x))

int
process_errors (double e1, double e2, double e3, double e4)
{
  double s = abs(e1) + abs(e2) + abs(e3) + abs(e4);
  return (s < 1.0e-5);
}

/* bug 104: Erroneous code generated */
shared int bug104_TT[16][THREADS];

void
bug104_proc_1 (int i, int j)
{
  if (i == j)
     return;
  fprintf (stderr, "Error bug 104: failed - i != j (%d != %d)\n", i, j);
  abort ();
}

void
bug104_proc (int i, int j)
{
   if (j != MYTHREAD)
     return;
   /* The gets of bug104_TT[i][j] below were interfering with the
      save area on the stack where i and j are stored.
      This caused the second call to fail. */
   bug104_proc_1 (bug104_TT[i][j], i); 
   bug104_proc_1 (bug104_TT[i][j]+2, i+2); 
}

/* Bug 106: GUPC fails with internal error on a few
   UPCR regression tests on OPTERON (x86_64) systems */

typedef struct bug106_U_shared bug106_u_t;
struct bug106_U_shared 
{ 
  double local[9][8][7][6][5];
}; 
shared [] bug106_u_t *bug106_u;
 
shared [] double *
bug106_func(int i, int j)
{
  shared [] double *result;
  result = &bug106_u[0].local[1][i][j][2][0];
  return result;
}

/* bug 195: ICE: fail to parse shared array declaration after a similar
   declaration has already been seen */
extern shared [2941*2] double sh_buf[THREADS][2941*2];
shared [2941*2] double sh_buf[THREADS][2941*2];

/* bug 213: pragma upc appearing just after opening bracket not supported.
   This was causing a compilation warning, because it was an unsupported
   pragma.  The code below doesn't do anything meaningful.  It just nests
   some brackets to test that aspect of pragma upc handling.  */
void
bug213_func (shared double *x, shared double *y, shared double *z[])
{
  int i;
  for (i = 0; i < 100; ++i)
    {
      int j;
      for (j = 0; j < 100; ++j)
        {
          #pragma upc strict
	  if (x[i] < x[j])
	    {
	      z[i][j] = x[i] * y[j];
	    }
	  else
	    {
	      z[i][j] = -x[i] * y[j];
	    }
	}
    }
}

struct bupc_156_struct {
  char No;
  int  a[20];
};

shared [10] struct bupc_156_struct bupc_156;
void
bupc156_test()
{
  size_t bs_No __attribute__ ((unused));
  size_t bs_a __attribute__ ((unused));
  bs_No = upc_blocksizeof(*&(bupc_156.No));
  bs_a = upc_blocksizeof(*&(bupc_156.a));
  upc_barrier;
  if (!MYTHREAD)
    {
      shared [] char * x = &(bupc_156.No);
      int i;
      for (i = 0; i < 20; ++i)
        bupc_156.a[i] = (i + 1);
      *x = 'X';
    }
  upc_barrier;
  if (!MYTHREAD)
    {
      int *a = (int *)&bupc_156.a[0];
      int i;
      if (bupc_156.No != 'X')
        {
	  fprintf (stderr, "Error bupc156 test failed: bupc_156.No mismatch"
	                   " - expected 'X', got '%c'\n", bupc_156.No);
          abort ();
	}
      for (i = 0; i < 20; ++i)
        {
	  int expected = i + 1;
	  int got = a[i];
	  if (got != expected)
	    {
	      fprintf (stderr, "Error bupc156 test failed: "
	               "expected %d got %d at i = %d\n",
	               expected, got, i);
	      abort ();
	    }
	}
    }
  upc_barrier;
}

#define NB228 100
struct bug228_struct {
  double a[NB228];
};

shared struct bug228_struct bug228_array[THREADS];

void
bug228_test (void)
{
  int i, t;
  upc_barrier;
  for (i = 0; i <  NB228; ++i)
    bug228_array[MYTHREAD].a[i] = (double)((i + 1)*(MYTHREAD+1));
  upc_barrier;
  if (MYTHREAD > 0)
    {
      bug228_array[MYTHREAD] = bug228_array[0];
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      for (t = 0; t < THREADS; ++t)
        for (i = 0; i < NB228; ++i)
	  {
	    double expected = (double)(i + 1);
	    double got = bug228_array[t].a[i];
	    if (got != expected)
	      {
		fprintf (stderr, "Error bug228 test failed: expected %.3g got %.3g"
		                 " at t = %d, i = %d\n",
			 expected, got, t, i);
		abort ();
	      }
	  }
    }
  upc_barrier;
}

/* 'strict' is needed here, because on a 64 bit machine,
   two threads might try to write each half of a 64 bit
   word and a race condition results.  */
strict shared int bug230_ia[2][10*THREADS];

void
bug230_proc(strict shared int ia[2][10*THREADS])
{
  int i, j;
  for (i = 0; i < 2; ++i)
    for (j = 0; j < 10; ++j)
      ia[i][j*THREADS+MYTHREAD] = i*10*THREADS + (j*THREADS + MYTHREAD);
}


/* Note: Bug 229 is a compile-time bug, that this
   test reproduces when compiled.  At runtime,
   this test exercises bug 230 (when compiled with
   static threads and THREADS=2).  */
void
bug230_test()
{
  int i, j, t;
  upc_barrier;
  bug230_proc (bug230_ia);
  upc_barrier;
  if (MYTHREAD == 0)
    {
      for (t = 0; t < THREADS; ++t)
        for (i = 0; i < 2; ++i)
          for (j = 0; j < 10; ++j)
	    {
	      int expected = i*10*THREADS + (j*THREADS + t);
	      int got = bug230_ia[i][j*THREADS+t];
	      if (got != expected)
	        {
	          fprintf (stderr, "Error bug #230 test failed: expected %d got %d"
		                   " at t = %d, i = %d, j = %d\n",
	                           expected, got, t, i, j);
		  abort ();
		}
	    }
    }
  upc_barrier;
}

/* Bug 233: upc_alloc.upc fails to compile, with complaint
   about attempt to copy shared pointer to local. */

shared int bug233_v1;
shared int * shared bug233_p1;
shared int * shared bug233_p2;

void
bug233_test ()
{
  upc_barrier;
    if (!MYTHREAD)
      {
	bug233_v1 = 101;
        bug233_p1 = &bug233_v1;
        /* The statement above should compile without complaint. */
        bug233_p2 = bug233_p1;
      }
  upc_barrier;
  if (bug233_p1 == NULL)
    {
      fprintf (stderr, "Error p1 is NULL - bug233 test failed.\n");
      abort ();
    }
  if (bug233_p2 != bug233_p1)
    {
      fprintf (stderr, "Error p2 != p1 - bug233 test failed.\n");
      abort ();
    }
  if (bug233_p2 != &bug233_v1)
    {
      fprintf (stderr, "Error p2 != &v1 - bug233 test failed.\n");
      abort ();
    }
  if (*bug233_p2 != 101)
    {
      fprintf (stderr, "Error *p2 = %d, expected: 101 - bug233 test failed.\n",
                       *bug233_p2);
      abort ();
    }
  upc_barrier;
}

/* Bug 235: incorrect address calculation involving blocked PTS
 * and a negative index.  This failure was originally discovered
 * when running the "compound_test" test in the new (RC2) GWU GUTS
 * test, though it has nothing to do with that test.   This test
 * fails only if THREADS > 2.  Upon further analysis, the incorrect
 * calculation implies to all negative index values. The reasons
 * for this boil down to the need to insure that a signed
 * floor_div() and floor_mod() calculation is used when performing
 * the address calculation. */
void
bug235_test()
{
  shared [5] int *p1;
  int i;
  p1 = (shared[5] int *) upc_all_alloc(3*THREADS, 5*sizeof(int));
  if (!p1)
    {
      fprintf(stderr, "Error Bug #235 allocation failed.\n");
      abort();
    }
  for (i = 1; i <= 3*THREADS*5; ++i)
    {
      int got = upc_threadof(p1+i-1);
      int expected = ((i-1)/5)%THREADS;
      if (got != expected)
        {
          fprintf(stderr, "Error Bug #235: affinity check for p1+i-1 failed.\n"
            " i = %d MYTHREAD = %d affinity = %d expected = %d\n",
            i, MYTHREAD, got, expected);
          abort ();
        }
    }
}

/* bug 236: bugzilla/bug1126.upc fails to compile -
   reports __copyblk3 has incompatible type for argument 2
   Problem occurs when a PTS is in shared space.  The result
   of incrementing/decrementing the pointer is erroneously
   qualified as "shared".   A similar situation arises
   for regular pointer arithmetic (bug 226) which is also
   tested below. */
shared [5] int bug236_A[5*THREADS];
shared [5] int * shared bug236_ptr;
void bug236_test ()
{
  int i;
  if (!MYTHREAD)
      bug236_ptr = bug236_A;
  upc_barrier;
  for (i = 0; i < 5*THREADS; ++i)
    {
      upc_barrier;
      if (MYTHREAD == ((i/5)%THREADS))
        {
	  int expected, got;
	  expected = (i/5)%THREADS;
	  got = upc_threadof (bug236_ptr);
	  bug236_ptr = bug236_ptr + 1;
	  if (got != expected)
	    {
	      fprintf (stderr, "Error threadof(%i) = %d, "
	               "expected: %d - bug226 test failed.\n",
		       i, got, expected);
	      abort ();
	    }
        }
      upc_barrier;
    }
  upc_barrier;
  if (!MYTHREAD)
      bug236_ptr = bug236_A;
  upc_barrier;
  for (i = 0; i < 5*THREADS; ++i)
    {
      upc_barrier;
      if (MYTHREAD == ((i/5)%THREADS))
        {
	  int expected, got;
	  expected = (i/5)%THREADS;
	  got = upc_threadof (bug236_ptr++);
	  if (got != expected)
	    {
	      fprintf (stderr, "Error threadof(%i) = %d, "
	               "expected: %d - bug236 test failed.\n",
		       i, got, expected);
	      abort ();
	    }
        }
      upc_barrier;
    }
  upc_barrier;
}

int
get_prev()
{
  /* The following generated an internal compiler error
     in gimplify_expr() when compiled at -O0 using the
     packed PTS representation.  */
  int val = gimple_arr[MYTHREAD?(MYTHREAD-1):THREADS-1];
  return val;
}

void
gimple_bug_test()
{
  int expected, got;
  if (!MYTHREAD)
    {
      int i;
      for (i = 0; i < THREADS; ++i) gimple_arr[i] = (i+1);
    }
  upc_barrier;
  expected = MYTHREAD ? MYTHREAD : THREADS;
  got = get_prev();
  if (got != expected)
    {
      fprintf (stderr, "thread %i: Error: "
               "expected: %d  got %d - gimple bug test failed.\n",
	       MYTHREAD, got, expected);
      abort ();
    }
  upc_barrier;
}

void
bupc_bug53_test ()
{
  if (MYTHREAD == 0)
    {
      int i;
      bupc_bug53_p = (int *) malloc (10 * sizeof (int));
      for (i = 0; i < 10; i++)
	bupc_bug53_p[i] = i;
      for (i = 0; i < 10; i++)
	{
	  int expected = i;
	  int got = bupc_bug53_p[i];
	  if (got != expected)
	    {
	      fprintf (stderr, "Error: expected: %d, got: %d "
	               "- BUPC bug53 test failed\n",
	               expected, got);
	      abort ();
	    }
	}
      free (bupc_bug53_p);
    }
}

void
bupc_bug206_test()
{
  sintptr *local = &bupc_206_S;
  bupc_206_S = bupc_206_A;
  local[0] = NULL;  /* local[0] is an alias for 'S' */
  if (bupc_206_S != NULL)
    {
      fprintf (stderr, "%d: Error: S != NULL"
                       " - BUPC bug206 test failed.\n", MYTHREAD);
      abort();
    }
}

void
bug323_test ()
{
  static size_t shared full_filevec_datasz = 10;
  static size_t shared full_full_smemvec_cnt1 __attribute__ ((unused)) = 0;
  static size_t shared full_full_smemvec_cnt  __attribute__ ((unused)) = 0;
  static unsigned int neededsz_bytes = 0;
  while (0) {
     /* Bug 323: ICE: in statement below.
        "The cause of failure is subtle. Internally, the compiler tries to
	cache constants in the form (<type> <value>). In the failing test, there
	are two places that it creates a ((shared unsigned int) 0) constant.
	But the internal cache discards "shared", yet the code tries to compare
	the constant's type that it pulls from the cache with the (shared
	unsigned int) type. They don't match, and the assertion fails."  */
     if (full_filevec_datasz > 0 &&
        full_filevec_datasz >= neededsz_bytes) break;
  }
}


typedef struct
{
  shared [] unsigned char *r;
} RowOfBytes;

#define BLK_349 100
shared [BLK_349] unsigned char data_349[BLK_349 * THREADS];
shared RowOfBytes A_349[THREADS];

void bug349_proc (shared RowOfBytes *arr, int i)
{
  int j;
  for (j = 0; j < BLK_349; ++j)
    {
      /* Bug 349: ICE: GUPC 4.3 - assertion check on
         attempt to create a shared temp, when compiling
         sobel (optimized).  */
      arr[i].r[j] = (unsigned char) (j + 1);
    }
}

void
bug349_test ()
{
  A_349[MYTHREAD].r = (shared [] unsigned char *)
                      &data_349[BLK_349 * MYTHREAD];
  upc_barrier;
  bug349_proc (A_349, MYTHREAD);
  upc_barrier;
  if (!MYTHREAD)
    {
      int i, j;
      for (i = 0; i < THREADS; ++i)
	{
	  for (j = 0; j < BLK_349; ++j)
	    {
	      unsigned char expected = (unsigned char) (j + 1);
	      unsigned char got = A_349[i].r[j];
	      if (got != expected)
	        {
		  fprintf (stderr, "Error: expected: %d, got: %d - bug 349 test failed\n",
			   (int) expected, (int) got);
		  abort ();
	        }
	    }
	}
    }
}

#define SZ_351 1000
shared int A_351[SZ_351*THREADS];

void
bug351_test ()
{
  /* bug 351: THREADS factor ignored in sizeof() calculation when compiled in
   * dynamic environment.  */
  if (!MYTHREAD)
    {
      unsigned long expected = ((unsigned long) SZ_351
                                * (unsigned long) THREADS
                                * (unsigned long) sizeof(A_351[0]));
      unsigned long got = (unsigned long) sizeof (A_351);
      if (got != expected)
        {
	  fprintf (stderr, "Error: expected: %lu, got: %lu - bug 351 test failed\n",
		   expected, got);
	  abort ();
        }
    }
}

/*   Bug 362: ICE for pointer comparison when compiler
     configured with --enable-checking-types.

     error: type mismatch in comparison expression
     internal compiler error: verify_gimple failed */

shared [10] int A_362[20*THREADS];

void
bug362_test ()
{
  /* Code partially taken from BUPC Bugzilla bug2280.upc */
  const int expected = 1;
  int got;
  shared [10] int *p = &A_362[8];
  shared [10] int *q = &A_362[11];
  /* ICE on pointer comparison below.  Note: the comparison
     and assignment to 'got' should remain as written.  */
  if (p > q)
    got = 1;
  else
    got = 0;
  if (got != expected)
    {
      fprintf (stderr, "Error bug362: shared pointer comparison failed"
                       " got %d expected %d\n", got, expected);
      abort ();
    }
}


/* bug 402: ICE: '[*]' layout factor on multi-dimensional shared array with
   dynamic threads.  */
shared [*] int A_402[THREADS][16];

void
bug402_test ()
{
  int i, j;
  for (j = 0; j < 16; ++j)
    A_402[MYTHREAD][j] = MYTHREAD*16 + j + 1;
  upc_barrier;
  if (!MYTHREAD)
    {
      if (upc_blocksizeof(A_402) != 16)
	{
	  fprintf (stderr, "Error: blocksizeof(A_402) != 16  "
	           "- bug 402 test failed\n");
	  abort ();
	}
      for (i = 0; i < THREADS; ++i)
        for (j = 0; j < 16; ++j)
	  {
	    int expected = i*16 + j + 1;
	    int got = A_402[i][j];
	    if (got != expected)
	      {
		fprintf (stderr, "Error: expected: %d, got: %d - bug 402 test failed\n",
			 expected, got);
		abort ();
	      }
	  }
    }
}

void
test18()
{
  int i, j;
  int got;
  int expected;
  /* bug 56: can't initialize a shared pointer to NULL at auto scope */
  shared int *sptr_auto = 0;
  /* bug 63: compiler refuses to initialize a pointer-to-shared,
     unless the initialization expression is 'simple' */
  shared [3] int * p_3_init = &(a_3_by_3[2][2]);
  /* bug 67: ICE on array initializer that references the address
     of shared variable */
  shared [5] int *p5_array[] = {&a_blk5[0], &a_blk5[9],
                                &a_blk5[4*MYTHREAD], 0};
  /* bug 24: upc_elemsizeof unimplemented */
  got = upc_elemsizeof a_3_by_3;
  expected = sizeof (int);
  if (got != expected)
    {
      fprintf (stderr, "Error bug24: upc_elemsizeof failed: "
               "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 25: upc_blocksizeof fails on arrays with THREADS multiple */
  got = upc_blocksizeof a_3_by_3;
  expected = 3;
  if (got != expected)
    {
      fprintf (stderr, "Error bug25: upc_blocksizeof fails "
               "on arrays with THREADS multiple: got %d expected %d\n",
               got, expected);
      abort ();
    }
  /* bug 26: upc_blocksizeof does not return 0 for arrays
     with indefinite block size */
  got = upc_blocksizeof a_indef;
  expected = 0;
  if (got != expected)
    {
      fprintf (stderr, "Error bug26: upc_blocksizeof does not "
               "return 0 for arrays with indefinite block size: "
	       "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 31: layout '[*]' not implemented */
  got = upc_blocksizeof a_star_10;
  expected = 10;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 31: layout '[*]' not implemented: "
               "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 39: can't statically initialize pointer-to-shared with NULL */
  got = !shared_ptr_null_init;
  expected = 1;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 39: can't statically initialize "
               "pointer-to-shared with NULL: got %d expected %d\n",
               got, expected);
      abort ();
    }
  /* bug 46: upc_alloc unimplemented */
  ptr_to_shared_int = upc_alloc (sizeof (int));
  got = !!ptr_to_shared_int;
  expected = 1;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 46: upc_alloc unimplemented: "
               "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 46: upc_affinitysize unimplemented */
  got = upc_affinitysize (3 * 5 * sizeof(int) * THREADS,
                          5 * sizeof(int), MYTHREAD);  
  expected = 3 * 5 * sizeof (int);
  if (got != expected)
    {
      fprintf (stderr, "Error bug 46: upc_affinitysize unimplemented: "
               "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 48: upc_lock_free unimplemented */
  /* We can't test upc_lock_free(), so we'll just call it. */
  lock = upc_global_lock_alloc ();
  upc_lock_free (lock);
#ifndef sgi /* KNOWN BUG #89 */
  /* bug 52: upc_resetphase unimplemented */
  ptr_to_blk5 = upc_resetphase (&a_blk5[1]);
  got = upc_phaseof (ptr_to_blk5);
  expected = 0;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 52: upc_resetphase unimplemented: "
               "got %d expected %d\n", got, expected);
      abort ();
    }
#endif /* !sgi */
  /* bug 53: cast of pointer-to-shared with differing block size
     does not reset phase */
  ptr_to_blk5 = &a_blk5[3];
  ptr_to_blk3 = (shared [3] int *)ptr_to_blk5;
  got = upc_phaseof (ptr_to_blk3);
  expected = 0;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 53: cast of pointer-to-shared "
               "with differing block size does not reset phase: "
	       "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 54: upc_fence is unimplemented */
  upc_fence;
  /* bug 55: upc_poll is unimplemented */
  upc_poll();
  /* bug 56: can't initialize a shared pointer to NULL at auto scope */
  got = (sptr_auto == (shared void *)0);
  expected = 1;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 56: can't initialize a "
               "shared pointer to NULL at auto scope: got %d expected %d\n",
               got, expected);
      abort ();
    }
  /* bug 59: UPC_MAX_BLOCK_SIZE is not an integer constant */
#if UPC_MAX_BLOCK_SIZE > 0
  got = 1;
#else
  got = 0;
#endif
  expected = 1;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 59: UPC_MAX_BLOCK_SIZE is not "
               "an integer constant: got %d expected %d\n",
               got, expected);
      abort ();
    }
  /* bug 62: pointer to shared incomplete type with
     layout specifier not deferred to type completion */
  got = upc_blocksizeof *ptr_to_incomplete;
  expected = 10;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 62: pointer to shared incomplete type "
               "with layout specifier not deferred to type completion: "
	       "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 63: compiler refuses to initialize a pointer-to-shared,
     unless the initialization expression is 'simple' */
  got = (p_3_init == &a_3_by_3[2][2]);
  expected = 1;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 63: compiler refuses to initialize "
               "a pointer-to-shared, unless the initialization "
	       "expression is 'simple': got %d expected %d\n",
               got, expected);
      abort ();
    }
  /* bug 67: ICE on array initializer that references the address
     of shared variable */
  got = (p5_array[2] == &a_blk5[4*MYTHREAD]);
  expected = 1;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 67: ICE on array initializer "
               "that references the address of shared variable: "
	       "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 68: comparison between generic pointer to shared,
     and pointer to shared fails */
  ptr_to_blk5 = &a_blk5[4];
  got = ((shared void *)&a_blk5[4] == ptr_to_blk5);
  expected = 1;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 68: comparison between "
               "generic pointer to shared, and pointer to shared fails: "
	       "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 70: indirection through shared local pointer fails */
  lock = upc_all_lock_alloc ();
  upc_barrier;
  upc_lock (lock);
  shared_ptr_to_local_int = &local_int;
  got = *shared_ptr_to_local_int;
  upc_unlock (lock);
  upc_barrier;
  if (MYTHREAD == 0) upc_lock_free (lock);
  expected = 99;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 70: indirection through "
               "shared local pointer fails: got %d expected %d\n",
               got, expected);
      abort ();
    }
  /* bug 71: upc_forall with integer affinity fails */
  upc_forall (i = 0; i < THREADS; ++i; i)
  {
    got = i;
    expected = MYTHREAD;
    if (got != expected)
      {
        fprintf (stderr, "Error bug 71: upc_forall with "
	         "integer affinity fails: got %d expected %d\n",
                 got, expected);
        abort ();
      }
  }
  /* bug 75: indirection through pointer-to-shared which is
     itself shared fails */
  if (!MYTHREAD)
    {
      a_blk5[1] = 99;
      shared_ptr_to_shared = &a_blk5[1];
    }
  upc_barrier;
  got = *shared_ptr_to_shared;
  expected = 99;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 75: indirection through "
               "pointer-to-shared which is itself shared fails: "
	       "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 77: anonymous barriers generate barrier id mismatch */
  if (MYTHREAD % 2)
    {
      upc_barrier 1;
      got = (MYTHREAD % 2) == 1;
    }
  else
    {
      upc_barrier;
      got = (MYTHREAD % 2) == 0;
    }
  expected = 1;
  if (got != expected)
    {
      fprintf (stderr, "Error bug 77: anonymous barriers "
               "generate barrier id mismatch: "
	       "got %d expected %d\n", got, expected);
      abort ();
    }
  /* bug 95: cast of pointer to shared const misclassified as an error */
  upc_barrier;
  for (i = 0; i < 10; ++i)
    {
      shared_struct.field[i] = 99;
    }
  expected = 99;
  /* should not complain attempt to cast shared pointer to local pointer */
  got = *((shared int *)shared_struct.field);
  if (got != expected)
    {
      fprintf (stderr, "Error bug 95: cast of pointer to shared const"
               "misclassified as an error: got %d expected %d\n",
	       got, expected);
      abort ();
    }
  /* bug 96: ICE: combination of shared and volatile crashes the compiler */
  upc_barrier;
  for (i = 0; i < 10; ++i)
    {
      shared_vol2[i][MYTHREAD] = i*THREADS + MYTHREAD;
    }
  upc_barrier;
  if (MYTHREAD == 0)
    {
      for (i = 0; i < 10; ++i)
	{
	  for (j = 0; j < THREADS; ++j)
	    {
	      expected = i*THREADS + j;
	      got = shared_vol2[i][j];
	      if (got != expected)
		{
		  fprintf (stderr, "Error bug 96: ICE: combination of "
		           "shared and volatile crashes the "
			   "compiler failed: got %d expected %d\n",
			   got, expected);
		  abort ();
		}
	    }
	}
    }
  /* bug 100: invalid compilation error when casting null
     pointer to shared to local pointer */
  upc_barrier;
  localPtr = &localData;
  localPtr = (double *)(shared [] double *) NULL;
  if (localPtr != NULL)
    {
      fprintf (stderr, "Error bug 100: failed #1 "
               "- null local pointer expected.\n");
    }
  localPtr = &localData;
  localPtr = (double *)(shared void *) NULL;
  if (localPtr != NULL)
    {
      fprintf (stderr, "Error bug 100: failed #2 "
               "- null local pointer expected.\n");
    }
  localPtr = &localData;
  localPtr = (double *)((shared void *) 0);
  if (localPtr != NULL)
    {
      fprintf (stderr, "Error bug 100: failed #3 "
               "- null local pointer expected.\n");
    }
  upc_barrier;
  /* Bug 103: ICE: codegen (AMD64) - fail in emit_move_insn
     when evaluating shared array element actual parameters */
  for (i=0; i<100; i++)
    for (j=0; j<4; j++)
       save_err[i][j][MYTHREAD] = 1.0e-6;
  upc_barrier;
  for (i=0; i<100; i++)
   {
      int ok;
      ok = process_errors (save_err[i][0][MYTHREAD], save_err[i][1][MYTHREAD],
                           save_err[i][2][MYTHREAD], save_err[i][3][MYTHREAD]);
      if (!ok)
        {
          fprintf (stderr, "Error bug 103: failed "
	           "- save_err[%d][0..3][%d] exceeds"
                   " error tolerance (%0.3lg,%0.3lg,%0.3lg,%0.3lg)\n",
                   i, MYTHREAD,
                   save_err[i][0][MYTHREAD], save_err[i][1][MYTHREAD],
                   save_err[i][2][MYTHREAD], save_err[i][3][MYTHREAD]);
          abort ();
        }
    }
  upc_barrier;
  /* Bug 104: Erroneous code generated */
  for (i=0; i<16; i++)
    {
      bug104_TT[i][MYTHREAD] = i;
    }
  upc_barrier;
  bug104_proc (4, MYTHREAD); 
  upc_barrier;
  /* Bug 106: GUPC fails with internal error on
     a few UPCR regression tests on OPTERON (x86_64) systems */
  {
    int i1, i2, i3, i4, i5, i6;
    shared [] double *p;
    double got, expected;
    bug106_u = upc_local_alloc (10, sizeof(bug106_u_t));
    j = 0;
    for (i1 = 0; i1 < 10; ++i1)
      for (i2 = 0; i2 < 9; ++i2)
        for (i3 = 0; i3 < 8; ++i3)
          for (i4 = 0; i4 < 7; ++i4)
            for (i5 = 0; i5 < 6; ++i5)
              for (i6 = 0; i6 < 5; ++i6)
                bug106_u[i1].local[i2][i3][i4][i5][i6] = (double)(++j);
    upc_barrier;
    p = bug106_func (2, 1);
    got = *p;
    expected = 2141;
    if (got != expected)
      {
        fprintf (stderr, "Error bug 106: failed - got: %0.5g expected %0.5g\n",
	                  got, expected);
	abort ();
      }
  }
  upc_barrier;
  bupc156_test ();
  upc_barrier;
  bug228_test ();
  upc_barrier;
  bug230_test ();
  upc_barrier;
  bug233_test ();
  upc_barrier;
  bug235_test ();
  upc_barrier;
  bug236_test ();
  upc_barrier;
  gimple_bug_test ();
  upc_barrier;
  bupc_bug53_test ();
  upc_barrier;
  bupc_bug206_test ();
  upc_barrier;
  bug323_test ();
  upc_barrier;
  bug349_test ();
  upc_barrier;
  bug351_test ();
  upc_barrier;
  bug402_test ();
  upc_barrier;
  if (MYTHREAD == 0)
    {
      printf ("test18: miscellaneous regression tests - passed.\n");
    }
}

int two() { return 2; }
volatile int v2;

int
main()
{
  test18 ();
  /* bug 65: default 'return 0;' not being implemented for main program */
  /* force the value 2 into the return register before falling off 'main'
     to ensure that the return from main isn't zero, unless the compiler
     adds the required default 'return 0;'. */
  v2 = two ();
}

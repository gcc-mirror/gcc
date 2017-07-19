/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */
/* { dg-additional-options "-std=c99 -ftree-slp-vectorize -foffload=-ftree-slp-vectorize -foffload=-fdump-tree-slp1 -foffload=-save-temps -save-temps" } */

#include <stdio.h>
#include <sys/time.h>

long long int p[32 *1000] __attribute__((aligned(16)));
long long int p2[32 *1000] __attribute__((aligned(16)));

int
main (void)
{
#pragma acc parallel num_gangs(1) num_workers(1) vector_length(32)
  {
    if (((unsigned long int)p & (0xfULL)) != 0)
      __builtin_abort ();
    if (((unsigned long int)p2 & (0xfULL)) != 0)
      __builtin_abort ();

    for (unsigned int k = 0; k < 10000; k += 1)
      {
#pragma acc loop vector
	for (unsigned long long int j = 0; j < 32; j += 1)
	  {
	    unsigned long long a, b;
	    unsigned long long *p3, *p4;
	    p3 = (unsigned long long *)((unsigned long long int)p & (~0xfULL));
	    p4 = (unsigned long long *)((unsigned long long int)p2 & (~0xfULL));

	    for (unsigned int i = 0; i < 1000; i += 2)
	      {
		a = p3[j * 1000 + i];
		b = p3[j * 1000 + i + 1];
		
		p4[j * 1000 + i] = a;
		p4[j * 1000 + i + 1] = b;
	      }
	  }
      }
  }

  return 0;
}

/* Todo: make a scan-tree-dump variant that scans vec.o instead.  */
/* { dg-final { file copy -force [glob vec.o.*] [regsub \.o\. [glob vec.o.*] \.c\.] } } */
/* { dg-final { scan-tree-dump "vector\\(2\\) long long unsigned int" "slp1" } } */

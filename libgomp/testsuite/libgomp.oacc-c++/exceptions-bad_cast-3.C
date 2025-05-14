/* 'std::bad_cast' exception in OpenACC compute region, dead code.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* Wrong code for offloading execution.
   { dg-skip-if PR119692 { ! openacc_host_selected } }
   { dg-additional-options -fdump-tree-gimple } */
/* { dg-additional-options -fdump-tree-optimized-raw } */

/* See also '../libgomp.c++/target-exceptions-bad_cast-3.C'.  */

/* See also '../../../gcc/testsuite/g++.target/gcn/exceptions-bad_cast-3.C',
   '../../../gcc/testsuite/g++.target/nvptx/exceptions-bad_cast-3.C'.  */

/* For PR119692 workarounds.  */
#ifndef DEFAULT
# define DEFAULT
#endif

struct C1
{
  virtual void f()
  {}
};

struct C2 : C1
{
};

int main()
{
#pragma omp target DEFAULT
#pragma acc serial DEFAULT
  {
    C1 c1;
    bool a = false;
    asm volatile ("" : : "r" (&a) : "memory");
    if (a)
      {
	[[maybe_unused]]
	C2 &c2 = dynamic_cast<C2 &>(c1);
	/* 'std::bad_cast' is thrown.  */
      }
  }
}

/* { dg-final { scan-tree-dump-not {(?n)#pragma omp target oacc_serial map\(tofrom:_ZTI2C2 \[len: [0-9]+\]\) map\(tofrom:_ZTI2C1 \[len: [0-9]+\]\) map\(tofrom:_ZTV2C1 \[len: [0-9]+\]\)$} gimple { xfail *-*-* } } } */

/* { dg-final { scan-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } } */

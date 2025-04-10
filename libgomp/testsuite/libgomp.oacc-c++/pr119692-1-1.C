/* PR119692 "C++ 'typeinfo', 'vtable' vs. OpenACC, OpenMP 'target' offloading" */

/* { dg-additional-options -UDEFAULT }
   Wrong code for offloading execution.
   { dg-skip-if PR119692 { ! openacc_host_selected } } */
/* { dg-additional-options -fdump-tree-gimple } */

/* See also '../libgomp.c++/pr119692-1-1.C'.  */

/* See also '../../../gcc/testsuite/g++.target/gcn/pr119692-1-1.C',
   '../../../gcc/testsuite/g++.target/nvptx/pr119692-1-1.C'.  */

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
  /* { dg-bogus {using 'vector_length \(32\)', ignoring 1} {} { target openacc_nvidia_accel_selected xfail *-*-* } .-1 } */
  {
    C1 c1;
    C1 *c1p = &c1;
    asm volatile ("" : : "r" (&c1p) : "memory");
    C2 *c2 = dynamic_cast<C2 *>(c1p);
    if (c2)
      __builtin_abort();
  }
}

/* { dg-final { scan-tree-dump-not {(?n)#pragma omp target oacc_serial map\(tofrom:_ZTI2C2 \[len: [0-9]+\]\) map\(tofrom:_ZTI2C1 \[len: [0-9]+\]\) map\(tofrom:_ZTV2C1 \[len: [0-9]+\]\)$} gimple { xfail *-*-* } } } */

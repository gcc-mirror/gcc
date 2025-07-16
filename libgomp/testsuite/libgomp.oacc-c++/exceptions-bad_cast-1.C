/* 'std::bad_cast' exception in OpenACC compute region.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */

/* See also '../libgomp.c++/target-exceptions-bad_cast-1.C'.  */

/* See also '../../../gcc/testsuite/g++.target/gcn/exceptions-bad_cast-1.C',
   '../../../gcc/testsuite/g++.target/nvptx/exceptions-bad_cast-1.C'.  */

#include <iostream>

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
  std::cerr << "CheCKpOInT\n";
#pragma omp target
#pragma acc serial
  /* { dg-bogus {using 'vector_length \(32\)', ignoring 1} {} { target openacc_nvidia_accel_selected xfail *-*-* } .-1 } */
  {
    C1 c1;
    [[maybe_unused]]
    C2 &c2 = dynamic_cast<C2 &>(c1);
    /* 'std::bad_cast' is thrown.  */
  }
}

/* { dg-output {CheCKpOInT[\r\n]+} }

   { dg-final { scan-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   For host execution, we print something like:
       terminate called after throwing an instance of 'std::bad_cast'
         what():  std::bad_cast
       Aborted (core dumped)
   { dg-output {.*std::bad_cast} { target openacc_host_selected } }
   For GCN, nvptx offload execution, we don't print anything, but just 'abort'.

   TODO For GCN, nvptx offload execution, this currently doesn't 'abort' due to
   the 'std::bad_cast' exception, but rather due to SIGSEGV in 'dynamic_cast';
   PR119692.

   { dg-shouldfail {'std::bad_cast' exception} } */
/* There are configurations where we 'WARNING: program timed out.' while in
   'dynamic_cast', see <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=119692#c6>.
   { dg-timeout 10 { target offload_device } } ... to make sure that happens quickly.  */

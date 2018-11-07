// Ensure that a non-scalar dummy arguments which are implicitly used inside
// offloaded regions are properly mapped using present_or_copy semantics.

// { dg-do run }

#include <cassert>

const int n = 100;

struct data {
  int v;
};

void
kernels_present (data &d, int &x)
{
#pragma acc kernels present (d, x) default (none)
  {
    d.v = x;
  }
}

void
parallel_present (data &d, int &x)
{
#pragma acc parallel present (d, x) default (none)
  {
    d.v = x;
  }
}

void
kernels_implicit (data &d, int &x)
{
#pragma acc kernels
  {
    d.v = x;
  }
}

void
parallel_implicit (data &d, int &x)
{
#pragma acc parallel
  {
    d.v = x;
  }
}

void
reference_data (data &d, int &x)
{
#pragma acc data copy(d, x)
  {
    kernels_present (d, x);

#pragma acc update host(d)
    assert (d.v == x);

    x = 200;
#pragma acc update device(x)
    
    parallel_present (d, x);
  }

  assert (d.v == x);

  x = 300;
  kernels_implicit (d, x);
  assert (d.v == x);

  x = 400;
  parallel_implicit (d, x);
  assert (d.v == x);
}

int
main ()
{
  data d;
  int x = 100;

#pragma acc data copy(d, x)
  {
    kernels_present (d, x);

#pragma acc update host(d)
    assert (d.v == x);

    x = 200;
#pragma acc update device(x)
    
    parallel_present (d, x);
  }

  assert (d.v == x);

  x = 300;
  kernels_implicit (d, x);
  assert (d.v == x);

  x = 400;
  parallel_implicit (d, x);
  assert (d.v == x);

  reference_data (d, x);

  return 0;
}

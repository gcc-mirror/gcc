// { dg-do run }

#include <cassert>

struct S
{
  int *myarr;
};

struct T
{
  S *s;
};

#pragma omp declare mapper (s100: S x) map(to: x.myarr) \
				       map(tofrom: x.myarr[0:100])
// Define this because ...
#pragma omp declare mapper (default: S x) map(to: x.myarr) \
					  map(tofrom: x.myarr[0:100])


void
bump (T t)
{
  /* Here we have an implicit/default mapper invoking a named mapper.  We
     need to make sure that can be located properly at gimplification
     time.  */

// ... the following is invalid in OpenMP - albeit supported by GCC
// (after disabling:  error: in ‘declare mapper’ directives, parameter to ‘mapper’ modifier must be ‘default’ )

// #pragma omp declare mapper (T t) map(to:t.s) map(mapper(s100), tofrom: t.s[0])

// ... thus, we now use ...
#pragma omp declare mapper (T t) map(to:t.s) map(mapper(default), tofrom: t.s[0])

#pragma omp target
  for (int i = 0; i < 100; i++)
    t.s->myarr[i]++;
}

int main (int argc, char *argv[])
{
  S my_s;
  T my_t;

  my_s.myarr = new int[100];
  my_t.s = &my_s;

  for (int i = 0; i < 100; i++)
    my_s.myarr[i] = 0;

  bump (my_t);

  for (int i = 0; i < 100; i++)
    assert (my_s.myarr[i] == 1);

  return 0;
}

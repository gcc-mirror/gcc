/* { dg-skip-if "" { *-*-* } } */
/* Used by target-same-name-1.c */

static int local_link = 42;
#pragma omp declare target link(local_link)

int decl_a_link = 123;
#pragma omp declare target link(decl_a_link)

#pragma omp declare target
static int __attribute__ ((noinline,noclone))
foo ()
{
  return 5;
}
#pragma omp end declare target

static int __attribute__ ((noinline,noclone))
bar ()
{
  int i;
  #pragma omp target map(from:i)
    i = foo ();
  return i;
}

int
one () {
  return bar ();
}

int
one_get_inc2_local_link ()
{
  int res, res2;
#pragma omp target map(from: res, res2)
  {
    res = local_link;
    local_link += 2;
    res2 = local_link;
  }
  if (res + 2 != res2)
    __builtin_abort ();
  return res;
}

int
one_get_inc3_link_a ()
{
  int res, res2;
#pragma omp target map(from: res, res2)
  {
    res = decl_a_link;
    decl_a_link += 3;
    res2 = decl_a_link;
  }
  if (res + 3 != res2)
    __builtin_abort ();
  return res;
}

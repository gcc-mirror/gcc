/* { dg-skip-if "" { *-*-* } } */
/* Used by target-same-name-1.c */

static int local_link = 55;
#pragma omp declare target link(local_link)

extern int decl_a_link;
#pragma omp declare target link(decl_a_link)

#pragma omp declare target
static int __attribute__ ((noinline,noclone))
foo ()
{
  return 7;
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
two () {
  return bar ();
}

int
two_get_inc4_local_link ()
{
  int res, res2;
#pragma omp target map(from: res, res2)
  {
    res = local_link;
    local_link += 4;
    res2 = local_link;
  }
  if (res + 4 != res2)
    __builtin_abort ();
  return res;
}

int
two_get_inc5_link_a ()
{
  int res, res2;
#pragma omp target map(from: res, res2)
  {
    res = decl_a_link;
    decl_a_link += 5;
    res2 = decl_a_link;
  }
  if (res + 5 != res2)
    __builtin_abort ();
  return res;
}

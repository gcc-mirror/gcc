/* Test dynamic unmapping of separate structure members.  */

/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include <assert.h>
#include <openacc.h>

struct s
{
  char a;
  char b;
};

int main ()
{
  struct s s;

#pragma acc enter data create(s.a, s.b)

  assert (acc_is_present (&s.a, sizeof s.a));
  assert (acc_is_present (&s.b, sizeof s.b));

#pragma acc exit data delete(s.a)
#pragma acc exit data delete(s.b)

  assert (!acc_is_present (&s.a, sizeof s.a));
  assert (!acc_is_present (&s.b, sizeof s.b));

  return 0;
}


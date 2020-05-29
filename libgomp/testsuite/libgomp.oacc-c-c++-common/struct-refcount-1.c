/* Test dynamic unmapping of separate structure members.  */

/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include <assert.h>
#include <stdbool.h>
#include <openacc.h>

struct s
{
  char a;
  float b;
};

void test (bool use_directives)
{
  struct s s;

#pragma acc enter data create(s.a, s.b)
  assert (acc_is_present (&s.a, sizeof s.a));
  assert (acc_is_present (&s.b, sizeof s.b));

  if (use_directives)
    {
#pragma acc exit data delete(s.a)
    }
  else
    acc_delete (&s.a, sizeof s.a);
  assert (!acc_is_present (&s.a, sizeof s.a));
  assert (acc_is_present (&s.b, sizeof s.b));
  if (use_directives)
    {
#pragma acc exit data delete(s.b)
    }
  else
    acc_delete (&s.b, sizeof s.b);
  assert (!acc_is_present (&s.a, sizeof s.a));
  assert (!acc_is_present (&s.b, sizeof s.b));
}

int main ()
{
  test (true);
  test (false);

  return 0;
}

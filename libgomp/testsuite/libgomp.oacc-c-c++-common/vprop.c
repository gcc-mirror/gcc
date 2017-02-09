/* { dg-do run } */
/* { dg-xfail-run-if "PR78266" { openacc_nvidia_accel_selected } { "*" } { "" } } */

#include <assert.h>

#define test(type)				\
void						\
test_##type ()					\
{						\
  signed type b[100];				\
  signed type i, j, x = -1, y = -1;		\
						\
  _Pragma("acc parallel loop copyout (b)")	\
  for (j = 0; j > -5; j--)			\
    {						\
      type c = x+y;                             \
      _Pragma("acc loop vector")		\
      for (i = 0; i < 20; i++)			\
	b[-j*20 + i] = c;			\
      b[5-j] = c;                               \
    }						\
						\
  for (i = 0; i < 100; i++)			\
    assert (b[i] == -2);			\
}

test(char)
test(short)

int
main ()
{
  test_char ();
  test_short ();

  return 0;
}

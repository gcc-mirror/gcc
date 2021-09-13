/* { dg-do run } */
/* { dg-additional-options "-foffload-options=nvptx-none=-latomic" { target offload_target_nvptx } } */

#include <stdlib.h>

#define N 512

#define GENERATE_TEST(T)	\
int test_##T (void)		\
{				\
  T a[N], res = 0;		\
				\
  for (int i = 0; i < N; ++i)	\
    a[i] = i & 1;		\
				\
_Pragma("omp target teams distribute reduction(||:res) defaultmap(tofrom:scalar)") \
  for (int i = 0; i < N; ++i)	\
    res = res || a[i];		\
				\
  /* res should be non-zero.  */\
  if (!res)			\
    return 1;			\
				\
_Pragma("omp target teams distribute reduction(&&:res) defaultmap(tofrom:scalar)") \
  for (int i = 0; i < N; ++i)	\
    res = res && a[i];		\
				\
  /* res should be zero.  */	\
  return res;			\
}

GENERATE_TEST(char)
GENERATE_TEST(short)
GENERATE_TEST(int)
GENERATE_TEST(long)
#ifdef __SIZEOF_INT128__
GENERATE_TEST(__int128)
#endif

int main(void)
{
  if (test_char ())
    abort ();
  if (test_short ())
    abort ();
  if (test_int ())
    abort ();
  if (test_long ())
    abort ();
#ifdef __SIZEOF_INT128__
  if (test___int128 ())
    abort ();
#endif
}

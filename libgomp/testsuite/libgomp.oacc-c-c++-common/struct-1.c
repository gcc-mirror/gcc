/* Test dynamic refcount and copy behavior of separate structure members.  */

#include <assert.h>
#include <stdbool.h>
#include <openacc.h>

struct s
{
  signed char a;
  float b;
};

static void test(unsigned variant)
{
  struct s s = { .a = 73, .b = -22 };

#pragma acc enter data copyin(s.a, s.b)
  assert(acc_is_present(&s.a, sizeof s.a));
  assert(acc_is_present(&s.b, sizeof s.b));

  /* To verify that any following 'copyin' doesn't 'copyin' again.  */
  s.a = -s.a;
  s.b = -s.b;

  if (variant & 4)
    {
      if (variant & 8)
	{
#pragma acc enter data copyin(s.b)
	}
      else
	acc_copyin(&s.b, sizeof s.b);
      assert(acc_is_present(&s.a, sizeof s.a));
      assert(acc_is_present(&s.b, sizeof s.b));

      if (variant & 16)
	{
#pragma acc enter data copyin(s.a)
	}
      else
	acc_copyin(&s.a, sizeof s.a);
      assert(acc_is_present(&s.a, sizeof s.a));
      assert(acc_is_present(&s.b, sizeof s.b));

      if (variant & 32)
	{
#pragma acc enter data copyin(s.a)
	  acc_copyin(&s.b, sizeof s.b);
#pragma acc enter data copyin(s.b)
#pragma acc enter data copyin(s.b)
	  acc_copyin(&s.a, sizeof s.a);
	  acc_copyin(&s.a, sizeof s.a);
	  acc_copyin(&s.a, sizeof s.a);
	}
      assert(acc_is_present(&s.a, sizeof s.a));
      assert(acc_is_present(&s.b, sizeof s.b));
    }

#pragma acc parallel \
  copy(s.a, s.b)
  {
#if ACC_MEM_SHARED
    if (s.a++ != -73)
      __builtin_abort();
    if (s.b-- != 22)
      __builtin_abort();
#else
    if (s.a++ != 73)
      __builtin_abort();
    if (s.b-- != -22)
      __builtin_abort();
#endif
  }
#if ACC_MEM_SHARED
  assert(s.a == -72);
  assert(s.b == 21);
#else
  assert(s.a == -73);
  assert(s.b == 22);
#endif

  if (variant & 32)
    {
      if (variant & 1)
	{
#pragma acc exit data copyout(s.a) finalize
	}
      else
	acc_copyout_finalize(&s.a, sizeof s.a);
    }
  else
    {
      if (variant & 1)
	{
#pragma acc exit data copyout(s.a)
	}
      else
	acc_copyout(&s.a, sizeof s.a);
      if (variant & 4)
	{
	  assert(acc_is_present(&s.a, sizeof s.a));
	  assert(acc_is_present(&s.b, sizeof s.b));
#if ACC_MEM_SHARED
	  assert(s.a == -72);
	  assert(s.b == 21);
#else
	  assert(s.a == -73);
	  assert(s.b == 22);
#endif
	  if (variant & 1)
	    {
#pragma acc exit data copyout(s.a)
	    }
	  else
	    acc_copyout(&s.a, sizeof s.a);
	}
    }
#if ACC_MEM_SHARED
  assert(acc_is_present(&s.a, sizeof s.a));
  assert(acc_is_present(&s.b, sizeof s.b));
  assert(s.a == -72);
  assert(s.b == 21);
#else
  assert(!acc_is_present(&s.a, sizeof s.a));
  assert(acc_is_present(&s.b, sizeof s.b));
  assert(s.a == 74);
  assert(s.b == 22);
#endif

  if (variant & 32)
    {
      if (variant & 2)
	{
#pragma acc exit data copyout(s.b) finalize
	}
      else
	acc_copyout_finalize(&s.b, sizeof s.b);
    }
  else
    {
      if (variant & 2)
	{
#pragma acc exit data copyout(s.b)
	}
      else
	acc_copyout(&s.b, sizeof s.b);
      if (variant & 4)
	{
#if ACC_MEM_SHARED
	  assert(acc_is_present(&s.a, sizeof s.a));
	  assert(acc_is_present(&s.b, sizeof s.b));
	  assert(s.a == -72);
	  assert(s.b == 21);
#else
	  assert(!acc_is_present(&s.a, sizeof s.a));
	  assert(acc_is_present(&s.b, sizeof s.b));
	  assert(s.a == 74);
	  assert(s.b == 22);
#endif
	  if (variant & 2)
	    {
#pragma acc exit data copyout(s.b)
	    }
	  else
	    acc_copyout(&s.b, sizeof s.b);
	}
    }
#if ACC_MEM_SHARED
  assert(acc_is_present(&s.a, sizeof s.a));
  assert(acc_is_present(&s.b, sizeof s.b));
  assert(s.a == -72);
  assert(s.b == 21);
#else
  assert(!acc_is_present(&s.a, sizeof s.a));
  assert(!acc_is_present(&s.b, sizeof s.b));
  assert(s.a == 74);
  assert(s.b == -23);
#endif
}

int main()
{
  for (unsigned variant = 0; variant < 64; ++variant)
    test(variant);

  return 0;
}

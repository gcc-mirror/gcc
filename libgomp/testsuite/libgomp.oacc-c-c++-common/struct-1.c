/* Test dynamic refcount of separate structure members.  */

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
  struct s s;

#pragma acc enter data create(s.a, s.b)
  assert(acc_is_present(&s.a, sizeof s.a));
  assert(acc_is_present(&s.b, sizeof s.b));

  if (variant & 4)
    {
      if (variant & 8)
	{
#pragma acc enter data create(s.b)
	}
      else
	acc_create(&s.b, sizeof s.b);
      assert(acc_is_present(&s.a, sizeof s.a));
      assert(acc_is_present(&s.b, sizeof s.b));

      if (variant & 16)
	{
#pragma acc enter data create(s.a)
	}
      else
	acc_create(&s.a, sizeof s.a);
      assert(acc_is_present(&s.a, sizeof s.a));
      assert(acc_is_present(&s.b, sizeof s.b));

      if (variant & 32)
	{
#pragma acc enter data create(s.a)
	  acc_create(&s.b, sizeof s.b);
#pragma acc enter data create(s.b)
#pragma acc enter data create(s.b)
	  acc_create(&s.a, sizeof s.a);
	  acc_create(&s.a, sizeof s.a);
	  acc_create(&s.a, sizeof s.a);
	}
      assert(acc_is_present(&s.a, sizeof s.a));
      assert(acc_is_present(&s.b, sizeof s.b));
    }

#pragma acc parallel \
  copy(s.a, s.b)
  {
  }

  if (variant & 32)
    {
      if (variant & 1)
	{
#pragma acc exit data delete(s.a) finalize
	}
      else
	acc_delete_finalize(&s.a, sizeof s.a);
    }
  else
    {
      if (variant & 1)
	{
#pragma acc exit data delete(s.a)
	}
      else
	acc_delete(&s.a, sizeof s.a);
      if (variant & 4)
	{
	  assert(acc_is_present(&s.a, sizeof s.a));
	  assert(acc_is_present(&s.b, sizeof s.b));
	  if (variant & 1)
	    {
#pragma acc exit data delete(s.a)
	    }
	  else
	    acc_delete(&s.a, sizeof s.a);
	}
    }
#if ACC_MEM_SHARED
  assert(acc_is_present(&s.a, sizeof s.a));
  assert(acc_is_present(&s.b, sizeof s.b));
#else
  assert(!acc_is_present(&s.a, sizeof s.a));
  assert(acc_is_present(&s.b, sizeof s.b));
#endif

  if (variant & 32)
    {
      if (variant & 2)
	{
#pragma acc exit data delete(s.b) finalize
	}
      else
	acc_delete_finalize(&s.b, sizeof s.b);
    }
  else
    {
      if (variant & 2)
	{
#pragma acc exit data delete(s.b)
	}
      else
	acc_delete(&s.b, sizeof s.b);
      if (variant & 4)
	{
#if ACC_MEM_SHARED
	  assert(acc_is_present(&s.a, sizeof s.a));
	  assert(acc_is_present(&s.b, sizeof s.b));
#else
	  assert(!acc_is_present(&s.a, sizeof s.a));
	  assert(acc_is_present(&s.b, sizeof s.b));
#endif
	  if (variant & 2)
	    {
#pragma acc exit data delete(s.b)
	    }
	  else
	    acc_delete(&s.b, sizeof s.b);
	}
    }
#if ACC_MEM_SHARED
  assert(acc_is_present(&s.a, sizeof s.a));
  assert(acc_is_present(&s.b, sizeof s.b));
#else
  assert(!acc_is_present(&s.a, sizeof s.a));
  assert(!acc_is_present(&s.b, sizeof s.b));
#endif
}

int main()
{
  for (unsigned variant = 0; variant < 64; ++variant)
    test(variant);

  return 0;
}

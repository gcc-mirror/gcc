/* Verify that OpenACC 'attach'/'detach' doesn't interfere with reference
   counting.  */

#include <assert.h>
#include <stdlib.h>
#include <openacc.h>

/* Need to shared this (and, in particular, implicit '&data_work' in
   'attach'/'detach' clauses) between 'test' and 'test_'.  */
static unsigned char *data_work;

static void test_(unsigned variant,
		  unsigned char *data,
		  void *data_d)
{
  assert(acc_is_present(&data_work, sizeof data_work));
  assert(data_work == data);

  acc_update_self(&data_work, sizeof data_work);
  assert(data_work == data);

  if (variant & 1)
    {
#pragma acc enter data attach(data_work)
    }
  else
    acc_attach((void **) &data_work);
  acc_update_self(&data_work, sizeof data_work);
  assert(data_work == data_d);

  if (variant & 4)
    {
      if (variant & 2)
	{ // attach some more
	  data_work = data;
	  acc_attach((void **) &data_work);
#pragma acc enter data attach(data_work)
	  acc_attach((void **) &data_work);
#pragma acc enter data attach(data_work)
#pragma acc enter data attach(data_work)
#pragma acc enter data attach(data_work)
	  acc_attach((void **) &data_work);
	  acc_attach((void **) &data_work);
#pragma acc enter data attach(data_work)
	}
      else
	{}
    }
  else
    { // detach
      data_work = data;
      if (variant & 2)
	{
#pragma acc exit data detach(data_work)
	}
      else
	acc_detach((void **) &data_work);
      acc_update_self(&data_work, sizeof data_work);
      assert(data_work == data);

      // now not attached anymore

#if 0
      if (TODO)
	{
	  acc_detach(&data_work); //TODO PR95203 "libgomp: attach count underflow"
	  acc_update_self(&data_work, sizeof data_work);
	  assert(data_work == data);
	}
#endif
    }

  assert(acc_is_present(&data_work, sizeof data_work));
}

static void test(unsigned variant)
{
  const int size = sizeof (void *);
  unsigned char *data = (unsigned char *) malloc(size);
  assert(data);
  void *data_d = acc_create(data, size);
  assert(data_d);
  assert(acc_is_present(data, size));

  data_work = data;

  if (variant & 8)
    {
#pragma acc data copyin(data_work)
      test_(variant, data, data_d);
    }
  else
    {
      acc_copyin(&data_work, sizeof data_work);
      test_(variant, data, data_d);
      acc_delete(&data_work, sizeof data_work);
    }
#if ACC_MEM_SHARED
  assert(acc_is_present(&data_work, sizeof data_work));
#else
  assert(!acc_is_present(&data_work, sizeof data_work));
#endif
  data_work = NULL;

  assert(acc_is_present(data, size));
  acc_delete(data, size);
  data_d = NULL;
#if ACC_MEM_SHARED
  assert(acc_is_present(data, size));
#else
  assert(!acc_is_present(data, size));
#endif
  free(data);
  data = NULL;
}

int main()
{
  for (size_t i = 0; i < 16; ++i)
    test(i);

  return 0;
}

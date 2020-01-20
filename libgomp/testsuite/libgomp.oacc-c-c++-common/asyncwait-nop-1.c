/* Several of the async/wait combinations invoked here are no-ops -- they don't
   effect anything, but are still valid.

   This doesn't verify that the asynchronous operations synchronize correctly,
   but just verifies that we don't refuse any variants.  */

#undef NDEBUG
#include <assert.h>
#include <openacc.h>

int values[] = { acc_async_sync,
		 acc_async_noval,
		 0,
		 1,
		 2,
		 36,
		 1982, };
const size_t values_n = sizeof values / sizeof values[0];

int
main ()
{
  /* Explicitly initialize: it's not clear whether the following OpenACC
     runtime library calls implicitly initialize;
     <https://github.com/OpenACC/openacc-spec/issues/102>.  */
  acc_device_t d;
#if defined ACC_DEVICE_TYPE_nvidia
  d = acc_device_nvidia;
#elif defined ACC_DEVICE_TYPE_gcn
  d = acc_device_radeon;
#elif defined ACC_DEVICE_TYPE_host
  d = acc_device_host;
#else
# error Not ported to this ACC_DEVICE_TYPE
#endif
  acc_init (d);


  for (size_t i = 0; i < values_n; ++i)
    assert (acc_async_test (values[i]) == 1);


  for (size_t i = 0; i < values_n; ++i)
    {
#pragma acc parallel wait (values[i])
      ;
#pragma acc wait (values[i])
      acc_wait (values[i]);
    }


  for (size_t i = 0; i < values_n; ++i)
    {
      for (size_t j = 0; j < values_n; ++j)
	{
#pragma acc parallel wait (values[i]) async (values[j])
	  ;
#pragma acc wait (values[i]) async (values[j])
	  acc_wait_async (values[i], values[j]);
	}
    }


  for (size_t i = 0; i < values_n; ++i)
    {
#pragma acc parallel wait async (values[i])
      ;
#pragma acc wait async (values[i])
      acc_wait_all_async (values[i]);
    }


  /* Clean up.  */
  acc_wait_all ();

  return 0;
}

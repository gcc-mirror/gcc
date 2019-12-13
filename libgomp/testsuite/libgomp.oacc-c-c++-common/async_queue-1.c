/* { dg-do run { target openacc_nvidia_accel_selected } } */

/* Test mapping of async values to specific underlying queues.  */

#undef NDEBUG
#include <assert.h>
#include <openacc.h>

/* This is implemented in terms of the "acc_get_cuda_stream" interface.  */

struct
{
  int async;
  void *cuda_stream;
} queues[] = { { acc_async_sync, NULL },
	       { acc_async_noval, NULL },
	       { 0, NULL },
	       { 1, NULL },
	       { 2, NULL },
	       { 36, NULL },
	       { 1982, NULL } };
const size_t queues_n = sizeof queues / sizeof queues[0];

int main(void)
{
  /* Explicitly initialize: it's not clear whether the following OpenACC
     runtime library calls implicitly initialize;
     <https://github.com/OpenACC/openacc-spec/issues/102>.  */
  acc_device_t d;
#if defined ACC_DEVICE_TYPE_nvidia
  d = acc_device_nvidia;
#elif defined ACC_DEVICE_TYPE_host
  d = acc_device_host;
#else
# error Not ported to this ACC_DEVICE_TYPE
#endif
  acc_init (d);

  for (size_t i = 0; i < queues_n; ++i)
    {
      /* Before actually being used, there are all NULL.  */
      queues[i].cuda_stream = acc_get_cuda_stream (queues[i].async);
      assert (queues[i].cuda_stream == NULL);
    }

  /* No-ops still don't initialize them.  */
  {
    size_t i = 0;
    /* Find the first non-special async-argument.  */
    while (queues[i].async < 0)
      ++i;
    assert (i < queues_n);

#pragma acc wait(queues[i].async) // no-op

    ++i;
    assert (i < queues_n);
#pragma acc parallel wait(queues[i].async) // no-op
    ;

    ++i;
    assert (i < queues_n);
    acc_wait(queues[i].async); // no-op

    i += 2;
    assert (i < queues_n);
    acc_wait_async(queues[i - 1].async, queues[i].async); // no-op, and async queue "i" does not get set up

    for (size_t i = 0; i < queues_n; ++i)
      {
	queues[i].cuda_stream = acc_get_cuda_stream (queues[i].async);
	assert (queues[i].cuda_stream == NULL);
      }
  }

  for (size_t i = 0; i < queues_n; ++i)
    {
      /* Use the queue to initialize it.  */
#pragma acc parallel async(queues[i].async)
      ;
#pragma acc wait

      /* Verify CUDA stream used.  */
      queues[i].cuda_stream = acc_get_cuda_stream (queues[i].async);
#if defined ACC_DEVICE_TYPE_nvidia
      /* "acc_async_sync" maps to the NULL CUDA default stream.  */
      if (queues[i].async == acc_async_sync)
	assert (queues[i].cuda_stream == NULL);
      else
	assert (queues[i].cuda_stream != NULL);
#elif defined ACC_DEVICE_TYPE_host
      /* For "acc_device_host" there are no CUDA streams.  */
      assert (queues[i].cuda_stream == NULL);
#else
# error Not ported to this ACC_DEVICE_TYPE
#endif
    }

  /* Verify same results.  */
  for (size_t i = 0; i < queues_n; ++i)
    {
      void *cuda_stream;

      cuda_stream = acc_get_cuda_stream (queues[i].async);
      assert (cuda_stream == queues[i].cuda_stream);

#pragma acc parallel async(queues[i].async)
      ;
#pragma acc wait

      cuda_stream = acc_get_cuda_stream (queues[i].async);
      assert (cuda_stream == queues[i].cuda_stream);
    }

  /* Verify individual underlying queues are all different.  */
  for (size_t i = 0; i < queues_n; ++i)
    {
      if (queues[i].cuda_stream == NULL)
	continue;
      for (size_t j = i + 1; j < queues_n; ++j)
	{
	  if (queues[j].cuda_stream == NULL)
	    continue;
	  assert (queues[j].cuda_stream != queues[i].cuda_stream);
	}
    }

  return 0;
}

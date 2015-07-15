/* Plugin for NVPTX execution.

   Copyright (C) 2013-2015 Free Software Foundation, Inc.

   Contributed by Mentor Embedded.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Nvidia PTX-specific parts of OpenACC support.  The cuda driver
   library appears to hold some implicit state, but the documentation
   is not clear as to what that state might be.  Or how one might
   propagate it from one thread to another.  */

#include "openacc.h"
#include "config.h"
#include "libgomp-plugin.h"
#include "oacc-ptx.h"
#include "oacc-plugin.h"

#include <pthread.h>
#include <cuda.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <dlfcn.h>
#include <unistd.h>
#include <assert.h>

#define	ARRAYSIZE(X) (sizeof (X) / sizeof ((X)[0]))

static struct
{
  CUresult r;
  char *m;
} cuda_errlist[]=
{
  { CUDA_ERROR_INVALID_VALUE, "invalid value" },
  { CUDA_ERROR_OUT_OF_MEMORY, "out of memory" },
  { CUDA_ERROR_NOT_INITIALIZED, "not initialized" },
  { CUDA_ERROR_DEINITIALIZED, "deinitialized" },
  { CUDA_ERROR_PROFILER_DISABLED, "profiler disabled" },
  { CUDA_ERROR_PROFILER_NOT_INITIALIZED, "profiler not initialized" },
  { CUDA_ERROR_PROFILER_ALREADY_STARTED, "already started" },
  { CUDA_ERROR_PROFILER_ALREADY_STOPPED, "already stopped" },
  { CUDA_ERROR_NO_DEVICE, "no device" },
  { CUDA_ERROR_INVALID_DEVICE, "invalid device" },
  { CUDA_ERROR_INVALID_IMAGE, "invalid image" },
  { CUDA_ERROR_INVALID_CONTEXT, "invalid context" },
  { CUDA_ERROR_CONTEXT_ALREADY_CURRENT, "context already current" },
  { CUDA_ERROR_MAP_FAILED, "map error" },
  { CUDA_ERROR_UNMAP_FAILED, "unmap error" },
  { CUDA_ERROR_ARRAY_IS_MAPPED, "array is mapped" },
  { CUDA_ERROR_ALREADY_MAPPED, "already mapped" },
  { CUDA_ERROR_NO_BINARY_FOR_GPU, "no binary for gpu" },
  { CUDA_ERROR_ALREADY_ACQUIRED, "already acquired" },
  { CUDA_ERROR_NOT_MAPPED, "not mapped" },
  { CUDA_ERROR_NOT_MAPPED_AS_ARRAY, "not mapped as array" },
  { CUDA_ERROR_NOT_MAPPED_AS_POINTER, "not mapped as pointer" },
  { CUDA_ERROR_ECC_UNCORRECTABLE, "ecc uncorrectable" },
  { CUDA_ERROR_UNSUPPORTED_LIMIT, "unsupported limit" },
  { CUDA_ERROR_CONTEXT_ALREADY_IN_USE, "context already in use" },
  { CUDA_ERROR_PEER_ACCESS_UNSUPPORTED, "peer access unsupported" },
  { CUDA_ERROR_INVALID_SOURCE, "invalid source" },
  { CUDA_ERROR_FILE_NOT_FOUND, "file not found" },
  { CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND,
                                           "shared object symbol not found" },
  { CUDA_ERROR_SHARED_OBJECT_INIT_FAILED, "shared object init error" },
  { CUDA_ERROR_OPERATING_SYSTEM, "operating system" },
  { CUDA_ERROR_INVALID_HANDLE, "invalid handle" },
  { CUDA_ERROR_NOT_FOUND, "not found" },
  { CUDA_ERROR_NOT_READY, "not ready" },
  { CUDA_ERROR_LAUNCH_FAILED, "launch error" },
  { CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES, "launch out of resources" },
  { CUDA_ERROR_LAUNCH_TIMEOUT, "launch timeout" },
  { CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING,
                                             "launch incompatibe texturing" },
  { CUDA_ERROR_PEER_ACCESS_ALREADY_ENABLED, "peer access already enabled" },
  { CUDA_ERROR_PEER_ACCESS_NOT_ENABLED, "peer access not enabled " },
  { CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE, "primary cotext active" },
  { CUDA_ERROR_CONTEXT_IS_DESTROYED, "context is destroyed" },
  { CUDA_ERROR_ASSERT, "assert" },
  { CUDA_ERROR_TOO_MANY_PEERS, "too many peers" },
  { CUDA_ERROR_HOST_MEMORY_ALREADY_REGISTERED,
                                           "host memory already registered" },
  { CUDA_ERROR_HOST_MEMORY_NOT_REGISTERED, "host memory not registered" },
  { CUDA_ERROR_NOT_PERMITTED, "not permitted" },
  { CUDA_ERROR_NOT_SUPPORTED, "not supported" },
  { CUDA_ERROR_UNKNOWN, "unknown" }
};

static char errmsg[128];

static char *
cuda_error (CUresult r)
{
  int i;

  for (i = 0; i < ARRAYSIZE (cuda_errlist); i++)
    {
      if (cuda_errlist[i].r == r)
	return &cuda_errlist[i].m[0];
    }

  sprintf (&errmsg[0], "unknown result code: %5d", r);

  return &errmsg[0];
}

struct targ_fn_descriptor
{
  CUfunction fn;
  const char *name;
};

static unsigned int instantiated_devices = 0;
static pthread_mutex_t ptx_dev_lock = PTHREAD_MUTEX_INITIALIZER;

struct ptx_stream
{
  CUstream stream;
  pthread_t host_thread;
  bool multithreaded;

  CUdeviceptr d;
  void *h;
  void *h_begin;
  void *h_end;
  void *h_next;
  void *h_prev;
  void *h_tail;

  struct ptx_stream *next;
};

/* Thread-specific data for PTX.  */

struct nvptx_thread
{
  struct ptx_stream *current_stream;
  struct ptx_device *ptx_dev;
};

struct map
{
  int     async;
  size_t  size;
  char    mappings[0];
};

static void
map_init (struct ptx_stream *s)
{
  CUresult r;

  int size = getpagesize ();

  assert (s);
  assert (!s->d);
  assert (!s->h);

  r = cuMemAllocHost (&s->h, size);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuMemAllocHost error: %s", cuda_error (r));

  r = cuMemHostGetDevicePointer (&s->d, s->h, 0);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuMemHostGetDevicePointer error: %s", cuda_error (r));

  assert (s->h);

  s->h_begin = s->h;
  s->h_end = s->h_begin + size;
  s->h_next = s->h_prev = s->h_tail = s->h_begin;

  assert (s->h_next);
  assert (s->h_end);
}

static void
map_fini (struct ptx_stream *s)
{
  CUresult r;

  r = cuMemFreeHost (s->h);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuMemFreeHost error: %s", cuda_error (r));
}

static void
map_pop (struct ptx_stream *s)
{
  struct map *m;

  assert (s != NULL);
  assert (s->h_next);
  assert (s->h_prev);
  assert (s->h_tail);

  m = s->h_tail;

  s->h_tail += m->size;

  if (s->h_tail >= s->h_end)
    s->h_tail = s->h_begin + (int) (s->h_tail - s->h_end);

  if (s->h_next == s->h_tail)
    s->h_prev = s->h_next;

  assert (s->h_next >= s->h_begin);
  assert (s->h_tail >= s->h_begin);
  assert (s->h_prev >= s->h_begin);

  assert (s->h_next <= s->h_end);
  assert (s->h_tail <= s->h_end);
  assert (s->h_prev <= s->h_end);
}

static void
map_push (struct ptx_stream *s, int async, size_t size, void **h, void **d)
{
  int left;
  int offset;
  struct map *m;

  assert (s != NULL);

  left = s->h_end - s->h_next;
  size += sizeof (struct map);

  assert (s->h_prev);
  assert (s->h_next);

  if (size >= left)
    {
      m = s->h_prev;
      m->size += left;
      s->h_next = s->h_begin;

      if (s->h_next + size > s->h_end)
	GOMP_PLUGIN_fatal ("unable to push map");
    }

  assert (s->h_next);

  m = s->h_next;
  m->async = async;
  m->size = size;

  offset = (void *)&m->mappings[0] - s->h;

  *d = (void *)(s->d + offset);
  *h = (void *)(s->h + offset);

  s->h_prev = s->h_next;
  s->h_next += size;

  assert (s->h_prev);
  assert (s->h_next);

  assert (s->h_next >= s->h_begin);
  assert (s->h_tail >= s->h_begin);
  assert (s->h_prev >= s->h_begin);
  assert (s->h_next <= s->h_end);
  assert (s->h_tail <= s->h_end);
  assert (s->h_prev <= s->h_end);

  return;
}

struct ptx_device
{
  CUcontext ctx;
  bool ctx_shared;
  CUdevice dev;
  struct ptx_stream *null_stream;
  /* All non-null streams associated with this device (actually context),
     either created implicitly or passed in from the user (via
     acc_set_cuda_stream).  */
  struct ptx_stream *active_streams;
  struct {
    struct ptx_stream **arr;
    int size;
  } async_streams;
  /* A lock for use when manipulating the above stream list and array.  */
  pthread_mutex_t stream_lock;
  int ord;
  bool overlap;
  bool map;
  bool concur;
  int  mode;
  bool mkern;

  struct ptx_device *next;
};

enum ptx_event_type
{
  PTX_EVT_MEM,
  PTX_EVT_KNL,
  PTX_EVT_SYNC,
  PTX_EVT_ASYNC_CLEANUP
};

struct ptx_event
{
  CUevent *evt;
  int type;
  void *addr;
  int ord;

  struct ptx_event *next;
};

struct ptx_image_data
{
  void *target_data;
  CUmodule module;
  struct ptx_image_data *next;
};

static pthread_mutex_t ptx_event_lock;
static struct ptx_event *ptx_events;

static struct ptx_device **ptx_devices;

static struct ptx_image_data *ptx_images = NULL;
static pthread_mutex_t ptx_image_lock = PTHREAD_MUTEX_INITIALIZER;

#define _XSTR(s) _STR(s)
#define _STR(s) #s

static struct _synames
{
  char *n;
} cuda_symnames[] =
{
  { _XSTR (cuCtxCreate) },
  { _XSTR (cuCtxDestroy) },
  { _XSTR (cuCtxGetCurrent) },
  { _XSTR (cuCtxPushCurrent) },
  { _XSTR (cuCtxSynchronize) },
  { _XSTR (cuDeviceGet) },
  { _XSTR (cuDeviceGetAttribute) },
  { _XSTR (cuDeviceGetCount) },
  { _XSTR (cuEventCreate) },
  { _XSTR (cuEventDestroy) },
  { _XSTR (cuEventQuery) },
  { _XSTR (cuEventRecord) },
  { _XSTR (cuInit) },
  { _XSTR (cuLaunchKernel) },
  { _XSTR (cuLinkAddData) },
  { _XSTR (cuLinkComplete) },
  { _XSTR (cuLinkCreate) },
  { _XSTR (cuMemAlloc) },
  { _XSTR (cuMemAllocHost) },
  { _XSTR (cuMemcpy) },
  { _XSTR (cuMemcpyDtoH) },
  { _XSTR (cuMemcpyDtoHAsync) },
  { _XSTR (cuMemcpyHtoD) },
  { _XSTR (cuMemcpyHtoDAsync) },
  { _XSTR (cuMemFree) },
  { _XSTR (cuMemFreeHost) },
  { _XSTR (cuMemGetAddressRange) },
  { _XSTR (cuMemHostGetDevicePointer) },
  { _XSTR (cuMemHostRegister) },
  { _XSTR (cuMemHostUnregister) },
  { _XSTR (cuModuleGetFunction) },
  { _XSTR (cuModuleLoadData) },
  { _XSTR (cuStreamDestroy) },
  { _XSTR (cuStreamQuery) },
  { _XSTR (cuStreamSynchronize) },
  { _XSTR (cuStreamWaitEvent) }
};

static int
verify_device_library (void)
{
  int i;
  void *dh, *ds;

  dh = dlopen ("libcuda.so", RTLD_LAZY);
  if (!dh)
    return -1;

  for (i = 0; i < ARRAYSIZE (cuda_symnames); i++)
    {
      ds = dlsym (dh, cuda_symnames[i].n);
      if (!ds)
        return -1;
    }

  dlclose (dh);

  return 0;
}

static inline struct nvptx_thread *
nvptx_thread (void)
{
  return (struct nvptx_thread *) GOMP_PLUGIN_acc_thread ();
}

static void
init_streams_for_device (struct ptx_device *ptx_dev, int concurrency)
{
  int i;
  struct ptx_stream *null_stream
    = GOMP_PLUGIN_malloc (sizeof (struct ptx_stream));

  null_stream->stream = NULL;
  null_stream->host_thread = pthread_self ();
  null_stream->multithreaded = true;
  null_stream->d = (CUdeviceptr) NULL;
  null_stream->h = NULL;
  map_init (null_stream);
  ptx_dev->null_stream = null_stream;

  ptx_dev->active_streams = NULL;
  pthread_mutex_init (&ptx_dev->stream_lock, NULL);

  if (concurrency < 1)
    concurrency = 1;

  /* This is just a guess -- make space for as many async streams as the
     current device is capable of concurrently executing.  This can grow
     later as necessary.  No streams are created yet.  */
  ptx_dev->async_streams.arr
    = GOMP_PLUGIN_malloc (concurrency * sizeof (struct ptx_stream *));
  ptx_dev->async_streams.size = concurrency;

  for (i = 0; i < concurrency; i++)
    ptx_dev->async_streams.arr[i] = NULL;
}

static void
fini_streams_for_device (struct ptx_device *ptx_dev)
{
  free (ptx_dev->async_streams.arr);

  while (ptx_dev->active_streams != NULL)
    {
      struct ptx_stream *s = ptx_dev->active_streams;
      ptx_dev->active_streams = ptx_dev->active_streams->next;

      map_fini (s);
      cuStreamDestroy (s->stream);
      free (s);
    }

  map_fini (ptx_dev->null_stream);
  free (ptx_dev->null_stream);
}

/* Select a stream for (OpenACC-semantics) ASYNC argument for the current
   thread THREAD (and also current device/context).  If CREATE is true, create
   the stream if it does not exist (or use EXISTING if it is non-NULL), and
   associate the stream with the same thread argument.  Returns stream to use
   as result.  */

static struct ptx_stream *
select_stream_for_async (int async, pthread_t thread, bool create,
			 CUstream existing)
{
  struct nvptx_thread *nvthd = nvptx_thread ();
  /* Local copy of TLS variable.  */
  struct ptx_device *ptx_dev = nvthd->ptx_dev;
  struct ptx_stream *stream = NULL;
  int orig_async = async;

  /* The special value acc_async_noval (-1) maps (for now) to an
     implicitly-created stream, which is then handled the same as any other
     numbered async stream.  Other options are available, e.g. using the null
     stream for anonymous async operations, or choosing an idle stream from an
     active set.  But, stick with this for now.  */
  if (async > acc_async_sync)
    async++;

  if (create)
    pthread_mutex_lock (&ptx_dev->stream_lock);

  /* NOTE: AFAICT there's no particular need for acc_async_sync to map to the
     null stream, and in fact better performance may be obtainable if it doesn't
     (because the null stream enforces overly-strict synchronisation with
     respect to other streams for legacy reasons, and that's probably not
     needed with OpenACC).  Maybe investigate later.  */
  if (async == acc_async_sync)
    stream = ptx_dev->null_stream;
  else if (async >= 0 && async < ptx_dev->async_streams.size
	   && ptx_dev->async_streams.arr[async] && !(create && existing))
    stream = ptx_dev->async_streams.arr[async];
  else if (async >= 0 && create)
    {
      if (async >= ptx_dev->async_streams.size)
	{
	  int i, newsize = ptx_dev->async_streams.size * 2;

	  if (async >= newsize)
	    newsize = async + 1;

	  ptx_dev->async_streams.arr
	    = GOMP_PLUGIN_realloc (ptx_dev->async_streams.arr,
				   newsize * sizeof (struct ptx_stream *));

	  for (i = ptx_dev->async_streams.size; i < newsize; i++)
	    ptx_dev->async_streams.arr[i] = NULL;

	  ptx_dev->async_streams.size = newsize;
	}

      /* Create a new stream on-demand if there isn't one already, or if we're
	 setting a particular async value to an existing (externally-provided)
	 stream.  */
      if (!ptx_dev->async_streams.arr[async] || existing)
        {
	  CUresult r;
	  struct ptx_stream *s
	    = GOMP_PLUGIN_malloc (sizeof (struct ptx_stream));

	  if (existing)
	    s->stream = existing;
	  else
	    {
	      r = cuStreamCreate (&s->stream, CU_STREAM_DEFAULT);
	      if (r != CUDA_SUCCESS)
		GOMP_PLUGIN_fatal ("cuStreamCreate error: %s", cuda_error (r));
	    }

	  /* If CREATE is true, we're going to be queueing some work on this
	     stream.  Associate it with the current host thread.  */
	  s->host_thread = thread;
	  s->multithreaded = false;

	  s->d = (CUdeviceptr) NULL;
	  s->h = NULL;
	  map_init (s);

	  s->next = ptx_dev->active_streams;
	  ptx_dev->active_streams = s;
	  ptx_dev->async_streams.arr[async] = s;
	}

      stream = ptx_dev->async_streams.arr[async];
    }
  else if (async < 0)
    GOMP_PLUGIN_fatal ("bad async %d", async);

  if (create)
    {
      assert (stream != NULL);

      /* If we're trying to use the same stream from different threads
	 simultaneously, set stream->multithreaded to true.  This affects the
	 behaviour of acc_async_test_all and acc_wait_all, which are supposed to
	 only wait for asynchronous launches from the same host thread they are
	 invoked on.  If multiple threads use the same async value, we make note
	 of that here and fall back to testing/waiting for all threads in those
	 functions.  */
      if (thread != stream->host_thread)
        stream->multithreaded = true;

      pthread_mutex_unlock (&ptx_dev->stream_lock);
    }
  else if (stream && !stream->multithreaded
	   && !pthread_equal (stream->host_thread, thread))
    GOMP_PLUGIN_fatal ("async %d used on wrong thread", orig_async);

  return stream;
}

/* Initialize the device.  Return TRUE on success, else FALSE.  PTX_DEV_LOCK
   should be locked on entry and remains locked on exit.  */
static bool
nvptx_init (void)
{
  CUresult r;
  int rc;
  int ndevs;

  if (instantiated_devices != 0)
    return true;

  rc = verify_device_library ();
  if (rc < 0)
    return false;

  r = cuInit (0);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuInit error: %s", cuda_error (r));

  ptx_events = NULL;

  pthread_mutex_init (&ptx_event_lock, NULL);

  r = cuDeviceGetCount (&ndevs);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuDeviceGetCount error: %s", cuda_error (r));

  ptx_devices = GOMP_PLUGIN_malloc_cleared (sizeof (struct ptx_device *)
					    * ndevs);

  return true;
}

/* Select the N'th PTX device for the current host thread.  The device must
   have been previously opened before calling this function.  */

static void
nvptx_attach_host_thread_to_device (int n)
{
  CUdevice dev;
  CUresult r;
  struct ptx_device *ptx_dev;
  CUcontext thd_ctx;

  r = cuCtxGetDevice (&dev);
  if (r != CUDA_SUCCESS && r != CUDA_ERROR_INVALID_CONTEXT)
    GOMP_PLUGIN_fatal ("cuCtxGetDevice error: %s", cuda_error (r));

  if (r != CUDA_ERROR_INVALID_CONTEXT && dev == n)
    return;
  else
    {
      CUcontext old_ctx;

      ptx_dev = ptx_devices[n];
      assert (ptx_dev);

      r = cuCtxGetCurrent (&thd_ctx);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuCtxGetCurrent error: %s", cuda_error (r));

      /* We don't necessarily have a current context (e.g. if it has been
         destroyed.  Pop it if we do though.  */
      if (thd_ctx != NULL)
	{
	  r = cuCtxPopCurrent (&old_ctx);
	  if (r != CUDA_SUCCESS)
            GOMP_PLUGIN_fatal ("cuCtxPopCurrent error: %s", cuda_error (r));
	}

      r = cuCtxPushCurrent (ptx_dev->ctx);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuCtxPushCurrent error: %s", cuda_error (r));
    }
}

static struct ptx_device *
nvptx_open_device (int n)
{
  struct ptx_device *ptx_dev;
  CUdevice dev, ctx_dev;
  CUresult r;
  int async_engines, pi;

  r = cuDeviceGet (&dev, n);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuDeviceGet error: %s", cuda_error (r));

  ptx_dev = GOMP_PLUGIN_malloc (sizeof (struct ptx_device));

  ptx_dev->ord = n;
  ptx_dev->dev = dev;
  ptx_dev->ctx_shared = false;

  r = cuCtxGetDevice (&ctx_dev);
  if (r != CUDA_SUCCESS && r != CUDA_ERROR_INVALID_CONTEXT)
    GOMP_PLUGIN_fatal ("cuCtxGetDevice error: %s", cuda_error (r));
  
  if (r != CUDA_ERROR_INVALID_CONTEXT && ctx_dev != dev)
    {
      /* The current host thread has an active context for a different device.
         Detach it.  */
      CUcontext old_ctx;
      
      r = cuCtxPopCurrent (&old_ctx);
      if (r != CUDA_SUCCESS)
	GOMP_PLUGIN_fatal ("cuCtxPopCurrent error: %s", cuda_error (r));
    }

  r = cuCtxGetCurrent (&ptx_dev->ctx);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuCtxGetCurrent error: %s", cuda_error (r));

  if (!ptx_dev->ctx)
    {
      r = cuCtxCreate (&ptx_dev->ctx, CU_CTX_SCHED_AUTO, dev);
      if (r != CUDA_SUCCESS)
	GOMP_PLUGIN_fatal ("cuCtxCreate error: %s", cuda_error (r));
    }
  else
    ptx_dev->ctx_shared = true;

  r = cuDeviceGetAttribute (&pi, CU_DEVICE_ATTRIBUTE_GPU_OVERLAP, dev);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuDeviceGetAttribute error: %s", cuda_error (r));

  ptx_dev->overlap = pi;

  r = cuDeviceGetAttribute (&pi, CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY, dev);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuDeviceGetAttribute error: %s", cuda_error (r));

  ptx_dev->map = pi;

  r = cuDeviceGetAttribute (&pi, CU_DEVICE_ATTRIBUTE_CONCURRENT_KERNELS, dev);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuDeviceGetAttribute error: %s", cuda_error (r));

  ptx_dev->concur = pi;

  r = cuDeviceGetAttribute (&pi, CU_DEVICE_ATTRIBUTE_COMPUTE_MODE, dev);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuDeviceGetAttribute error: %s", cuda_error (r));

  ptx_dev->mode = pi;

  r = cuDeviceGetAttribute (&pi, CU_DEVICE_ATTRIBUTE_INTEGRATED, dev);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuDeviceGetAttribute error: %s", cuda_error (r));

  ptx_dev->mkern = pi;

  r = cuDeviceGetAttribute (&async_engines,
			    CU_DEVICE_ATTRIBUTE_ASYNC_ENGINE_COUNT, dev);
  if (r != CUDA_SUCCESS)
    async_engines = 1;

  init_streams_for_device (ptx_dev, async_engines);

  return ptx_dev;
}

static void
nvptx_close_device (struct ptx_device *ptx_dev)
{
  CUresult r;

  if (!ptx_dev)
    return;

  fini_streams_for_device (ptx_dev);

  if (!ptx_dev->ctx_shared)
    {
      r = cuCtxDestroy (ptx_dev->ctx);
      if (r != CUDA_SUCCESS)
	GOMP_PLUGIN_fatal ("cuCtxDestroy error: %s", cuda_error (r));
    }

  free (ptx_dev);
}

static int
nvptx_get_num_devices (void)
{
  int n;
  CUresult r;

  /* PR libgomp/65099: Currently, we only support offloading in 64-bit
     configurations.  */
  if (sizeof (void *) != 8)
    return 0;

  /* This function will be called before the plugin has been initialized in
     order to enumerate available devices, but CUDA API routines can't be used
     until cuInit has been called.  Just call it now (but don't yet do any
     further initialization).  */
  if (instantiated_devices == 0)
    cuInit (0);

  r = cuDeviceGetCount (&n);
  if (r!= CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuDeviceGetCount error: %s", cuda_error (r));

  return n;
}


static void
link_ptx (CUmodule *module, char *ptx_code)
{
  CUjit_option opts[7];
  void *optvals[7];
  float elapsed = 0.0;
#define LOGSIZE 8192
  char elog[LOGSIZE];
  char ilog[LOGSIZE];
  unsigned long logsize = LOGSIZE;
  CUlinkState linkstate;
  CUresult r;
  void *linkout;
  size_t linkoutsize __attribute__ ((unused));

  GOMP_PLUGIN_debug (0, "attempting to load:\n---\n%s\n---\n", ptx_code);

  opts[0] = CU_JIT_WALL_TIME;
  optvals[0] = &elapsed;

  opts[1] = CU_JIT_INFO_LOG_BUFFER;
  optvals[1] = &ilog[0];

  opts[2] = CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES;
  optvals[2] = (void *) logsize;

  opts[3] = CU_JIT_ERROR_LOG_BUFFER;
  optvals[3] = &elog[0];

  opts[4] = CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES;
  optvals[4] = (void *) logsize;

  opts[5] = CU_JIT_LOG_VERBOSE;
  optvals[5] = (void *) 1;

  opts[6] = CU_JIT_TARGET;
  optvals[6] = (void *) CU_TARGET_COMPUTE_30;

  r = cuLinkCreate (7, opts, optvals, &linkstate);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuLinkCreate error: %s", cuda_error (r));

  char *abort_ptx = ABORT_PTX;
  r = cuLinkAddData (linkstate, CU_JIT_INPUT_PTX, abort_ptx,
		     strlen (abort_ptx) + 1, 0, 0, 0, 0);
  if (r != CUDA_SUCCESS)
    {
      GOMP_PLUGIN_error ("Link error log %s\n", &elog[0]);
      GOMP_PLUGIN_fatal ("cuLinkAddData (abort) error: %s", cuda_error (r));
    }

  char *acc_on_device_ptx = ACC_ON_DEVICE_PTX;
  r = cuLinkAddData (linkstate, CU_JIT_INPUT_PTX, acc_on_device_ptx,
		     strlen (acc_on_device_ptx) + 1, 0, 0, 0, 0);
  if (r != CUDA_SUCCESS)
    {
      GOMP_PLUGIN_error ("Link error log %s\n", &elog[0]);
      GOMP_PLUGIN_fatal ("cuLinkAddData (acc_on_device) error: %s",
			 cuda_error (r));
    }

  char *goacc_internal_ptx = GOACC_INTERNAL_PTX;
  r = cuLinkAddData (linkstate, CU_JIT_INPUT_PTX, goacc_internal_ptx,
		     strlen (goacc_internal_ptx) + 1, 0, 0, 0, 0);
  if (r != CUDA_SUCCESS)
    {
      GOMP_PLUGIN_error ("Link error log %s\n", &elog[0]);
      GOMP_PLUGIN_fatal ("cuLinkAddData (goacc_internal_ptx) error: %s",
			 cuda_error (r));
    }

  r = cuLinkAddData (linkstate, CU_JIT_INPUT_PTX, ptx_code,
              strlen (ptx_code) + 1, 0, 0, 0, 0);
  if (r != CUDA_SUCCESS)
    {
      GOMP_PLUGIN_error ("Link error log %s\n", &elog[0]);
      GOMP_PLUGIN_fatal ("cuLinkAddData (ptx_code) error: %s", cuda_error (r));
    }

  r = cuLinkComplete (linkstate, &linkout, &linkoutsize);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuLinkComplete error: %s", cuda_error (r));

  GOMP_PLUGIN_debug (0, "Link complete: %fms\n", elapsed);
  GOMP_PLUGIN_debug (0, "Link log %s\n", &ilog[0]);

  r = cuModuleLoadData (module, linkout);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuModuleLoadData error: %s", cuda_error (r));
}

static void
event_gc (bool memmap_lockable)
{
  struct ptx_event *ptx_event = ptx_events;
  struct nvptx_thread *nvthd = nvptx_thread ();

  pthread_mutex_lock (&ptx_event_lock);

  while (ptx_event != NULL)
    {
      CUresult r;
      struct ptx_event *e = ptx_event;

      ptx_event = ptx_event->next;

      if (e->ord != nvthd->ptx_dev->ord)
	continue;

      r = cuEventQuery (*e->evt);
      if (r == CUDA_SUCCESS)
	{
	  CUevent *te;

	  te = e->evt;

	  switch (e->type)
	    {
	    case PTX_EVT_MEM:
	    case PTX_EVT_SYNC:
	      break;

	    case PTX_EVT_KNL:
	      map_pop (e->addr);
	      break;

	    case PTX_EVT_ASYNC_CLEANUP:
	      {
		/* The function gomp_plugin_async_unmap_vars needs to claim the
		   memory-map splay tree lock for the current device, so we
		   can't call it when one of our callers has already claimed
		   the lock.  In that case, just delay the GC for this event
		   until later.  */
		if (!memmap_lockable)
		  continue;

		GOMP_PLUGIN_async_unmap_vars (e->addr);
	      }
	      break;
	    }

	  cuEventDestroy (*te);
	  free ((void *)te);

	  if (ptx_events == e)
	    ptx_events = ptx_events->next;
	  else
	    {
	      struct ptx_event *e_ = ptx_events;
	      while (e_->next != e)
		e_ = e_->next;
	      e_->next = e_->next->next;
	    }

	  free (e);
	}
    }

  pthread_mutex_unlock (&ptx_event_lock);
}

static void
event_add (enum ptx_event_type type, CUevent *e, void *h)
{
  struct ptx_event *ptx_event;
  struct nvptx_thread *nvthd = nvptx_thread ();

  assert (type == PTX_EVT_MEM || type == PTX_EVT_KNL || type == PTX_EVT_SYNC
	  || type == PTX_EVT_ASYNC_CLEANUP);

  ptx_event = GOMP_PLUGIN_malloc (sizeof (struct ptx_event));
  ptx_event->type = type;
  ptx_event->evt = e;
  ptx_event->addr = h;
  ptx_event->ord = nvthd->ptx_dev->ord;

  pthread_mutex_lock (&ptx_event_lock);

  ptx_event->next = ptx_events;
  ptx_events = ptx_event;

  pthread_mutex_unlock (&ptx_event_lock);
}

void
nvptx_exec (void (*fn), size_t mapnum, void **hostaddrs, void **devaddrs,
	  size_t *sizes, unsigned short *kinds, int num_gangs, int num_workers,
	  int vector_length, int async, void *targ_mem_desc)
{
  struct targ_fn_descriptor *targ_fn = (struct targ_fn_descriptor *) fn;
  CUfunction function;
  CUresult r;
  int i;
  struct ptx_stream *dev_str;
  void *kargs[1];
  void *hp, *dp;
  unsigned int nthreads_in_block;
  struct nvptx_thread *nvthd = nvptx_thread ();
  const char *maybe_abort_msg = "(perhaps abort was called)";

  function = targ_fn->fn;

  dev_str = select_stream_for_async (async, pthread_self (), false, NULL);
  assert (dev_str == nvthd->current_stream);

  /* This reserves a chunk of a pre-allocated page of memory mapped on both
     the host and the device. HP is a host pointer to the new chunk, and DP is
     the corresponding device pointer.  */
  map_push (dev_str, async, mapnum * sizeof (void *), &hp, &dp);

  GOMP_PLUGIN_debug (0, "  %s: prepare mappings\n", __FUNCTION__);

  /* Copy the array of arguments to the mapped page.  */
  for (i = 0; i < mapnum; i++)
    ((void **) hp)[i] = devaddrs[i];

  /* Copy the (device) pointers to arguments to the device (dp and hp might in
     fact have the same value on a unified-memory system).  */
  r = cuMemcpy ((CUdeviceptr)dp, (CUdeviceptr)hp, mapnum * sizeof (void *));
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuMemcpy failed: %s", cuda_error (r));

  GOMP_PLUGIN_debug (0, "  %s: kernel %s: launch\n", __FUNCTION__, targ_fn->name);

  // OpenACC		CUDA
  //
  // num_gangs		blocks
  // num_workers	warps (where a warp is equivalent to 32 threads)
  // vector length	threads
  //

  /* The openacc vector_length clause 'determines the vector length to use for
     vector or SIMD operations'.  The question is how to map this to CUDA.

     In CUDA, the warp size is the vector length of a CUDA device.  However, the
     CUDA interface abstracts away from that, and only shows us warp size
     indirectly in maximum number of threads per block, which is a product of
     warp size and the number of hyperthreads of a multiprocessor.

     We choose to map openacc vector_length directly onto the number of threads
     in a block, in the x dimension.  This is reflected in gcc code generation
     that uses ThreadIdx.x to access vector elements.

     Attempting to use an openacc vector_length of more than the maximum number
     of threads per block will result in a cuda error.  */
  nthreads_in_block = vector_length;

  kargs[0] = &dp;
  r = cuLaunchKernel (function,
		      num_gangs, 1, 1,
		      nthreads_in_block, 1, 1,
		      0, dev_str->stream, kargs, 0);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuLaunchKernel error: %s", cuda_error (r));

#ifndef DISABLE_ASYNC
  if (async < acc_async_noval)
    {
      r = cuStreamSynchronize (dev_str->stream);
      if (r == CUDA_ERROR_LAUNCH_FAILED)
	GOMP_PLUGIN_fatal ("cuStreamSynchronize error: %s %s\n", cuda_error (r),
			   maybe_abort_msg);
      else if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuStreamSynchronize error: %s", cuda_error (r));
    }
  else
    {
      CUevent *e;

      e = (CUevent *)GOMP_PLUGIN_malloc (sizeof (CUevent));

      r = cuEventCreate (e, CU_EVENT_DISABLE_TIMING);
      if (r == CUDA_ERROR_LAUNCH_FAILED)
	GOMP_PLUGIN_fatal ("cuEventCreate error: %s %s\n", cuda_error (r),
			   maybe_abort_msg);
      else if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuEventCreate error: %s", cuda_error (r));

      event_gc (true);

      r = cuEventRecord (*e, dev_str->stream);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuEventRecord error: %s", cuda_error (r));

      event_add (PTX_EVT_KNL, e, (void *)dev_str);
    }
#else
  r = cuCtxSynchronize ();
  if (r == CUDA_ERROR_LAUNCH_FAILED)
    GOMP_PLUGIN_fatal ("cuCtxSynchronize error: %s %s\n", cuda_error (r),
		       maybe_abort_msg);
  else if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuCtxSynchronize error: %s", cuda_error (r));
#endif

  GOMP_PLUGIN_debug (0, "  %s: kernel %s: finished\n", __FUNCTION__,
		     targ_fn->name);

#ifndef DISABLE_ASYNC
  if (async < acc_async_noval)
#endif
    map_pop (dev_str);
}

void * openacc_get_current_cuda_context (void);

static void *
nvptx_alloc (size_t s)
{
  CUdeviceptr d;
  CUresult r;

  r = cuMemAlloc (&d, s);
  if (r == CUDA_ERROR_OUT_OF_MEMORY)
    return 0;
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuMemAlloc error: %s", cuda_error (r));
  return (void *)d;
}

static void
nvptx_free (void *p)
{
  CUresult r;
  CUdeviceptr pb;
  size_t ps;

  r = cuMemGetAddressRange (&pb, &ps, (CUdeviceptr)p);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuMemGetAddressRange error: %s", cuda_error (r));

  if ((CUdeviceptr)p != pb)
    GOMP_PLUGIN_fatal ("invalid device address");

  r = cuMemFree ((CUdeviceptr)p);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuMemFree error: %s", cuda_error (r));
}

static void *
nvptx_host2dev (void *d, const void *h, size_t s)
{
  CUresult r;
  CUdeviceptr pb;
  size_t ps;
  struct nvptx_thread *nvthd = nvptx_thread ();

  if (!s)
    return 0;

  if (!d)
    GOMP_PLUGIN_fatal ("invalid device address");

  r = cuMemGetAddressRange (&pb, &ps, (CUdeviceptr)d);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuMemGetAddressRange error: %s", cuda_error (r));

  if (!pb)
    GOMP_PLUGIN_fatal ("invalid device address");

  if (!h)
    GOMP_PLUGIN_fatal ("invalid host address");

  if (d == h)
    GOMP_PLUGIN_fatal ("invalid host or device address");

  if ((void *)(d + s) > (void *)(pb + ps))
    GOMP_PLUGIN_fatal ("invalid size");

#ifndef DISABLE_ASYNC
  if (nvthd->current_stream != nvthd->ptx_dev->null_stream)
    {
      CUevent *e;

      e = (CUevent *)GOMP_PLUGIN_malloc (sizeof (CUevent));

      r = cuEventCreate (e, CU_EVENT_DISABLE_TIMING);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuEventCreate error: %s", cuda_error (r));

      event_gc (false);

      r = cuMemcpyHtoDAsync ((CUdeviceptr)d, h, s,
			     nvthd->current_stream->stream);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuMemcpyHtoDAsync error: %s", cuda_error (r));

      r = cuEventRecord (*e, nvthd->current_stream->stream);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuEventRecord error: %s", cuda_error (r));

      event_add (PTX_EVT_MEM, e, (void *)h);
    }
  else
#endif
    {
      r = cuMemcpyHtoD ((CUdeviceptr)d, h, s);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuMemcpyHtoD error: %s", cuda_error (r));
    }

  return 0;
}

static void *
nvptx_dev2host (void *h, const void *d, size_t s)
{
  CUresult r;
  CUdeviceptr pb;
  size_t ps;
  struct nvptx_thread *nvthd = nvptx_thread ();

  if (!s)
    return 0;

  if (!d)
    GOMP_PLUGIN_fatal ("invalid device address");

  r = cuMemGetAddressRange (&pb, &ps, (CUdeviceptr)d);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuMemGetAddressRange error: %s", cuda_error (r));

  if (!pb)
    GOMP_PLUGIN_fatal ("invalid device address");

  if (!h)
    GOMP_PLUGIN_fatal ("invalid host address");

  if (d == h)
    GOMP_PLUGIN_fatal ("invalid host or device address");

  if ((void *)(d + s) > (void *)(pb + ps))
    GOMP_PLUGIN_fatal ("invalid size");

#ifndef DISABLE_ASYNC
  if (nvthd->current_stream != nvthd->ptx_dev->null_stream)
    {
      CUevent *e;

      e = (CUevent *)GOMP_PLUGIN_malloc (sizeof (CUevent));

      r = cuEventCreate (e, CU_EVENT_DISABLE_TIMING);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuEventCreate error: %s\n", cuda_error (r));

      event_gc (false);

      r = cuMemcpyDtoHAsync (h, (CUdeviceptr)d, s,
			     nvthd->current_stream->stream);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuMemcpyDtoHAsync error: %s", cuda_error (r));

      r = cuEventRecord (*e, nvthd->current_stream->stream);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuEventRecord error: %s", cuda_error (r));

      event_add (PTX_EVT_MEM, e, (void *)h);
    }
  else
#endif
    {
      r = cuMemcpyDtoH (h, (CUdeviceptr)d, s);
      if (r != CUDA_SUCCESS)
	GOMP_PLUGIN_fatal ("cuMemcpyDtoH error: %s", cuda_error (r));
    }

  return 0;
}

static void
nvptx_set_async (int async)
{
  struct nvptx_thread *nvthd = nvptx_thread ();
  nvthd->current_stream
    = select_stream_for_async (async, pthread_self (), true, NULL);
}

static int
nvptx_async_test (int async)
{
  CUresult r;
  struct ptx_stream *s;

  s = select_stream_for_async (async, pthread_self (), false, NULL);

  if (!s)
    GOMP_PLUGIN_fatal ("unknown async %d", async);

  r = cuStreamQuery (s->stream);
  if (r == CUDA_SUCCESS)
    {
      /* The oacc-parallel.c:goacc_wait function calls this hook to determine
	 whether all work has completed on this stream, and if so omits the call
	 to the wait hook.  If that happens, event_gc might not get called
	 (which prevents variables from getting unmapped and their associated
	 device storage freed), so call it here.  */
      event_gc (true);
      return 1;
    }
  else if (r == CUDA_ERROR_NOT_READY)
    return 0;

  GOMP_PLUGIN_fatal ("cuStreamQuery error: %s", cuda_error (r));

  return 0;
}

static int
nvptx_async_test_all (void)
{
  struct ptx_stream *s;
  pthread_t self = pthread_self ();
  struct nvptx_thread *nvthd = nvptx_thread ();

  pthread_mutex_lock (&nvthd->ptx_dev->stream_lock);

  for (s = nvthd->ptx_dev->active_streams; s != NULL; s = s->next)
    {
      if ((s->multithreaded || pthread_equal (s->host_thread, self))
	  && cuStreamQuery (s->stream) == CUDA_ERROR_NOT_READY)
	{
	  pthread_mutex_unlock (&nvthd->ptx_dev->stream_lock);
	  return 0;
	}
    }

  pthread_mutex_unlock (&nvthd->ptx_dev->stream_lock);

  event_gc (true);

  return 1;
}

static void
nvptx_wait (int async)
{
  CUresult r;
  struct ptx_stream *s;

  s = select_stream_for_async (async, pthread_self (), false, NULL);

  if (!s)
    GOMP_PLUGIN_fatal ("unknown async %d", async);

  r = cuStreamSynchronize (s->stream);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuStreamSynchronize error: %s", cuda_error (r));

  event_gc (true);
}

static void
nvptx_wait_async (int async1, int async2)
{
  CUresult r;
  CUevent *e;
  struct ptx_stream *s1, *s2;
  pthread_t self = pthread_self ();

  /* The stream that is waiting (rather than being waited for) doesn't
     necessarily have to exist already.  */
  s2 = select_stream_for_async (async2, self, true, NULL);

  s1 = select_stream_for_async (async1, self, false, NULL);
  if (!s1)
    GOMP_PLUGIN_fatal ("invalid async 1\n");

  if (s1 == s2)
    GOMP_PLUGIN_fatal ("identical parameters");

  e = (CUevent *)GOMP_PLUGIN_malloc (sizeof (CUevent));

  r = cuEventCreate (e, CU_EVENT_DISABLE_TIMING);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuEventCreate error: %s", cuda_error (r));

  event_gc (true);

  r = cuEventRecord (*e, s1->stream);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuEventRecord error: %s", cuda_error (r));

  event_add (PTX_EVT_SYNC, e, NULL);

  r = cuStreamWaitEvent (s2->stream, *e, 0);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuStreamWaitEvent error: %s", cuda_error (r));
}

static void
nvptx_wait_all (void)
{
  CUresult r;
  struct ptx_stream *s;
  pthread_t self = pthread_self ();
  struct nvptx_thread *nvthd = nvptx_thread ();

  pthread_mutex_lock (&nvthd->ptx_dev->stream_lock);

  /* Wait for active streams initiated by this thread (or by multiple threads)
     to complete.  */
  for (s = nvthd->ptx_dev->active_streams; s != NULL; s = s->next)
    {
      if (s->multithreaded || pthread_equal (s->host_thread, self))
	{
	  r = cuStreamQuery (s->stream);
	  if (r == CUDA_SUCCESS)
	    continue;
	  else if (r != CUDA_ERROR_NOT_READY)
	    GOMP_PLUGIN_fatal ("cuStreamQuery error: %s", cuda_error (r));

	  r = cuStreamSynchronize (s->stream);
	  if (r != CUDA_SUCCESS)
	    GOMP_PLUGIN_fatal ("cuStreamSynchronize error: %s", cuda_error (r));
	}
    }

  pthread_mutex_unlock (&nvthd->ptx_dev->stream_lock);

  event_gc (true);
}

static void
nvptx_wait_all_async (int async)
{
  CUresult r;
  struct ptx_stream *waiting_stream, *other_stream;
  CUevent *e;
  struct nvptx_thread *nvthd = nvptx_thread ();
  pthread_t self = pthread_self ();

  /* The stream doing the waiting.  This could be the first mention of the
     stream, so create it if necessary.  */
  waiting_stream
    = select_stream_for_async (async, pthread_self (), true, NULL);

  /* Launches on the null stream already block on other streams in the
     context.  */
  if (!waiting_stream || waiting_stream == nvthd->ptx_dev->null_stream)
    return;

  event_gc (true);

  pthread_mutex_lock (&nvthd->ptx_dev->stream_lock);

  for (other_stream = nvthd->ptx_dev->active_streams;
       other_stream != NULL;
       other_stream = other_stream->next)
    {
      if (!other_stream->multithreaded
	  && !pthread_equal (other_stream->host_thread, self))
	continue;

      e = (CUevent *) GOMP_PLUGIN_malloc (sizeof (CUevent));

      r = cuEventCreate (e, CU_EVENT_DISABLE_TIMING);
      if (r != CUDA_SUCCESS)
	GOMP_PLUGIN_fatal ("cuEventCreate error: %s", cuda_error (r));

      /* Record an event on the waited-for stream.  */
      r = cuEventRecord (*e, other_stream->stream);
      if (r != CUDA_SUCCESS)
	GOMP_PLUGIN_fatal ("cuEventRecord error: %s", cuda_error (r));

      event_add (PTX_EVT_SYNC, e, NULL);

      r = cuStreamWaitEvent (waiting_stream->stream, *e, 0);
      if (r != CUDA_SUCCESS)
	GOMP_PLUGIN_fatal ("cuStreamWaitEvent error: %s", cuda_error (r));
   }

  pthread_mutex_unlock (&nvthd->ptx_dev->stream_lock);
}

static void *
nvptx_get_current_cuda_device (void)
{
  struct nvptx_thread *nvthd = nvptx_thread ();

  if (!nvthd || !nvthd->ptx_dev)
    return NULL;

  return &nvthd->ptx_dev->dev;
}

static void *
nvptx_get_current_cuda_context (void)
{
  struct nvptx_thread *nvthd = nvptx_thread ();

  if (!nvthd || !nvthd->ptx_dev)
    return NULL;

  return nvthd->ptx_dev->ctx;
}

static void *
nvptx_get_cuda_stream (int async)
{
  struct ptx_stream *s;
  struct nvptx_thread *nvthd = nvptx_thread ();

  if (!nvthd || !nvthd->ptx_dev)
    return NULL;

  s = select_stream_for_async (async, pthread_self (), false, NULL);

  return s ? s->stream : NULL;
}

static int
nvptx_set_cuda_stream (int async, void *stream)
{
  struct ptx_stream *oldstream;
  pthread_t self = pthread_self ();
  struct nvptx_thread *nvthd = nvptx_thread ();

  pthread_mutex_lock (&nvthd->ptx_dev->stream_lock);

  if (async < 0)
    GOMP_PLUGIN_fatal ("bad async %d", async);

  /* We have a list of active streams and an array mapping async values to
     entries of that list.  We need to take "ownership" of the passed-in stream,
     and add it to our list, removing the previous entry also (if there was one)
     in order to prevent resource leaks.  Note the potential for surprise
     here: maybe we should keep track of passed-in streams and leave it up to
     the user to tidy those up, but that doesn't work for stream handles
     returned from acc_get_cuda_stream above...  */

  oldstream = select_stream_for_async (async, self, false, NULL);

  if (oldstream)
    {
      if (nvthd->ptx_dev->active_streams == oldstream)
	nvthd->ptx_dev->active_streams = nvthd->ptx_dev->active_streams->next;
      else
	{
	  struct ptx_stream *s = nvthd->ptx_dev->active_streams;
	  while (s->next != oldstream)
	    s = s->next;
	  s->next = s->next->next;
	}

      cuStreamDestroy (oldstream->stream);
      map_fini (oldstream);
      free (oldstream);
    }

  pthread_mutex_unlock (&nvthd->ptx_dev->stream_lock);

  (void) select_stream_for_async (async, self, true, (CUstream) stream);

  return 1;
}

/* Plugin entry points.  */

const char *
GOMP_OFFLOAD_get_name (void)
{
  return "nvptx";
}

unsigned int
GOMP_OFFLOAD_get_caps (void)
{
  return GOMP_OFFLOAD_CAP_OPENACC_200;
}

int
GOMP_OFFLOAD_get_type (void)
{
  return OFFLOAD_TARGET_TYPE_NVIDIA_PTX;
}

int
GOMP_OFFLOAD_get_num_devices (void)
{
  return nvptx_get_num_devices ();
}

void
GOMP_OFFLOAD_init_device (int n)
{
  pthread_mutex_lock (&ptx_dev_lock);

  if (!nvptx_init () || ptx_devices[n] != NULL)
    {
      pthread_mutex_unlock (&ptx_dev_lock);
      return;
    }

  ptx_devices[n] = nvptx_open_device (n);
  instantiated_devices++;

  pthread_mutex_unlock (&ptx_dev_lock);
}

void
GOMP_OFFLOAD_fini_device (int n)
{
  pthread_mutex_lock (&ptx_dev_lock);

  if (ptx_devices[n] != NULL)
    {
      nvptx_attach_host_thread_to_device (n);
      nvptx_close_device (ptx_devices[n]);
      ptx_devices[n] = NULL;
      instantiated_devices--;
    }

  pthread_mutex_unlock (&ptx_dev_lock);
}

int
GOMP_OFFLOAD_load_image (int ord, void *target_data,
			 struct addr_pair **target_table)
{
  CUmodule module;
  char **fn_names, **var_names;
  unsigned int fn_entries, var_entries, i, j;
  CUresult r;
  struct targ_fn_descriptor *targ_fns;
  void **img_header = (void **) target_data;
  struct ptx_image_data *new_image;

  GOMP_OFFLOAD_init_device (ord);

  nvptx_attach_host_thread_to_device (ord);

  link_ptx (&module, img_header[0]);

  pthread_mutex_lock (&ptx_image_lock);
  new_image = GOMP_PLUGIN_malloc (sizeof (struct ptx_image_data));
  new_image->target_data = target_data;
  new_image->module = module;
  new_image->next = ptx_images;
  ptx_images = new_image;
  pthread_mutex_unlock (&ptx_image_lock);

  /* The mkoffload utility emits a table of pointers/integers at the start of
     each offload image:

     img_header[0] -> ptx code
     img_header[1] -> number of variables
     img_header[2] -> array of variable names (pointers to strings)
     img_header[3] -> number of kernels
     img_header[4] -> array of kernel names (pointers to strings)

     The array of kernel names and the functions addresses form a
     one-to-one correspondence.  */

  var_entries = (uintptr_t) img_header[1];
  var_names = (char **) img_header[2];
  fn_entries = (uintptr_t) img_header[3];
  fn_names = (char **) img_header[4];

  *target_table = GOMP_PLUGIN_malloc (sizeof (struct addr_pair)
				      * (fn_entries + var_entries));
  targ_fns = GOMP_PLUGIN_malloc (sizeof (struct targ_fn_descriptor)
				 * fn_entries);

  for (i = 0; i < fn_entries; i++)
    {
      CUfunction function;

      r = cuModuleGetFunction (&function, module, fn_names[i]);
      if (r != CUDA_SUCCESS)
	GOMP_PLUGIN_fatal ("cuModuleGetFunction error: %s", cuda_error (r));

      targ_fns[i].fn = function;
      targ_fns[i].name = (const char *) fn_names[i];

      (*target_table)[i].start = (uintptr_t) &targ_fns[i];
      (*target_table)[i].end = (*target_table)[i].start + 1;
    }

  for (j = 0; j < var_entries; j++, i++)
    {
      CUdeviceptr var;
      size_t bytes;

      r = cuModuleGetGlobal (&var, &bytes, module, var_names[j]);
      if (r != CUDA_SUCCESS)
        GOMP_PLUGIN_fatal ("cuModuleGetGlobal error: %s", cuda_error (r));

      (*target_table)[i].start = (uintptr_t) var;
      (*target_table)[i].end = (*target_table)[i].start + bytes;
    }

  return i;
}

void
GOMP_OFFLOAD_unload_image (int tid __attribute__((unused)), void *target_data)
{
  void **img_header = (void **) target_data;
  struct targ_fn_descriptor *targ_fns
    = (struct targ_fn_descriptor *) img_header[0];
  struct ptx_image_data *image, *prev = NULL, *newhd = NULL;

  free (targ_fns);

  pthread_mutex_lock (&ptx_image_lock);
  for (image = ptx_images; image != NULL;)
    {
      struct ptx_image_data *next = image->next;

      if (image->target_data == target_data)
	{
	  cuModuleUnload (image->module);
	  free (image);
	  if (prev)
	    prev->next = next;
	}
      else
	{
	  prev = image;
	  if (!newhd)
	    newhd = image;
	}

      image = next;
    }
  ptx_images = newhd;
  pthread_mutex_unlock (&ptx_image_lock);
}

void *
GOMP_OFFLOAD_alloc (int ord, size_t size)
{
  nvptx_attach_host_thread_to_device (ord);
  return nvptx_alloc (size);
}

void
GOMP_OFFLOAD_free (int ord, void *ptr)
{
  nvptx_attach_host_thread_to_device (ord);
  nvptx_free (ptr);
}

void *
GOMP_OFFLOAD_dev2host (int ord, void *dst, const void *src, size_t n)
{
  nvptx_attach_host_thread_to_device (ord);
  return nvptx_dev2host (dst, src, n);
}

void *
GOMP_OFFLOAD_host2dev (int ord, void *dst, const void *src, size_t n)
{
  nvptx_attach_host_thread_to_device (ord);
  return nvptx_host2dev (dst, src, n);
}

void (*device_run) (int n, void *fn_ptr, void *vars) = NULL;

void
GOMP_OFFLOAD_openacc_parallel (void (*fn) (void *), size_t mapnum,
			       void **hostaddrs, void **devaddrs, size_t *sizes,
			       unsigned short *kinds, int num_gangs,
			       int num_workers, int vector_length, int async,
			       void *targ_mem_desc)
{
  nvptx_exec (fn, mapnum, hostaddrs, devaddrs, sizes, kinds, num_gangs,
	    num_workers, vector_length, async, targ_mem_desc);
}

void
GOMP_OFFLOAD_openacc_register_async_cleanup (void *targ_mem_desc)
{
  CUevent *e;
  CUresult r;
  struct nvptx_thread *nvthd = nvptx_thread ();

  e = (CUevent *) GOMP_PLUGIN_malloc (sizeof (CUevent));

  r = cuEventCreate (e, CU_EVENT_DISABLE_TIMING);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuEventCreate error: %s", cuda_error (r));

  r = cuEventRecord (*e, nvthd->current_stream->stream);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuEventRecord error: %s", cuda_error (r));

  event_add (PTX_EVT_ASYNC_CLEANUP, e, targ_mem_desc);
}

int
GOMP_OFFLOAD_openacc_async_test (int async)
{
  return nvptx_async_test (async);
}

int
GOMP_OFFLOAD_openacc_async_test_all (void)
{
  return nvptx_async_test_all ();
}

void
GOMP_OFFLOAD_openacc_async_wait (int async)
{
  nvptx_wait (async);
}

void
GOMP_OFFLOAD_openacc_async_wait_async (int async1, int async2)
{
  nvptx_wait_async (async1, async2);
}

void
GOMP_OFFLOAD_openacc_async_wait_all (void)
{
  nvptx_wait_all ();
}

void
GOMP_OFFLOAD_openacc_async_wait_all_async (int async)
{
  nvptx_wait_all_async (async);
}

void
GOMP_OFFLOAD_openacc_async_set_async (int async)
{
  nvptx_set_async (async);
}

void *
GOMP_OFFLOAD_openacc_create_thread_data (int ord)
{
  struct ptx_device *ptx_dev;
  struct nvptx_thread *nvthd
    = GOMP_PLUGIN_malloc (sizeof (struct nvptx_thread));
  CUresult r;
  CUcontext thd_ctx;

  ptx_dev = ptx_devices[ord];

  assert (ptx_dev);

  r = cuCtxGetCurrent (&thd_ctx);
  if (r != CUDA_SUCCESS)
    GOMP_PLUGIN_fatal ("cuCtxGetCurrent error: %s", cuda_error (r));

  assert (ptx_dev->ctx);

  if (!thd_ctx)
    {
      r = cuCtxPushCurrent (ptx_dev->ctx);
      if (r != CUDA_SUCCESS)
	GOMP_PLUGIN_fatal ("cuCtxPushCurrent error: %s", cuda_error (r));
    }

  nvthd->current_stream = ptx_dev->null_stream;
  nvthd->ptx_dev = ptx_dev;

  return (void *) nvthd;
}

void
GOMP_OFFLOAD_openacc_destroy_thread_data (void *data)
{
  free (data);
}

void *
GOMP_OFFLOAD_openacc_get_current_cuda_device (void)
{
  return nvptx_get_current_cuda_device ();
}

void *
GOMP_OFFLOAD_openacc_get_current_cuda_context (void)
{
  return nvptx_get_current_cuda_context ();
}

/* NOTE: This returns a CUstream, not a ptx_stream pointer.  */

void *
GOMP_OFFLOAD_openacc_get_cuda_stream (int async)
{
  return nvptx_get_cuda_stream (async);
}

/* NOTE: This takes a CUstream, not a ptx_stream pointer.  */

int
GOMP_OFFLOAD_openacc_set_cuda_stream (int async, void *stream)
{
  return nvptx_set_cuda_stream (async, stream);
}

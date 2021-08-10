/* Test dispatch of events to callbacks.  */

#undef NDEBUG
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <acc_prof.h>


/* Use explicit 'copyin' clauses, to work around "'firstprivate'
   optimizations", which will cause the value at the point of call to be used
   (*before* any potential modifications done in callbacks), as opposed to its
   address being taken, which then later gets dereferenced (*after* any
   modifications done in callbacks).  */
#define COPYIN(...) copyin(__VA_ARGS__)


/* See the 'DEVICE_INIT_INSIDE_COMPUTE_CONSTRUCT' reference in
   'libgomp.texi'.  */
#define DEVICE_INIT_INSIDE_COMPUTE_CONSTRUCT 0


#define DEBUG_printf(...) //__builtin_printf (__VA_ARGS__)


volatile // TODO PR90488
static int state = -1;

#define STATE_OP(state, op) \
  do \
    { \
      typeof (state) state_o = (state); \
      (void) state_o; \
      (state)op; \
      DEBUG_printf("state: %d -> %d\n", state_o, (state)); \
    } \
  while (0)


static acc_device_t acc_device_type;
static int acc_device_num;
static int num_gangs, num_workers, vector_length;


static void cb_enqueue_launch_start (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (acc_device_type != acc_device_host);

  assert (state == 0);
  STATE_OP (state, = 1);

  assert (prof_info->event_type == acc_ev_enqueue_launch_start);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_type);
  assert (prof_info->device_number == acc_device_num);
  assert (prof_info->thread_id == -1);
  assert (prof_info->async == acc_async_sync);
  assert (prof_info->async_queue == prof_info->async);
  assert (prof_info->src_file == NULL);
  assert (prof_info->func_name == NULL);
  assert (prof_info->line_no == -1);
  assert (prof_info->end_line_no == -1);
  assert (prof_info->func_line_no == -1);
  assert (prof_info->func_end_line_no == -1);

  assert (event_info->launch_event.event_type == prof_info->event_type);
  assert (event_info->launch_event.valid_bytes == _ACC_LAUNCH_EVENT_INFO_VALID_BYTES);
  assert (event_info->launch_event.parent_construct == acc_construct_parallel);
  assert (event_info->launch_event.implicit == 1);
  assert (event_info->launch_event.tool_info == NULL);
  assert (event_info->launch_event.kernel_name != NULL);
  {
    const char *s = strstr (event_info->launch_event.kernel_name, "main");
    assert (s != NULL);
    s = strstr (s, "omp_fn");
    assert (s != NULL);
  }
  if (num_gangs < 1)
    assert (event_info->launch_event.num_gangs >= 1);
  else
    {
#ifdef __OPTIMIZE__
      assert (event_info->launch_event.num_gangs == num_gangs);
#else
      /* No parallelized OpenACC 'kernels' constructs.  Unparallelized OpenACC
	 'kernels' constructs must get launched as 1 x 1 x 1 GPU kernels.  */
      assert (event_info->launch_event.num_gangs == 1);
#endif
    }
  if (num_workers < 1)
    assert (event_info->launch_event.num_workers >= 1);
  else
    {
#ifdef __OPTIMIZE__
      assert (event_info->launch_event.num_workers == num_workers);
#else
      /* See 'num_gangs' above.  */
      assert (event_info->launch_event.num_workers == 1);
#endif
    }
  if (vector_length < 1)
    assert (event_info->launch_event.vector_length >= 1);
  else if (acc_device_type == acc_device_nvidia) /* ... is special.  */
    assert (event_info->launch_event.vector_length == 32);
  else if (acc_device_type == acc_device_radeon) /* ...and so is this.  */
    assert (event_info->launch_event.vector_length == 64);
  else
    {
#ifdef __OPTIMIZE__
      assert (event_info->launch_event.vector_length == vector_length);
#else
      /* See 'num_gangs' above.  */
      assert (event_info->launch_event.vector_length == 1);
#endif
    }

  if (acc_device_type == acc_device_host)
    assert (api_info->device_api == acc_device_api_none);
  else if (acc_device_type == acc_device_radeon)
    assert (api_info->device_api == acc_device_api_other);
  else
    assert (api_info->device_api == acc_device_api_cuda);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);
}


static acc_prof_reg reg;
static acc_prof_reg unreg;
static acc_prof_lookup_func lookup;
void acc_register_library (acc_prof_reg reg_, acc_prof_reg unreg_, acc_prof_lookup_func lookup_)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  reg = reg_;
  unreg = unreg_;
  lookup = lookup_;
}


int main()
{
  acc_register_library (acc_prof_register, acc_prof_unregister, acc_prof_lookup);

  STATE_OP (state, = 0);
  reg (acc_ev_enqueue_launch_start, cb_enqueue_launch_start, acc_reg);
  assert (state == 0);

  acc_device_type = acc_get_device_type ();
  acc_device_num = acc_get_device_num (acc_device_type);
  assert (state == 0);

  /* Parallelism dimensions: compiler/runtime decides.  */
  STATE_OP (state, = 0);
  num_gangs = num_workers = vector_length = 0;
  {
#define N 100
    int x[N];
#pragma acc kernels
    {
      for (int i = 0; i < N; ++i)
	x[i] = i * i;
    }
    if (acc_device_type == acc_device_host)
      assert (state == 0); /* No 'acc_ev_enqueue_launch_start'.  */
    else
      assert (state == 1);
    for (int i = 0; i < N; ++i)
      if (x[i] != i * i)
	__builtin_abort ();
#undef N
  }

  /* Parallelism dimensions: literal.  */
  STATE_OP (state, = 0);
  num_gangs = 30;
  num_workers = 3;
  vector_length = 5;
  {
#define N 100
    int x[N];
#pragma acc kernels \
  num_gangs (30) num_workers (3) vector_length (5)
    /* { dg-prune-output "using vector_length \\(32\\), ignoring 5" } */
    {
      for (int i = 0; i < N; ++i)
	x[i] = i * i;
    }
    if (acc_device_type == acc_device_host)
      assert (state == 0); /* No 'acc_ev_enqueue_launch_start'.  */
    else
      assert (state == 1);
    for (int i = 0; i < N; ++i)
      if (x[i] != i * i)
	__builtin_abort ();
#undef N
  }

  /* Parallelism dimensions: variable.  */
  STATE_OP (state, = 0);
  num_gangs = 22;
  num_workers = 5;
  vector_length = 7;
  {
#define N 100
    int x[N];
#pragma acc kernels \
  num_gangs (num_gangs) num_workers (num_workers) vector_length (vector_length)
    /* { dg-prune-output "using vector_length \\(32\\), ignoring runtime setting" } */
    {
      for (int i = 0; i < N; ++i)
	x[i] = i * i;
    }
    if (acc_device_type == acc_device_host)
      assert (state == 0); /* No 'acc_ev_enqueue_launch_start'.  */
    else
      assert (state == 1);
    for (int i = 0; i < N; ++i)
      if (x[i] != i * i)
	__builtin_abort ();
#undef N
  }

  return 0;
}

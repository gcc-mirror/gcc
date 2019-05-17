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
   libgomp.texi.  */
#define DEVICE_INIT_INSIDE_COMPUTE_CONSTRUCT 0


/* Do we expect to see 'acc_ev_exit_data_start' and 'acc_ev_exit_data_end'
   after a compute construct with an 'async' clause?  */
#define ASYNC_EXIT_DATA 1


#define DEBUG_printf(...) //__builtin_printf (__VA_ARGS__)


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
static int acc_async;


struct tool_info
{
  acc_event_info event_info;
  struct tool_info *nested;
};
struct tool_info *tool_info;

static void cb_device_init_start (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

#if DEVICE_INIT_INSIDE_COMPUTE_CONSTRUCT
  assert (state == 1
	  || state == 101);
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_compute_construct_start);
  assert (tool_info->nested == NULL);
  tool_info->nested = (struct tool_info *) malloc(sizeof *tool_info);
  assert (tool_info->nested != NULL);
  tool_info->nested->nested = NULL;
#else
  assert (state == 0
	  || state == 100);
  STATE_OP (state, ++);

  assert (tool_info == NULL);
  tool_info = (struct tool_info *) malloc(sizeof *tool_info);
  assert (tool_info != NULL);
  tool_info->nested = NULL;
#endif

  assert (prof_info->event_type == acc_ev_device_init_start);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_default);
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

  assert (event_info->other_event.event_type == prof_info->event_type);
  assert (event_info->other_event.valid_bytes == _ACC_OTHER_EVENT_INFO_VALID_BYTES);
  assert (event_info->other_event.parent_construct == acc_construct_parallel);
  assert (event_info->other_event.implicit == 1);
  assert (event_info->other_event.tool_info == NULL);

  assert (api_info->device_api == acc_device_api_none);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

#if DEVICE_INIT_INSIDE_COMPUTE_CONSTRUCT
  tool_info->nested->event_info.other_event.event_type = event_info->other_event.event_type;
  event_info->other_event.tool_info = tool_info->nested;
#else
  tool_info->event_info.other_event.event_type = event_info->other_event.event_type;
  event_info->other_event.tool_info = tool_info;
#endif
}

static void cb_device_init_end (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

#if DEVICE_INIT_INSIDE_COMPUTE_CONSTRUCT
  assert (state == 2
	  || state == 102);
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_compute_construct_start);
  assert (tool_info->nested != NULL);
  assert (tool_info->nested->event_info.other_event.event_type == acc_ev_device_init_start);
#else
  assert (state == 1
	  || state == 101);
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_device_init_start);
#endif

  assert (prof_info->event_type == acc_ev_device_init_end);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_default);
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

  assert (event_info->other_event.event_type == prof_info->event_type);
  assert (event_info->other_event.valid_bytes == _ACC_OTHER_EVENT_INFO_VALID_BYTES);
  assert (event_info->other_event.parent_construct == acc_construct_parallel);
  assert (event_info->other_event.implicit == 1);
#if DEVICE_INIT_INSIDE_COMPUTE_CONSTRUCT
  assert (event_info->other_event.tool_info == tool_info->nested);
#else
  assert (event_info->other_event.tool_info == tool_info);
#endif

  assert (api_info->device_api == acc_device_api_none);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

#if DEVICE_INIT_INSIDE_COMPUTE_CONSTRUCT
  free (tool_info->nested);
  tool_info->nested = NULL;
#else
  free (tool_info);
  tool_info = NULL;
#endif
}

static void cb_enter_data_start (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 3
	  || state == 103);
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_compute_construct_start);
  assert (tool_info->nested == NULL);
  tool_info->nested = (struct tool_info *) malloc(sizeof *tool_info);
  assert (tool_info->nested != NULL);
  tool_info->nested->nested = NULL;

  assert (prof_info->event_type == acc_ev_enter_data_start);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_type);
  assert (prof_info->device_number == acc_device_num);
  assert (prof_info->thread_id == -1);
  assert (prof_info->async == acc_async);
  assert (prof_info->async_queue == prof_info->async);
  assert (prof_info->src_file == NULL);
  assert (prof_info->func_name == NULL);
  assert (prof_info->line_no == -1);
  assert (prof_info->end_line_no == -1);
  assert (prof_info->func_line_no == -1);
  assert (prof_info->func_end_line_no == -1);

  assert (event_info->other_event.event_type == prof_info->event_type);
  assert (event_info->other_event.valid_bytes == _ACC_OTHER_EVENT_INFO_VALID_BYTES);
  assert (event_info->other_event.parent_construct == acc_construct_parallel);
  assert (event_info->other_event.implicit == 1);
  assert (event_info->other_event.tool_info == NULL);

  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

  tool_info->nested->event_info.other_event.event_type = event_info->other_event.event_type;
  event_info->other_event.tool_info = tool_info->nested;
}

static void cb_enter_data_end (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 4
	  || state == 104);
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_compute_construct_start);
  assert (tool_info->nested != NULL);
  assert (tool_info->nested->event_info.other_event.event_type == acc_ev_enter_data_start);

  assert (prof_info->event_type == acc_ev_enter_data_end);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_type);
  assert (prof_info->device_number == acc_device_num);
  assert (prof_info->thread_id == -1);
  assert (prof_info->async == acc_async);
  assert (prof_info->async_queue == prof_info->async);
  assert (prof_info->src_file == NULL);
  assert (prof_info->func_name == NULL);
  assert (prof_info->line_no == -1);
  assert (prof_info->end_line_no == -1);
  assert (prof_info->func_line_no == -1);
  assert (prof_info->func_end_line_no == -1);

  assert (event_info->other_event.event_type == prof_info->event_type);
  assert (event_info->other_event.valid_bytes == _ACC_OTHER_EVENT_INFO_VALID_BYTES);
  assert (event_info->other_event.parent_construct == acc_construct_parallel);
  assert (event_info->other_event.implicit == 1);
  assert (event_info->other_event.tool_info == tool_info->nested);

  if (acc_device_type == acc_device_host)
    assert (api_info->device_api == acc_device_api_none);
  else
    assert (api_info->device_api == acc_device_api_cuda);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

  free (tool_info->nested);
  tool_info->nested = NULL;
}

static void cb_exit_data_start (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 7
#if ASYNC_EXIT_DATA
	  || state == 107
#endif
	  );
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_compute_construct_start);
  assert (tool_info->nested == NULL);
  tool_info->nested = (struct tool_info *) malloc(sizeof *tool_info);
  assert (tool_info->nested != NULL);
  tool_info->nested->nested = NULL;

  assert (prof_info->event_type == acc_ev_exit_data_start);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_type);
  assert (prof_info->device_number == acc_device_num);
  assert (prof_info->thread_id == -1);
  assert (prof_info->async == acc_async);
  assert (prof_info->async_queue == prof_info->async);
  assert (prof_info->src_file == NULL);
  assert (prof_info->func_name == NULL);
  assert (prof_info->line_no == -1);
  assert (prof_info->end_line_no == -1);
  assert (prof_info->func_line_no == -1);
  assert (prof_info->func_end_line_no == -1);

  assert (event_info->other_event.event_type == prof_info->event_type);
  assert (event_info->other_event.valid_bytes == _ACC_OTHER_EVENT_INFO_VALID_BYTES);
  assert (event_info->other_event.parent_construct == acc_construct_parallel);
  assert (event_info->other_event.implicit == 1);
  assert (event_info->other_event.tool_info == NULL);

  if (acc_device_type == acc_device_host)
    assert (api_info->device_api == acc_device_api_none);
  else
    assert (api_info->device_api == acc_device_api_cuda);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

  tool_info->nested->event_info.other_event.event_type = event_info->other_event.event_type;
  event_info->other_event.tool_info = tool_info->nested;
}

static void cb_exit_data_end (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 8
#if ASYNC_EXIT_DATA
	  || state == 108
#endif
	  );
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_compute_construct_start);
  assert (tool_info->nested != NULL);
  assert (tool_info->nested->event_info.other_event.event_type == acc_ev_exit_data_start);

  assert (prof_info->event_type == acc_ev_exit_data_end);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_type);
  assert (prof_info->device_number == acc_device_num);
  assert (prof_info->thread_id == -1);
  assert (prof_info->async == acc_async);
  assert (prof_info->async_queue == prof_info->async);
  assert (prof_info->src_file == NULL);
  assert (prof_info->func_name == NULL);
  assert (prof_info->line_no == -1);
  assert (prof_info->end_line_no == -1);
  assert (prof_info->func_line_no == -1);
  assert (prof_info->func_end_line_no == -1);

  assert (event_info->other_event.event_type == prof_info->event_type);
  assert (event_info->other_event.valid_bytes == _ACC_OTHER_EVENT_INFO_VALID_BYTES);
  assert (event_info->other_event.parent_construct == acc_construct_parallel);
  assert (event_info->other_event.implicit == 1);
  assert (event_info->other_event.tool_info == tool_info->nested);

  if (acc_device_type == acc_device_host)
    assert (api_info->device_api == acc_device_api_none);
  else
    assert (api_info->device_api == acc_device_api_cuda);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

  free (tool_info->nested);
  tool_info->nested = NULL;
}

static void cb_compute_construct_start (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

#if DEVICE_INIT_INSIDE_COMPUTE_CONSTRUCT
  assert (state == 0
	  || state == 100);
  if (state == 100)
    {
      /* Compensate for the missing 'acc_ev_device_init_start' and
	 'acc_ev_device_init_end'.  */
      state += 2;
    }
#else
  if (state == 100)
    {
      /* Compensate for the missing 'acc_ev_device_init_start' and
	 'acc_ev_device_init_end'.  */
      state += 2;
    }
  assert (state == 2
	  || state == 102);
#endif
  STATE_OP (state, ++);

  assert (tool_info == NULL);
  tool_info = (struct tool_info *) malloc(sizeof *tool_info);
  assert (tool_info != NULL);
  tool_info->nested = NULL;

  assert (prof_info->event_type == acc_ev_compute_construct_start);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_type);
  assert (prof_info->device_number == acc_device_num);
  assert (prof_info->thread_id == -1);
  assert (prof_info->async == /* TODO acc_async */ acc_async_sync);
  assert (prof_info->async_queue == prof_info->async);
  assert (prof_info->src_file == NULL);
  assert (prof_info->func_name == NULL);
  assert (prof_info->line_no == -1);
  assert (prof_info->end_line_no == -1);
  assert (prof_info->func_line_no == -1);
  assert (prof_info->func_end_line_no == -1);

  assert (event_info->other_event.event_type == prof_info->event_type);
  assert (event_info->other_event.valid_bytes == _ACC_OTHER_EVENT_INFO_VALID_BYTES);
  assert (event_info->other_event.parent_construct == acc_construct_parallel);
  assert (event_info->other_event.implicit == 0);
  assert (event_info->other_event.tool_info == NULL);

  assert (api_info->device_api == acc_device_api_none);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

  tool_info->event_info.other_event.event_type = event_info->other_event.event_type;
  event_info->other_event.tool_info = tool_info;

  if (acc_device_type == acc_device_host)
    {
      /* Compensate for the missing 'acc_ev_enter_data_start'.  */
      state += 1;
    }
}

static void cb_compute_construct_end (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  if (acc_device_type == acc_device_host)
    {
      /* Compensate for the missing 'acc_ev_enter_data_end'.  */
      state += 1;
      /* Compensate for the missing 'acc_ev_enqueue_launch_start' and
	 'acc_ev_enqueue_launch_end'.  */
      state += 2;
      /* Compensate for the missing 'acc_ev_exit_data_start' and
	 'acc_ev_exit_data_end'.  */
      state += 2;
    }
#if !ASYNC_EXIT_DATA
  else if (acc_async != acc_async_sync)
    {
      /* Compensate for the missing 'acc_ev_exit_data_start' and
	 'acc_ev_exit_data_end'.  */
      state += 2;
    }
#endif
  assert (state == 9
	  || state == 109);
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_compute_construct_start);
  assert (tool_info->nested == NULL);

  assert (prof_info->event_type == acc_ev_compute_construct_end);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_type);
  assert (prof_info->device_number == acc_device_num);
  assert (prof_info->thread_id == -1);
  if (acc_device_type == acc_device_host)
    assert (prof_info->async == acc_async_sync);
  else
    assert (prof_info->async == acc_async);
  assert (prof_info->async_queue == prof_info->async);
  assert (prof_info->src_file == NULL);
  assert (prof_info->func_name == NULL);
  assert (prof_info->line_no == -1);
  assert (prof_info->end_line_no == -1);
  assert (prof_info->func_line_no == -1);
  assert (prof_info->func_end_line_no == -1);

  assert (event_info->other_event.event_type == prof_info->event_type);
  assert (event_info->other_event.valid_bytes == _ACC_OTHER_EVENT_INFO_VALID_BYTES);
  assert (event_info->other_event.parent_construct == acc_construct_parallel);
  assert (event_info->other_event.implicit == 0);
  assert (event_info->other_event.tool_info == tool_info);

  if (acc_device_type == acc_device_host)
    assert (api_info->device_api == acc_device_api_none);
  else
    assert (api_info->device_api == acc_device_api_cuda);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

  free (tool_info);
  tool_info = NULL;
}

static void cb_enqueue_launch_start (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (acc_device_type != acc_device_host);

  assert (state == 5
	  || state == 105);
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_compute_construct_start);
  assert (tool_info->nested == NULL);
  tool_info->nested = (struct tool_info *) malloc(sizeof *tool_info);
  assert (tool_info->nested != NULL);
  tool_info->nested->nested = NULL;

  assert (prof_info->event_type == acc_ev_enqueue_launch_start);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_type);
  assert (prof_info->device_number == acc_device_num);
  assert (prof_info->thread_id == -1);
  assert (prof_info->async == acc_async);
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
  assert (event_info->launch_event.num_gangs >= 1);
  assert (event_info->launch_event.num_workers >= 1);
  assert (event_info->launch_event.vector_length >= 1);

  if (acc_device_type == acc_device_host)
    assert (api_info->device_api == acc_device_api_none);
  else
    assert (api_info->device_api == acc_device_api_cuda);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

  tool_info->nested->event_info.launch_event.event_type = event_info->launch_event.event_type;
  tool_info->nested->event_info.launch_event.kernel_name = strdup (event_info->launch_event.kernel_name);
  tool_info->nested->event_info.launch_event.num_gangs = event_info->launch_event.num_gangs;
  tool_info->nested->event_info.launch_event.num_workers = event_info->launch_event.num_workers;
  tool_info->nested->event_info.launch_event.vector_length = event_info->launch_event.vector_length;
  event_info->other_event.tool_info = tool_info->nested;
}

static void cb_enqueue_launch_end (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (acc_device_type != acc_device_host);

  assert (state == 6
	  || state == 106);
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_compute_construct_start);
  assert (tool_info->nested != NULL);
  assert (tool_info->nested->event_info.launch_event.event_type == acc_ev_enqueue_launch_start);
  assert (tool_info->nested->event_info.launch_event.kernel_name != NULL);
  assert (tool_info->nested->event_info.launch_event.num_gangs >= 1);
  assert (tool_info->nested->event_info.launch_event.num_workers >= 1);
  assert (tool_info->nested->event_info.launch_event.vector_length >= 1);

  assert (prof_info->event_type == acc_ev_enqueue_launch_end);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  assert (prof_info->device_type == acc_device_type);
  assert (prof_info->device_number == acc_device_num);
  assert (prof_info->thread_id == -1);
  assert (prof_info->async == acc_async);
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
  assert (event_info->launch_event.tool_info == tool_info->nested);
  assert (event_info->launch_event.kernel_name != NULL);
  assert (strcmp (event_info->launch_event.kernel_name, tool_info->nested->event_info.launch_event.kernel_name) == 0);
  assert (event_info->launch_event.num_gangs == tool_info->nested->event_info.launch_event.num_gangs);
  assert (event_info->launch_event.num_workers == tool_info->nested->event_info.launch_event.num_workers);
  assert (event_info->launch_event.vector_length == tool_info->nested->event_info.launch_event.vector_length);

  if (acc_device_type == acc_device_host)
    assert (api_info->device_api == acc_device_api_none);
  else
    assert (api_info->device_api == acc_device_api_cuda);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

  free ((void *) tool_info->nested->event_info.launch_event.kernel_name);
  free (tool_info->nested);
  tool_info->nested = NULL;
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
  reg (acc_ev_device_init_start, cb_device_init_start, acc_reg);
  reg (acc_ev_device_init_end, cb_device_init_end, acc_reg);
  reg (acc_ev_enter_data_start, cb_enter_data_start, acc_reg);
  reg (acc_ev_enter_data_end, cb_enter_data_end, acc_reg);
  reg (acc_ev_exit_data_start, cb_exit_data_start, acc_reg);
  reg (acc_ev_exit_data_end, cb_exit_data_end, acc_reg);
  reg (acc_ev_compute_construct_start, cb_compute_construct_start, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end, acc_reg);
  reg (acc_ev_enqueue_launch_start, cb_enqueue_launch_start, acc_reg);
  reg (acc_ev_enqueue_launch_end, cb_enqueue_launch_end, acc_reg);
  assert (state == 0);

  acc_device_type = acc_get_device_type ();
  acc_device_num = acc_get_device_num (acc_device_type);
  acc_async = acc_async_sync;
  assert (state == 0);

  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      asm volatile ("" : : : "memory"); // TODO PR90488

      state_init = state;
    }
    assert (state_init == 4);
  }
  assert (state == 10);

  STATE_OP (state, = 100);

  acc_async = 12;
  {
    int state_init;
#pragma acc parallel async(acc_async) COPYIN(state) copyout(state_init)
    {
      asm volatile ("" : : : "memory"); // TODO PR90488

      state_init = state;
    }
#pragma acc wait
    assert (state_init == 104);
  }
  assert (state == 110);

  return 0;
}

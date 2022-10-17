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


#define DEBUG_printf(...) //__builtin_printf (__VA_ARGS__)


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
static int acc_async = acc_async_sync;


struct tool_info
{
  acc_event_info event_info;
  struct tool_info *nested;
};
struct tool_info *tool_info;

static void cb_device_init_start (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 0
	  || state == 100);
  STATE_OP (state, ++);

  assert (tool_info == NULL);
  tool_info = (struct tool_info *) malloc(sizeof *tool_info);
  assert (tool_info != NULL);
  tool_info->nested = NULL;

  assert (prof_info->event_type == acc_ev_device_init_start);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  if (state == 1)
    assert (prof_info->device_type == acc_device_host);
  else
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
  assert (event_info->other_event.parent_construct == acc_construct_runtime_api);
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
}

static void cb_device_init_end (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 1
	  || state == 101);
  STATE_OP (state, ++);

  assert (tool_info != NULL);
  assert (tool_info->event_info.other_event.event_type == acc_ev_device_init_start);

  assert (prof_info->event_type == acc_ev_device_init_end);
  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (prof_info->version == _ACC_PROF_INFO_VERSION);
  if (state == 2)
    assert (prof_info->device_type == acc_device_host);
  else
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
  assert (event_info->other_event.parent_construct == acc_construct_runtime_api);
  assert (event_info->other_event.implicit == 0);
  assert (event_info->other_event.tool_info == tool_info);

  assert (api_info->device_api == acc_device_api_none);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);
  assert (api_info->device_type == prof_info->device_type);
  assert (api_info->vendor == -1);
  assert (api_info->device_handle == NULL);
  assert (api_info->context_handle == NULL);
  assert (api_info->async_handle == NULL);

  free (tool_info);
  tool_info = NULL;
}

static void cb_compute_construct_start (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 10
	  || state == 110);
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
}

static void cb_compute_construct_end (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 11
	  || state == 111);
#if defined COPYIN
  /* In an 'async' setting, this event may be triggered before actual 'async'
     data copying has completed.  Given that 'state' appears in 'COPYIN', we
     first have to synchronize (that is, let the 'async' 'COPYIN' read the
     current 'state' value)...  */
  if (acc_async != acc_async_sync)
    {
      /* "We're not yet accounting for the fact that _OpenACC events may occur
	 during event processing_"; temporarily disable to avoid deadlock.  */
      unreg (acc_ev_none, NULL, acc_toggle_per_thread);
      acc_wait (acc_async);
      reg (acc_ev_none, NULL, acc_toggle_per_thread);
    }
  /* ... before modifying it in the following.  */
#endif
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

  free (tool_info);
  tool_info = NULL;
}


int main()
{
  acc_register_library (acc_prof_register, acc_prof_unregister, acc_prof_lookup);

  STATE_OP (state, = 0);
  reg (acc_ev_device_init_start, cb_device_init_start, acc_reg);
  reg (acc_ev_device_init_end, cb_device_init_end, acc_reg);
  reg (acc_ev_compute_construct_start, cb_compute_construct_start, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end, acc_reg);
  assert (state == 0);

  acc_init (acc_device_host);
  assert (state == 2);

  STATE_OP (state, = 10);

  acc_device_type = acc_get_device_type ();
  acc_device_num = acc_get_device_num (acc_device_type);

  {
    int state_init;
    acc_async = 12;
#pragma acc parallel async(acc_async) COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    acc_async = acc_async_sync;
#pragma acc wait
    assert (state_init == 11);
  }
  assert (state == 12);

  STATE_OP (state, = 90);
  acc_shutdown (acc_device_host);
  assert (state == 90);


  STATE_OP (state, = 100);
  acc_init (acc_device_default);
  assert (state == 102);

  STATE_OP (state, = 110);

  acc_device_type = acc_get_device_type ();
  acc_device_num = acc_get_device_num (acc_device_type);

  {
    int state_init;
    acc_async = 12;
#pragma acc parallel async(acc_async) COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    acc_async = acc_async_sync;
#pragma acc wait
    assert (state_init == 111);
  }
  assert (state == 112);

  STATE_OP (state, = 190);
  acc_shutdown (acc_device_default);
  assert (state == 190);

  return 0;
}

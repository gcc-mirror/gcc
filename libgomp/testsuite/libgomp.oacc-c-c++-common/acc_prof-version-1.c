/* Test the 'version' field of 'acc_prof_info'.  */

#undef NDEBUG
#include <assert.h>

#include <acc_prof.h>


#define DEBUG_printf(...) //__builtin_printf (__VA_ARGS__)


static int ev_count;


static void cb_any_event (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s %d\n", __FUNCTION__, prof_info->event_type);

  assert (prof_info->version == 201711);

  ++ev_count;
}


void acc_register_library (acc_prof_reg reg_, acc_prof_reg unreg_, acc_prof_lookup_func lookup_)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  reg_ (acc_ev_device_init_start, cb_any_event, acc_reg);
  reg_ (acc_ev_device_init_end, cb_any_event, acc_reg);
  reg_ (acc_ev_device_shutdown_start, cb_any_event, acc_reg);
  reg_ (acc_ev_device_shutdown_end, cb_any_event, acc_reg);
  reg_ (acc_ev_runtime_shutdown, cb_any_event, acc_reg);
  reg_ (acc_ev_create, cb_any_event, acc_reg);
  reg_ (acc_ev_delete, cb_any_event, acc_reg);
  reg_ (acc_ev_alloc, cb_any_event, acc_reg);
  reg_ (acc_ev_free, cb_any_event, acc_reg);
  reg_ (acc_ev_enter_data_start, cb_any_event, acc_reg);
  reg_ (acc_ev_enter_data_end, cb_any_event, acc_reg);
  reg_ (acc_ev_exit_data_start, cb_any_event, acc_reg);
  reg_ (acc_ev_exit_data_end, cb_any_event, acc_reg);
  reg_ (acc_ev_update_start, cb_any_event, acc_reg);
  reg_ (acc_ev_update_end, cb_any_event, acc_reg);
  reg_ (acc_ev_compute_construct_start, cb_any_event, acc_reg);
  reg_ (acc_ev_compute_construct_end, cb_any_event, acc_reg);
  reg_ (acc_ev_enqueue_launch_start, cb_any_event, acc_reg);
  reg_ (acc_ev_enqueue_launch_end, cb_any_event, acc_reg);
  reg_ (acc_ev_enqueue_upload_start, cb_any_event, acc_reg);
  reg_ (acc_ev_enqueue_upload_end, cb_any_event, acc_reg);
  reg_ (acc_ev_enqueue_download_start, cb_any_event, acc_reg);
  reg_ (acc_ev_enqueue_download_end, cb_any_event, acc_reg);
  reg_ (acc_ev_wait_start, cb_any_event, acc_reg);
  reg_ (acc_ev_wait_end, cb_any_event, acc_reg);
}


int main()
{
  acc_register_library (acc_prof_register, acc_prof_unregister, acc_prof_lookup);

  ev_count = 0;

  /* Trigger tests done in 'cb_*' functions.  */
#pragma acc parallel
  {
    asm volatile ("" : : : "memory"); // TODO PR90488
  }

  DEBUG_printf ("ev_count = %d\n", ev_count);
  /* We don't know exactly how many events to expect, but we at least expect
     'acc_ev_device_init_start', 'acc_ev_device_init_end',
     'acc_ev_compute_construct_start', 'acc_ev_compute_construct_end'.  */
  assert (ev_count >= 4);

  return 0;
}

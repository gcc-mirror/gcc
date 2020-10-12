/* { dg-do run } */
/* { dg-timeout 10 } */

/* Test the calling of 'acc_get_device_type' from within
   'cb_device_init_start' and 'cb_device_init_end' callbacks.  This occurs
   when the CUDA 9.0 'nvprof' tool is used, and previously deadlocked.  */

#include <assert.h>
#include <stdbool.h>
#include <acc_prof.h>

static acc_prof_reg reg;
static acc_prof_reg unreg;
static acc_prof_lookup_func lookup;

void acc_register_library (acc_prof_reg reg_, acc_prof_reg unreg_, acc_prof_lookup_func lookup_)
{
  reg = reg_;
  unreg = unreg_;
  lookup = lookup_;
}

static bool expect_cb_device_init_start;
static bool expect_cb_device_init_end;

static void cb_device_init_start (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  assert (expect_cb_device_init_start);
  expect_cb_device_init_start = false;

  acc_device_t acc_device_type;
  acc_device_type = acc_get_device_type ();
  assert (acc_device_type == acc_device_none);

  expect_cb_device_init_end = true;
}

static void cb_device_init_end (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  assert (expect_cb_device_init_end);
  expect_cb_device_init_end = false;

  acc_device_t acc_device_type;
  acc_device_type = acc_get_device_type ();
  assert (acc_device_type == acc_device_none);
}

int main(void)
{
  acc_register_library (acc_prof_register, acc_prof_unregister, acc_prof_lookup);

  reg (acc_ev_device_init_start, cb_device_init_start, acc_reg);
  reg (acc_ev_device_init_end, cb_device_init_end, acc_reg);

  expect_cb_device_init_start = true;
  expect_cb_device_init_end = false;
  acc_init (acc_device_host);
  assert (!expect_cb_device_init_start);
  assert (!expect_cb_device_init_end);
  {
    acc_device_t acc_device_type;
    acc_device_type = acc_get_device_type ();
    assert (acc_device_type == acc_device_host);
  }
  acc_shutdown (acc_device_host);

  expect_cb_device_init_start = true;
  expect_cb_device_init_end = false;
  acc_init (acc_device_default);
  assert (!expect_cb_device_init_start);
  assert (!expect_cb_device_init_end);
  {
    acc_device_t acc_device_type;
    acc_device_type = acc_get_device_type ();
    assert (acc_device_type != acc_device_none);
  }
  acc_shutdown (acc_device_default);

  return 0;
}

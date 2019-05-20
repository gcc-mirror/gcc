/* Test the 'valid_bytes' magic.  */

#undef NDEBUG
#include <assert.h>

#include <acc_prof.h>


#define DEBUG_printf(...) //__builtin_printf (__VA_ARGS__)


static int ev_count_data;

static void cb_data_event (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s %d\n", __FUNCTION__, prof_info->event_type);

  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (event_info->data_event.valid_bytes == _ACC_DATA_EVENT_INFO_VALID_BYTES);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);

  ++ev_count_data;
}

static int ev_count_launch;

static void cb_launch_event (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s %d\n", __FUNCTION__, prof_info->event_type);

  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (event_info->launch_event.valid_bytes == _ACC_LAUNCH_EVENT_INFO_VALID_BYTES);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);

  ++ev_count_launch;
}

static int ev_count_other;

static void cb_other_event (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s %d\n", __FUNCTION__, prof_info->event_type);

  assert (prof_info->valid_bytes == _ACC_PROF_INFO_VALID_BYTES);
  assert (event_info->other_event.valid_bytes == _ACC_OTHER_EVENT_INFO_VALID_BYTES);
  assert (api_info->valid_bytes == _ACC_API_INFO_VALID_BYTES);

  ++ev_count_other;
}


void acc_register_library (acc_prof_reg reg_, acc_prof_reg unreg_, acc_prof_lookup_func lookup_)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  reg_ (acc_ev_device_init_start, cb_other_event, acc_reg);
  reg_ (acc_ev_device_init_end, cb_other_event, acc_reg);
  reg_ (acc_ev_device_shutdown_start, cb_other_event, acc_reg);
  reg_ (acc_ev_device_shutdown_end, cb_other_event, acc_reg);
  reg_ (acc_ev_runtime_shutdown, cb_other_event, acc_reg);
  reg_ (acc_ev_create, cb_data_event, acc_reg);
  reg_ (acc_ev_delete, cb_data_event, acc_reg);
  reg_ (acc_ev_alloc, cb_data_event, acc_reg);
  reg_ (acc_ev_free, cb_data_event, acc_reg);
  reg_ (acc_ev_enter_data_start, cb_other_event, acc_reg);
  reg_ (acc_ev_enter_data_end, cb_other_event, acc_reg);
  reg_ (acc_ev_exit_data_start, cb_other_event, acc_reg);
  reg_ (acc_ev_exit_data_end, cb_other_event, acc_reg);
  reg_ (acc_ev_update_start, cb_other_event, acc_reg);
  reg_ (acc_ev_update_end, cb_other_event, acc_reg);
  reg_ (acc_ev_compute_construct_start, cb_other_event, acc_reg);
  reg_ (acc_ev_compute_construct_end, cb_other_event, acc_reg);
  reg_ (acc_ev_enqueue_launch_start, cb_launch_event, acc_reg);
  reg_ (acc_ev_enqueue_launch_end, cb_launch_event, acc_reg);
  reg_ (acc_ev_enqueue_upload_start, cb_data_event, acc_reg);
  reg_ (acc_ev_enqueue_upload_end, cb_data_event, acc_reg);
  reg_ (acc_ev_enqueue_download_start, cb_data_event, acc_reg);
  reg_ (acc_ev_enqueue_download_end, cb_data_event, acc_reg);
  reg_ (acc_ev_wait_start, cb_other_event, acc_reg);
  reg_ (acc_ev_wait_end, cb_other_event, acc_reg);
}


/* Basic struct.  */
typedef struct A
{
  int a;
  int b;
#define VALID_BYTES_A \
  _ACC_PROF_VALID_BYTES_STRUCT (A, b, \
				_ACC_PROF_VALID_BYTES_BASICTYPE (int))
} A;

/* Add a 'char' field.  */
typedef struct B
{
  int a;
  int b;
  char c;
#define VALID_BYTES_B \
  _ACC_PROF_VALID_BYTES_STRUCT (B, c, \
				_ACC_PROF_VALID_BYTES_BASICTYPE (char))
} B;

/* Add another 'char' field.  */
typedef struct C
{
  int a;
  int b;
  char c, d;
#define VALID_BYTES_C \
  _ACC_PROF_VALID_BYTES_STRUCT (C, d, \
				_ACC_PROF_VALID_BYTES_BASICTYPE (char))
} C;

/* Add two 'void *' fields.  */
typedef struct D
{
  int a;
  int b;
  char c, d;
  void *e;
  void *f;
#define VALID_BYTES_D \
  _ACC_PROF_VALID_BYTES_STRUCT (D, f, \
				_ACC_PROF_VALID_BYTES_BASICTYPE (void *))
} D;

/* Add another three 'char' fields.  */
typedef struct E
{
  int a;
  int b;
  char c, d;
  void *e;
  void *f;
  char g, h, i;
#define VALID_BYTES_E \
  _ACC_PROF_VALID_BYTES_STRUCT (E, i, \
				_ACC_PROF_VALID_BYTES_BASICTYPE (char))
} E;


int main()
{
  acc_register_library (acc_prof_register, acc_prof_unregister, acc_prof_lookup);

  A A1;
  DEBUG_printf ("s=%zd, vb=%zd\n", sizeof A1, VALID_BYTES_A);
  assert (VALID_BYTES_A <= sizeof A1);
  DEBUG_printf ("&A1=%p, &A1.b=%p\n", &A1, &A1.b);
  assert (((char *) &A1) + VALID_BYTES_A == (char *) (&A1.b + 1));

  B B1;
  DEBUG_printf ("s=%zd, vb=%zd\n", sizeof B1, VALID_BYTES_B);
  assert (VALID_BYTES_B <= sizeof B1);
  DEBUG_printf ("&B1=%p, &B1.c=%p\n", &B1, &B1.c);
  assert (((char *) &B1) + VALID_BYTES_B == (char *) (&B1.c + 1));

  assert (VALID_BYTES_B == VALID_BYTES_A + 1 * sizeof (char));

  C C1;
  DEBUG_printf ("s=%zd, vb=%zd\n", sizeof C1, VALID_BYTES_C);
  assert (VALID_BYTES_C <= sizeof C1);
  DEBUG_printf ("&C1=%p, &C1.d=%p\n", &C1, &C1.d);
  assert (((char *) &C1) + VALID_BYTES_C == (char *) (&C1.d + 1));

  assert (VALID_BYTES_C == VALID_BYTES_B + 1 * sizeof (char));

  D D1;
  DEBUG_printf ("s=%zd, vb=%zd\n", sizeof D1, VALID_BYTES_D);
  assert (VALID_BYTES_D <= sizeof D1);
  DEBUG_printf ("&D1=%p, &D1.f=%p\n", &D1, &D1.f);
  assert (((char *) &D1) + VALID_BYTES_D == (char *) (&D1.f + 1));

  assert (VALID_BYTES_D > VALID_BYTES_C);

  E E1;
  DEBUG_printf ("s=%zd, vb=%zd\n", sizeof E1, VALID_BYTES_E);
  assert (VALID_BYTES_E <= sizeof E1);
  DEBUG_printf ("&E1=%p, &E1.i=%p\n", &E1, &E1.i);
  assert (((char *) &E1) + VALID_BYTES_E == (char *) (&E1.i + 1));

  assert (VALID_BYTES_E == VALID_BYTES_D + 3 * sizeof (char));

  ev_count_data = 0;
  ev_count_launch = 0;
  ev_count_other = 0;

  /* Trigger tests done in 'cb_*' functions.  */
  int host;
#pragma acc parallel copyout (host)
  {
    asm volatile ("" : : : "memory"); // TODO PR90488

    host = acc_on_device (acc_device_host);
  }

  DEBUG_printf ("ev_count_data = %d\n", ev_count_data);
  if (host)
    assert (ev_count_data == 0);
  else
    {
      /* We don't know exactly how many data events to expect, but we at least
	 expect some.  */
      assert (ev_count_data > 0);
    }

  DEBUG_printf ("ev_count_launch = %d\n", ev_count_launch);
  if (host)
    assert (ev_count_data == 0);
  else
    {
      /* We expect two launch events, 'acc_ev_enqueue_launch_start',
	 'acc_ev_enqueue_launch_end'.  */
      assert (ev_count_launch == 2);
    }

  DEBUG_printf ("ev_count_other = %d\n", ev_count_other);
  /* We don't know exactly how many other events to expect, but we at least
     expect 'acc_ev_device_init_start', 'acc_ev_device_init_end',
     'acc_ev_compute_construct_start', 'acc_ev_compute_construct_end'.  */
  assert (ev_count_other >= 4);

  return 0;
}

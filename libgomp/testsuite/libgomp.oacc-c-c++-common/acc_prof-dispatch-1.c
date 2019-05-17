/* Test dispatch of events to callbacks.  */

#undef NDEBUG
#include <assert.h>

#include <acc_prof.h>


/* Use explicit 'copyin' clauses, to work around "'firstprivate'
   optimizations", which will cause the value at the point of call to be used
   (*before* any potential modifications done in callbacks), as opposed to its
   address being taken, which then later gets dereferenced (*after* any
   modifications done in callbacks).  */
#define COPYIN(...) copyin(__VA_ARGS__)


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


static void cb_compute_construct_start_1 (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 0
	  || state == 10
	  || state == 30
	  || state == 41
	  || state == 51
	  || state == 91
	  || state == 101
	  || state == 151);
  STATE_OP (state, ++);
}

static void cb_compute_construct_start_2 (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 1
	  || state == 11
	  || state == 40
	  || state == 50
	  || state == 90
	  || state == 100
	  || state == 150);
  STATE_OP (state, ++);
}

static void cb_compute_construct_end_1 (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 14
	  || state == 21
	  || state == 32
	  || state == 42
	  || state == 80
	  || state == 103
	  || state == 152);
  STATE_OP (state, ++);
}

static void cb_compute_construct_end_2 (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 13
	  || state == 43
	  || state == 102
	  || state == 154);
  STATE_OP (state, ++);
}

static void cb_compute_construct_end_3 (acc_prof_info *prof_info, acc_event_info *event_info, acc_api_info *api_info)
{
  DEBUG_printf ("%s\n", __FUNCTION__);

  assert (state == 12
	  || state == 20
	  || state == 31
	  || state == 44
	  || state == 81
	  || state == 104
	  || state == 153);
  STATE_OP (state, ++);
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
  reg (acc_ev_compute_construct_start, cb_compute_construct_start_1, acc_reg);
  reg (acc_ev_compute_construct_start, cb_compute_construct_start_1, acc_reg);
  reg (acc_ev_compute_construct_start, cb_compute_construct_start_2, acc_reg);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 2);
  }
  assert (state == 2);

  STATE_OP (state, = 10);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_1, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_3, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_3, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_3, acc_reg);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 12);
  }
  assert (state == 15);

  STATE_OP (state, = 20);
  unreg (acc_ev_compute_construct_start, cb_compute_construct_start_1, acc_toggle);
  unreg (acc_ev_compute_construct_start, cb_compute_construct_start_2, acc_toggle);
  unreg (acc_ev_compute_construct_start, cb_compute_construct_start_1, acc_reg);
  unreg (acc_ev_compute_construct_start, cb_compute_construct_start_2, acc_reg);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_1, acc_toggle);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_toggle);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_3, acc_toggle);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_reg);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_reg);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_toggle);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_toggle);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 20);
  }
  assert (state == 20);

  STATE_OP (state, = 30);
  reg (acc_ev_compute_construct_start, cb_compute_construct_start_1, acc_toggle);
  reg (acc_ev_compute_construct_start, cb_compute_construct_start_2, acc_toggle);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_1, acc_toggle);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_toggle);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_3, acc_toggle);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 31);
  }
  assert (state == 33);

  STATE_OP (state, = 40);
  reg (acc_ev_compute_construct_start, cb_compute_construct_start_2, acc_reg);
  unreg (acc_ev_compute_construct_start, cb_compute_construct_start_1, acc_reg);
  reg (acc_ev_compute_construct_start, cb_compute_construct_start_1, acc_reg);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_3, acc_reg);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_3, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_reg);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_1, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_1, acc_reg);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 42);
  }
  assert (state == 45);

  STATE_OP (state, = 50);
  unreg (acc_ev_compute_construct_end, NULL, acc_toggle);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 52);
  }
  assert (state == 52);

  STATE_OP (state, = 60);
  unreg (acc_ev_compute_construct_end, NULL, acc_toggle);
  unreg (/* TODO */ (acc_event_t) 0, NULL, acc_toggle_per_thread);
  unreg (/* TODO */ (acc_event_t) 0, NULL, acc_toggle_per_thread);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 60);
  }
  assert (state == 60);

  STATE_OP (state, = 70);
  unreg (acc_ev_compute_construct_start, NULL, acc_toggle);
  reg (/* TODO */ (acc_event_t) 0, NULL, acc_toggle_per_thread);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 70);
  }
  assert (state == 70);

  STATE_OP (state, = 80);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_reg);
  reg (acc_ev_compute_construct_end, NULL, acc_toggle);
  reg (/* TODO */ (acc_event_t) 0, NULL, acc_toggle_per_thread);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 80);
  }
  assert (state == 82);

  STATE_OP (state, = 90);
  reg (acc_ev_compute_construct_start, NULL, acc_toggle);
  unreg (acc_ev_compute_construct_end, NULL, acc_toggle);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_2, acc_reg);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 92);
  }
  assert (state == 92);

  STATE_OP (state, = 100);
  reg (acc_ev_compute_construct_end, NULL, acc_toggle);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 102);
  }
  assert (state == 105);

  STATE_OP (state, = 110);
  unreg (/* TODO */ (acc_event_t) 0, NULL, acc_toggle);
  unreg (/* TODO */ (acc_event_t) 0, NULL, acc_toggle);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 110);
  }
  assert (state == 110);

  STATE_OP (state, = 120);
  unreg (/* TODO */ (acc_event_t) 0, NULL, acc_toggle_per_thread);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 120);
  }
  assert (state == 120);

  STATE_OP (state, = 130);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_3, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_3, acc_reg);
  reg (/* TODO */ (acc_event_t) 0, NULL, acc_toggle);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 130);
  }
  assert (state == 130);

  STATE_OP (state, = 140);
  unreg (acc_ev_compute_construct_start, cb_compute_construct_start_1, acc_reg);
  reg (acc_ev_compute_construct_start, cb_compute_construct_start_1, acc_reg);
  unreg (acc_ev_compute_construct_end, cb_compute_construct_end_1, acc_reg);
  reg (acc_ev_compute_construct_end, cb_compute_construct_end_1, acc_reg);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 140);
  }
  assert (state == 140);

  STATE_OP (state, = 150);
  reg (/* TODO */ (acc_event_t) 0, NULL, acc_toggle_per_thread);
  {
    int state_init;
#pragma acc parallel COPYIN(state) copyout(state_init)
    {
      state_init = state;
    }
    assert (state_init == 152);
  }
  assert (state == 155);

  return 0;
}

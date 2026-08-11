/* Verify task handling in the barrier used by single-copyprivate.  A selected
   logical thread queues tasks and is forced to win the first single region;
   every other thread therefore waits in GOMP_single_copy_start while the
   winner completes through GOMP_single_copy_end.  The copied value, task
   payload, and task count must all have been flushed when that barrier
   returns.

   A second copyprivate region rotates the winner to exercise reclamation of
   the first work-share.  Repeated 2 -> 4 -> 1 -> 3 transitions check the
   logical-ID argument while retained tree-barrier workers are reused.

   This testcase is in order to exercise team_id propagation from
   GOMP_single_copy_start and GOMP_single_copy_end.

   For targets other than linux_futex_waitv this is still worth running since
   it exercises controlled winners with queued tasks in the same team.  This is
   not something exercised elsewhere in the testsuite.  */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "tree" } */

#include <omp.h>
#include <sched.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum { tasks = 4, rounds = 8 };

struct state
{
  int queued;
  int gate_first;
  int gate_second;
  int completed;
  int first_blocks;
  int second_blocks;
  int after_first;
  int after_second;
  uint32_t payload[tasks];
};

static int
read_atomic (int *value)
{
  int result;
#pragma omp atomic read
  result = *value;
  return result;
}

static void
wait_for (int *value)
{
  while (!read_atomic (value))
    sched_yield ();
}

static uint32_t
task_payload (int task, int epoch)
{
  uint32_t value = (uint32_t) (epoch * tasks + task + 1);
  for (unsigned int i = 0; i < 128; ++i)
    value = value * 1103515245u + 12345u + i;
  return value;
}

static void
run_region (int nthreads, int epoch)
{
  struct state state;
  int first_winner = epoch % nthreads;
  int second_winner = (first_winner + 1) % nthreads;
  long long first_value = 0x12340000LL + epoch;
  long long second_value = 0x56780000LL + epoch;
  memset (&state, 0, sizeof (state));

#pragma omp parallel num_threads(nthreads) shared(state) \
  firstprivate(epoch, first_winner, second_winner, first_value, second_value)
  {
    int tid = omp_get_thread_num ();
    long long copied = -1;

    if (tid == first_winner)
      {
	for (int task = 0; task < tasks; ++task)
	  {
#pragma omp task firstprivate(task, epoch) shared(state)
	    {
	      state.payload[task] = task_payload (task, epoch);
#pragma omp atomic update
	      state.completed++;
	    }
	  }
#pragma omp atomic write
	state.queued = 1;
      }
    else
      wait_for (&state.queued);

    if (tid != first_winner)
      wait_for (&state.gate_first);
#pragma omp single copyprivate(copied)
    {
      if (tid != first_winner)
	abort ();
      copied = first_value;
#pragma omp atomic update
      state.first_blocks++;
#pragma omp atomic write
      state.gate_first = 1;
    }

    if (copied != first_value || state.completed != tasks)
      abort ();
    for (int task = 0; task < tasks; ++task)
      if (state.payload[task] != task_payload (task, epoch))
	abort ();
#pragma omp atomic update
    state.after_first++;

    copied = -1;
    if (tid != second_winner)
      wait_for (&state.gate_second);
#pragma omp single copyprivate(copied)
    {
      if (tid != second_winner)
	abort ();
      copied = second_value;
#pragma omp atomic update
      state.second_blocks++;
#pragma omp atomic write
      state.gate_second = 1;
    }

    if (copied != second_value)
      abort ();
#pragma omp atomic update
    state.after_second++;
  }

  if (!state.queued || state.completed != tasks || state.first_blocks != 1
      || state.second_blocks != 1 || state.after_first != nthreads
      || state.after_second != nthreads)
    abort ();
}

int
main (void)
{
  static const int sizes[] = { 2, 4, 1, 3 };
  int epoch = 0;

  omp_set_dynamic (0);
  for (int round = 0; round < rounds; ++round)
    for (int i = 0; i < 4; ++i)
      run_region (sizes[i], ++epoch);
  return 0;
}

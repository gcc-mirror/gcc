/* Verify the task-aware split barrier interface used in gomp_work_share_end.
   Tasks are queued immediately before a dynamically scheduled loop.  The
   loop's implicit GOMP_loop_end barrier must drain all team tasks, flush their
   payloads, and release every logical ID before a second work-share is
   allocated and reclaimed.

   Repeated 2 -> 4 -> 1 -> 3 team-size transitions cover retained-worker
   reuse.  This test selects the flat implementation; tree split-barrier
   callers are covered by the copyprivate and task-reduction tests.  */

/* { dg-do run { target linux_futex_waitv } } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "flat" } */

#include <omp.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum { tasks = 4, rounds = 8 };

struct state
{
  int completed;
  int first_iterations;
  int second_iterations;
  int after_first;
  uint32_t payload[tasks];
};

static uint32_t
task_payload (int task, int epoch)
{
  uint32_t value = (uint32_t) (epoch * tasks + task + 1);
  for (unsigned int i = 0; i < 128; ++i)
    value = value * 1664525u + 1013904223u + i;
  return value;
}

static void
run_region (int nthreads, int epoch)
{
  struct state state;
  memset (&state, 0, sizeof (state));

#pragma omp parallel num_threads(nthreads) firstprivate(nthreads, epoch) \
  shared(state)
  {
#pragma omp single nowait
    for (int task = 0; task < tasks; ++task)
      {
#pragma omp task firstprivate(task, epoch) shared(state)
	{
	  state.payload[task] = task_payload (task, epoch);
#pragma omp atomic update
	  state.completed++;
	}
      }

    /* A runtime schedule uses gomp_work_share_end's split start/end barrier
       rather than lowering directly to GOMP_barrier.  */
#pragma omp for schedule(dynamic, 1)
    for (int i = 0; i < nthreads * 3 + 1; ++i)
      {
#pragma omp atomic update
	state.first_iterations++;
      }

    if (state.completed != tasks)
      abort ();
    for (int task = 0; task < tasks; ++task)
      if (state.payload[task] != task_payload (task, epoch))
	abort ();
#pragma omp atomic update
    state.after_first++;

#pragma omp for schedule(guided, 1)
    for (int i = 0; i < nthreads * 2 + 3; ++i)
      {
#pragma omp atomic update
	state.second_iterations++;
      }
  }

  if (state.completed != tasks
      || state.first_iterations != nthreads * 3 + 1
      || state.second_iterations != nthreads * 2 + 3
      || state.after_first != nthreads)
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

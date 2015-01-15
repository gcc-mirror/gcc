
#include <stdio.h>
#include <cuda.h>

static int _Tnum_timers;
static CUevent *_Tstart_events, *_Tstop_events;
static CUstream _Tstream;

void
init_timers (int ntimers)
{
  int i;
  CUresult r;

  _Tnum_timers = ntimers;

  _Tstart_events = (CUevent *) malloc (_Tnum_timers * sizeof (CUevent));
  _Tstop_events = (CUevent *) malloc (_Tnum_timers * sizeof (CUevent));

  r = cuStreamCreate (&_Tstream, CU_STREAM_DEFAULT);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuStreamCreate failed: %d\n", r);
      abort ();
    }

  for (i = 0; i < _Tnum_timers; i++)
    {
      r = cuEventCreate (&_Tstart_events[i], CU_EVENT_DEFAULT);
      if (r != CUDA_SUCCESS)
	{
	  fprintf (stderr, "cuEventCreate failed: %d\n", r);
	  abort ();
	}

      r = cuEventCreate (&_Tstop_events[i], CU_EVENT_DEFAULT);
      if (r != CUDA_SUCCESS)
	{
	  fprintf (stderr, "cuEventCreate failed: %d\n", r);
	  abort ();
	}
    }
}

void
fini_timers (void)
{
  int i;

  for (i = 0; i < _Tnum_timers; i++)
    {
      cuEventDestroy (_Tstart_events[i]);
      cuEventDestroy (_Tstop_events[i]);
    }

  cuStreamDestroy (_Tstream);

  free (_Tstart_events);
  free (_Tstop_events);
}

void
start_timer (int timer)
{
  CUresult r;

  r = cuEventRecord (_Tstart_events[timer], _Tstream);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuEventRecord failed: %d\n", r);
      abort ();
    }
}

float
stop_timer (int timer)
{
  CUresult r;
  float etime;

  r = cuEventRecord (_Tstop_events[timer], _Tstream);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuEventRecord failed: %d\n", r);
      abort ();
    }

  r = cuEventSynchronize (_Tstop_events[timer]);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuEventSynchronize failed: %d\n", r);
      abort ();
    }

  r = cuEventElapsedTime (&etime, _Tstart_events[timer], _Tstop_events[timer]);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuEventElapsedTime failed: %d\n", r);
      abort ();
    }

  return etime;
}

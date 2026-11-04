// This test assumes that no environment variable changes the default!
// i.e. neither NUM_TEAMS (> 0) is set nor OMP_TEAMS_THREAD_LIMIT.

#include <omp.h>

int
main ()
{
  int nteams, tlimit;
  int ndevs = omp_get_num_devices ();

  if (omp_default_device < -10 || omp_invalid_device < -10)
    __builtin_abort ();

  for (int dev = -10; dev < 10; dev++)
    {
      bool is_host = (dev == omp_initial_device
		      || dev == ndevs
		      || (dev == omp_default_device
			  && (omp_get_default_device() == omp_initial_device
			      || omp_get_default_device() == ndevs)));
      bool is_invalid = (dev != omp_default_device
			 && dev != omp_initial_device
			 && (dev < 0 || dev > ndevs));

      nteams = omp_get_device_num_teams (dev);
      tlimit = omp_get_device_teams_thread_limit (dev);

      if (is_host
	  && (tlimit != omp_get_teams_thread_limit ()
	      || tlimit != omp_get_device_teams_thread_limit (dev)))
	__builtin_abort ();

      if (is_invalid)
	{
	  if (nteams != 0 || tlimit != 0)
	    __builtin_abort ();
	}
      else if (is_host && nteams != 1) // outside 'teams'
	__builtin_abort ();
      else if (!is_host && nteams != 0) // default init = 0 (according to the spec)
	__builtin_abort ();

      if (!is_invalid)
	{
	  #pragma omp target firstprivate(nteams, tlimit) device(dev)
	  {
	    if (tlimit != omp_get_teams_thread_limit ())
	      __builtin_abort ();
	  }
	}
    }

  omp_set_default_device (omp_initial_device);

  for (int i = 0; i < 4; i++)
    {
      int val1 = 4 + i;
      int val2 = 3 + i;

      switch(i)
	{
	case 0: omp_set_num_teams(val1); break;
	case 1: omp_set_device_num_teams (val1, omp_initial_device); break;
	case 2: omp_set_device_num_teams (val1, omp_default_device); break;
	case 3: omp_set_device_num_teams (val1, ndevs); break;
	}

	#pragma omp teams
	{
	  #pragma omp parallel if(0)
	  {
	  if (val1 != omp_get_num_teams ())
	    __builtin_abort ();
	  if (val1 != omp_get_device_num_teams (omp_initial_device))
	    __builtin_abort ();
	  if (val1 != omp_get_device_num_teams (omp_default_device))
	    __builtin_abort ();
	  if (val1 != omp_get_device_num_teams (ndevs))
	    __builtin_abort ();
	  }
	}

      switch(i)
	{
	case 0: omp_set_teams_thread_limit (val2); break;
	case 1: omp_set_device_teams_thread_limit (val2, omp_initial_device); break;
	case 2: omp_set_device_teams_thread_limit (val2, omp_default_device); break;
	case 3: omp_set_device_teams_thread_limit (val2, ndevs); break;
	}

      if (val2 != omp_get_teams_thread_limit ())
	__builtin_abort ();
      if (val2 != omp_get_device_teams_thread_limit (omp_initial_device))
	__builtin_abort ();
      if (val2 != omp_get_device_teams_thread_limit (omp_default_device))
	__builtin_abort ();
      if (val2 != omp_get_device_teams_thread_limit (ndevs))
	__builtin_abort ();
    }

  for (int dev = 0; dev < ndevs; dev++)
    {
	 omp_set_device_num_teams (5, dev);
	 if (5 != omp_get_device_num_teams (dev))
	   __builtin_abort ();

	 #pragma omp target teams device(dev)
	  if (5 != omp_get_num_teams ())
	    __builtin_abort ();

	 omp_set_device_teams_thread_limit (7, dev);

	 if (7 != omp_get_device_teams_thread_limit (dev))
	   __builtin_abort ();

      #pragma omp target teams device(dev) num_teams(1)
	{
	  #pragma omp parallel if(0)
	  {
	  if (7 != omp_get_teams_thread_limit ())
	    __builtin_abort ();
	  }
        }
    }

  for (int dev = omp_initial_device; dev <= ndevs; dev++)
    {
      bool is_host = (dev == omp_initial_device || dev == ndevs);

      omp_set_device_teams_thread_limit (8, dev);
      if (omp_get_device_teams_thread_limit (dev) != 8)
	__builtin_abort ();
      if (is_host && omp_get_teams_thread_limit () != 8)
	__builtin_abort ();

      // Setting to value -1 is not permitted, hence, expect old value:
      omp_set_device_teams_thread_limit (-1, dev);
      if (omp_get_device_teams_thread_limit (dev) != 8)
	__builtin_abort ();
      if (is_host && omp_get_teams_thread_limit () != 8)
	__builtin_abort ();

      // Likewise.
      if (is_host)
	{
	  omp_set_teams_thread_limit (-1);
	  if (omp_get_device_teams_thread_limit (dev) != 8)
	    __builtin_abort ();
	  if (is_host && omp_get_teams_thread_limit () != 8)
	    __builtin_abort ();
	}

      // Let's try value = 0. This value is well defined (to mean implementation
      // defined) but not permitted. Hence, permit both results:
      omp_set_device_teams_thread_limit (0, dev);
      if (omp_get_device_teams_thread_limit (dev) != 8
	  && omp_get_device_teams_thread_limit (dev) != 0)
	__builtin_abort ();
      if (is_host
	  && omp_get_teams_thread_limit () != 8
	  && omp_get_teams_thread_limit () != 0)
	__builtin_abort ();

      // Likewise.
      if (is_host)
	{
	  omp_set_teams_thread_limit (0);
	  if (omp_get_device_teams_thread_limit (dev) != 8
	      &&  omp_get_device_teams_thread_limit (dev) != 0)
	    __builtin_abort ();
	  if (is_host
	      && omp_get_teams_thread_limit () != 8
	      && omp_get_teams_thread_limit () != 0)
	    __builtin_abort ();
	}

      // However, the nteams-ICV can be set to zero,
      // but we cannot check its value on the host as
      // omp_get_num_teams gives the effective team size not the ICV value.
      if (is_host)
	continue;
      
      omp_set_device_num_teams (8, dev);
      if (omp_get_device_num_teams (dev) != 8)
	__builtin_abort ();

      // Setting to value 0 is permitted, hence, expect that value:
      omp_set_device_num_teams (0, dev);
      if (omp_get_device_num_teams (dev) != 0)
	__builtin_abort ();
    }
}

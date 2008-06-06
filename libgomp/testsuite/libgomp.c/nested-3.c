#include <omp.h>
#include <stdlib.h>
#include <string.h>

int
main (void)
{
  int e[3];

  memset (e, '\0', sizeof (e));
  omp_set_nested (1);
  omp_set_dynamic (0);
  if (omp_in_parallel ()
      || omp_get_level () != 0
      || omp_get_ancestor_thread_num (0) != 0
      || omp_get_ancestor_thread_num (-1) != -1
      || omp_get_ancestor_thread_num (1) != -1
      || omp_get_team_size (0) != 1
      || omp_get_team_size (-1) != -1
      || omp_get_team_size (1) != -1
      || omp_get_active_level () != 0)
    abort ();
#pragma omp parallel num_threads (4)
  {
    int tn1 = omp_get_thread_num ();
    if (omp_in_parallel () != 1
	|| omp_get_num_threads () != 4
	|| tn1 >= 4 || tn1 < 0
	|| omp_get_level () != 1
	|| omp_get_ancestor_thread_num (0) != 0
	|| omp_get_ancestor_thread_num (1) != tn1
	|| omp_get_ancestor_thread_num (-1) != -1
	|| omp_get_ancestor_thread_num (2) != -1
	|| omp_get_team_size (0) != 1
	|| omp_get_team_size (1) != omp_get_num_threads ()
	|| omp_get_team_size (-1) != -1
	|| omp_get_team_size (2) != -1
	|| omp_get_active_level () != 1)
      #pragma omp atomic
	e[0] += 1;
    #pragma omp parallel if (0) num_threads(5) firstprivate(tn1)
    {
      int tn2 = omp_get_thread_num ();
      if (omp_in_parallel () != 1
	  || omp_get_num_threads () != 1
	  || tn2 != 0
	  || omp_get_level () != 2
	  || omp_get_ancestor_thread_num (0) != 0
	  || omp_get_ancestor_thread_num (1) != tn1
	  || omp_get_ancestor_thread_num (2) != tn2
	  || omp_get_ancestor_thread_num (-1) != -1
	  || omp_get_ancestor_thread_num (3) != -1
	  || omp_get_team_size (0) != 1
	  || omp_get_team_size (1) != 4
	  || omp_get_team_size (2) != 1
	  || omp_get_team_size (-1) != -1
	  || omp_get_team_size (3) != -1
	  || omp_get_active_level () != 1)
	#pragma omp atomic
	  e[1] += 1;
      #pragma omp parallel num_threads(2) firstprivate(tn1, tn2)
      {
	int tn3 = omp_get_thread_num ();
	if (omp_in_parallel () != 1
	    || omp_get_num_threads () != 2
	    || tn3 > 1 || tn3 < 0
	    || omp_get_level () != 3
	    || omp_get_ancestor_thread_num (0) != 0
	    || omp_get_ancestor_thread_num (1) != tn1
	    || omp_get_ancestor_thread_num (2) != tn2
	    || omp_get_ancestor_thread_num (3) != tn3
	    || omp_get_ancestor_thread_num (-1) != -1
	    || omp_get_ancestor_thread_num (4) != -1
	    || omp_get_team_size (0) != 1
	    || omp_get_team_size (1) != 4
	    || omp_get_team_size (2) != 1
	    || omp_get_team_size (3) != 2
	    || omp_get_team_size (-1) != -1
	    || omp_get_team_size (4) != -1
	    || omp_get_active_level () != 2)
	  #pragma omp atomic
	    e[2] += 1;
      }
    }
  }
  if (e[0] || e[1] || e[2])
    abort ();
  return 0;
}

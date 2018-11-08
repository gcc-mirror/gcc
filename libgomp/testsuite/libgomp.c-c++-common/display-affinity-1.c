/* { dg-set-target-env-var OMP_PROC_BIND "spread,close" } */
/* { dg-set-target-env-var OMP_PLACES "cores" } */
/* { dg-set-target-env-var OMP_NUM_THREADS "4" } */
/* { dg-set-target-env-var OMP_AFFINITY_FORMAT "hello" } */

#include <omp.h>
#include <string.h>
#include <stdlib.h>

int
main ()
{
#define FMT "L:%0.5L%%%n>%32H<!%.33{host}!%.6P_%i_%0.18i_%0.7{ancestor_tnum} %18A"
  char buf[] = FMT, hostname[256], buf2[512 + 32], *q;
  size_t l, l2, l3;
  char *r = getenv ("OMP_AFFINITY_FORMAT");
  if (r && strcmp (r, "hello") == 0)
    {
      if (omp_get_affinity_format (NULL, 0) != 5)
	abort ();
      if (omp_get_affinity_format (buf2, 3) != 5
	  || strcmp (buf2, "he") != 0)
	abort ();
      if (omp_get_affinity_format (buf2, 6) != 5
	  || strcmp (buf2, "hello") != 0)
	abort ();
    }
  omp_set_affinity_format (buf);
  memset (buf, '^', sizeof (buf));
  if (omp_get_affinity_format (NULL, 0) != sizeof (buf) - 1)
    abort ();
  if (omp_get_affinity_format (buf, 3) != sizeof (buf) - 1
      || buf[0] != FMT[0] || buf[1] != FMT[1] || buf[2] != '\0')
    abort ();
  memset (buf, ' ', sizeof (buf));
  if (omp_get_affinity_format (buf, sizeof (buf) - 1) != sizeof (buf) - 1
      || strncmp (buf, FMT, sizeof (buf) - 2) != 0
      || buf[sizeof (buf) - 2] != '\0')
    abort ();
  memset (buf, '-', sizeof (buf));
  if (omp_get_affinity_format (buf, sizeof (buf)) != sizeof (buf) - 1
      || strcmp (buf, FMT) != 0)
    abort ();
  memset (buf, '0', sizeof (buf));
  omp_display_affinity (NULL);
  omp_display_affinity ("");
  omp_display_affinity ("%%%0.9N");
  omp_set_affinity_format ("%{host}");
  l = omp_capture_affinity (hostname, sizeof hostname, NULL);
  if (l < sizeof (hostname))
    {
      if (strlen (hostname) != l)
	abort ();
      l2 = omp_capture_affinity (NULL, 0,
				 "%0.5{nesting_level}%%%32{host}|||%.33H"
				 "%0.7a%3N!%N!");
      if (l2 != (5 + 1 + (l > 32 ? l : 32) + 3 + (l > 33 ? l : 33)
		 + 7 + 3 + 1 + 1 + 1))
	abort ();
      omp_set_affinity_format ("%.5L%%%32H|||%.33{host}%0.7{ancestor_tnum}"
			       "%3{num_threads}!%{num_threads}!");
      l3 = omp_capture_affinity (buf2, sizeof buf2, "");
      if (l3 != l2)
	abort ();
      if (memcmp (buf2, "    0%", 5 + 1) != 0)
	abort ();
      q = buf2 + 6;
      if (memcmp (q, hostname, l) != 0)
	abort ();
      q += l;
      if (l < 32)
	for (l3 = 32 - l; l3; l3--)
	  if (*q++ != ' ')
	    abort ();
      if (memcmp (q, "|||", 3) != 0)
	abort ();
      q += 3;
      if (l < 33)
	for (l3 = 33 - l; l3; l3--)
	  if (*q++ != ' ')
	    abort ();
      if (memcmp (q, hostname, l) != 0)
	abort ();
      q += l;
      if (strcmp (q, "-0000011  !1!") != 0)
	abort ();
    }
  #pragma omp parallel num_threads (4) proc_bind(spread)
  omp_display_affinity ("%0.2a!%n!%.4L!%N;%.2t;%0.2T;%{team_num};%{num_teams};%A");
  return 0;
}

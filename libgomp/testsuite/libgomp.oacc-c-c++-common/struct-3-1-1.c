/* Test dynamic mapping of separate structure members.  */

#include <assert.h>
#include <stdio.h>
#include <openacc.h>

struct s
{
  char a;
  float b;
};

int main ()
{
  struct s s;

#pragma acc enter data create(s.a)
  assert (acc_is_present (&s.a, sizeof s.a));

  fprintf (stderr, "CheCKpOInT1\n");
  /* { dg-output ".*CheCKpOInT1(\n|\r\n|\r)" } */
#pragma acc enter data create(s.b)
  /* { dg-output "(\n|\r\n|\r)libgomp: Trying to map into device \\\[\[0-9a-fA-FxX.\]+\\\) structure element when other mapped elements from the same structure weren't mapped together with it(\n|\r\n|\r)$" { target { ! openacc_host_selected } } } ! Scan for what we expect in the "XFAILed" case (without actually XFAILing).
     { dg-shouldfail "XFAILed" { ! openacc_host_selected } } ! ... instead of 'dg-xfail-run-if' so that 'dg-output' is evaluated at all.
     { dg-final { if { [dg-process-target { xfail { ! openacc_host_selected } }] == "F" } { xfail "[testname-for-summary] really is XFAILed" } } } ! ... so that we still get an XFAIL visible in the log.  */
  fprintf (stderr, "CheCKpOInT2\n");
  /* { dg-output "CheCKpOInT2(\n|\r\n|\r)" { target { openacc_host_selected } } } */
  assert (acc_is_present (&s.b, sizeof s.b));

  //TODO PR95236
  assert (acc_is_present (&s, sizeof s));

  return 0;
}

! { dg-do run }

/* Nullify the 'finalize' clause.

   That means, we do not detach properly, the host sees a device pointer, and
   we fail as follows.
   { dg-output "STOP 30(\n|\r\n|\r)+" { target { ! openacc_host_selected } } }
   { dg-shouldfail "" { ! openacc_host_selected } }
*/
#define finalize
#include "deep-copy-6.f90"


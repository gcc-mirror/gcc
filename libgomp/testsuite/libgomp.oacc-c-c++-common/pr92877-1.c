/* Make sure that we can resolve back via 'acc_hostptr' an 'acc_deviceptr'
   retrieved for a structured mapping.  */

#include <assert.h>
#include <openacc.h>

int
main ()
{
  int var;

#pragma acc data create (var)
  {
    void *var_p_d = acc_deviceptr (&var);
    assert (acc_hostptr (var_p_d) == &var);
  }

  return 0;
}

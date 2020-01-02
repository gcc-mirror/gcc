/* Verify that 'acc_delete' etc. on non-present data is a no-op.  */

#include <openacc.h>

int
main ()
{
  int a;

  int async = 0;

#pragma acc exit data copyout (a)
  acc_copyout (&a, sizeof a);
#pragma acc exit data copyout (a) async (async++)
  acc_copyout_async (&a, sizeof a, async++);
#pragma acc exit data copyout (a) finalize
  acc_copyout_finalize (&a, sizeof a);
#pragma acc exit data copyout (a) finalize async (async++)
  acc_copyout_finalize_async (&a, sizeof a, async++);

#pragma acc exit data delete (a)
  acc_delete (&a, sizeof a);
#pragma acc exit data delete (a) async (async++)
  acc_delete_async (&a, sizeof a, async++);
#pragma acc exit data delete (a) finalize
  acc_delete_finalize (&a, sizeof a);
#pragma acc exit data delete (a) finalize async (async++)
  acc_delete_finalize_async (&a, sizeof a, async++);

  acc_wait_all ();

  return 0;
}

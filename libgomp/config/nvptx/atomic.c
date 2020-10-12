#include "libgomp.h"

#include "../../atomic.c"

/* Implement __sync_val_compare_and_swap_16, to support offloading from hosts
   that support this builtin.  Fallback on libatomic.  This can be removed
   once omp-expand starts using __atomic_compare_exchange_n instead.  */

unsigned __int128
__sync_val_compare_and_swap_16 (volatile void *vptr, unsigned __int128 oldval,
				unsigned __int128 newval)
{
  volatile __int128 *ptr = vptr;
  __int128 expected = oldval;
  __atomic_compare_exchange_n (ptr, &expected, newval, false,
			       MEMMODEL_SEQ_CST, MEMMODEL_SEQ_CST);
  return expected;
}

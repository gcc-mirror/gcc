/* Avoiding the synchronizations are a good thing.  */
#define WANT_SPECIALCASE_RELAXED
#define WANT_SPECIALCASE_ACQREL

/* Glibc, at least, uses lwsync (aka acq_rel) in its pthread mutex
   implementation.  If the user is asking for seq_cst (aka hwsync),
   this is insufficient.  */

static inline void __attribute__((always_inline, artificial))
pre_seq_barrier(int model)
{
  if (model == __ATOMIC_SEQ_CST)
    __atomic_thread_fence (__ATOMIC_SEQ_CST);
}

static inline void __attribute__((always_inline, artificial))
post_seq_barrier(int model)
{
  pre_seq_barrier(model);
}

#define pre_post_seq_barrier 1

#include_next <host-config.h>

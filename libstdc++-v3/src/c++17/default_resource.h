// This is only in a header so we can use the system_header pragma,
// to suppress the warning caused by using a reserved init_priority.
#pragma GCC system_header

#if ATOMIC_POINTER_LOCK_FREE == 2 || defined(__GTHREAD_MUTEX_INIT)
# error "This file should not be included for this build"
#endif

struct {
  atomic_mem_res obj = &newdel_res.obj;
} default_res __attribute__ ((init_priority (100)));

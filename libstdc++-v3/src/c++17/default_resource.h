// This is only in a header so we can use the system_header pragma,
// to suppress the warning caused by using a reserved init_priority.
#pragma GCC system_header

#ifndef _GLIBCXX_HAS_GTHREADS
# error "This file should not be included for this build"
#elif ATOMIC_POINTER_LOCK_FREE == 2
# error "This file should not be included for this build"
#elif defined __GTHREAD_MUTEX_INIT
# error "This file should not be included for this build"
#endif

struct {
  atomic_mem_res obj = &newdel_res.obj;
} default_res __attribute__ ((init_priority (100)));

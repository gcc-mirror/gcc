/**
 * D header file for Darwin.
 *
 * Copyright: Copyright Sean Kelly 2008 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly
 */

/*          Copyright Sean Kelly 2008 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.darwin.pthread;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):
extern (C):
nothrow:
@nogc:

public import core.sys.posix.pthread;
public import core.sys.darwin.mach.port;

int pthread_is_threaded_np();
int pthread_threadid_np(pthread_t, ulong*);
// ^ __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_3_2)
int pthread_rwlock_longrdlock_np(pthread_rwlock_t*);
int pthread_rwlock_yieldwrlock_np(pthread_rwlock_t*);
// ^ __OSX_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_NA);
int pthread_rwlock_downgrade_np(pthread_rwlock_t*);
int pthread_rwlock_upgrade_np(pthread_rwlock_t*);
int pthread_rwlock_tryupgrade_np(pthread_rwlock_t*);
int pthread_rwlock_held_np(pthread_rwlock_t*);
int pthread_rwlock_rdheld_np(pthread_rwlock_t*);
int pthread_rwlock_wrheld_np(pthread_rwlock_t*);
int pthread_getname_np(pthread_t, char*, size_t);
int pthread_setname_np(in char*);
// ^ __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_3_2)
int pthread_main_np();
mach_port_t pthread_mach_thread_np(pthread_t);
size_t pthread_get_stacksize_np(pthread_t);
void* pthread_get_stackaddr_np(pthread_t);
int pthread_cond_signal_thread_np(pthread_cond_t*, pthread_t);
int pthread_cond_timedwait_relative_np(pthread_cond_t*, pthread_mutex_t*, in timespec*);
int pthread_create_suspended_np(pthread_t*, in pthread_attr_t*, void* function(void*), void*);
int pthread_kill(pthread_t, int);
pthread_t pthread_from_mach_thread_np(mach_port_t);
// ^ __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_2_0)
int pthread_sigmask(int, in sigset_t*, sigset_t*);
// ^ __DARWIN_ALIAS(pthread_sigmask)
void pthread_yield_np();

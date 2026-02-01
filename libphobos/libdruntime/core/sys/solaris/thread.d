/**
  * D header file for Solaris thread.h.
  *
  * Copyright: Copyright © 2025, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Iain Buclaw
  */
module core.sys.solaris.thread;

version (Solaris):
extern (C):
nothrow:
@nogc:

import core.stdc.config : c_long;
import core.sys.posix.signal : sigset_t, stack_t;

/*
 * definitions needed to use the thread interface except synchronization.
 */
alias thread_t = int;
alias thread_key_t = int;

int thr_create(void*, size_t, void* function(void*), void*, c_long, thread_t*);
int thr_join(thread_t, thread_t*, void**);
int thr_setconcurrency(int);
int thr_getconcurrency();
noreturn thr_exit(void*);
thread_t thr_self();

int thr_sigsetmask(int, const scope sigset_t*, sigset_t*);

int thr_stksegment(stack_t*);

int thr_main();
int thr_kill(thread_t, int);
int thr_suspend(thread_t);
int thr_continue(thread_t);
void thr_yield();
int thr_setprio(thread_t, int);
int thr_getprio(thread_t, int*);
int thr_keycreate(thread_key_t*, void function(void*));
int thr_keycreate_once(thread_key_t*, void function(void*));
int thr_setspecific(thread_key_t, void*);
int thr_getspecific(thread_key_t, void**);
size_t thr_min_stack();

alias THR_MIN_STACK = thr_min_stack;

/*
 * thread flags (one word bit mask)
 */
enum : c_long
{
    THR_BOUND = 0x00000001,     // = PTHREAD_SCOPE_SYSTEM
    THR_NEW_LWP = 0x00000002,
    THR_DETACHED = 0x00000040,  // = PTHREAD_CREATE_DETACHED
    THR_SUSPENDED = 0x00000080,
    THR_DAEMON = 0x00000100,
}

/*
 * The key to be created by thr_keycreate_once()
 * must be statically initialized with THR_ONCE_KEY.
 */
enum thread_key_t THR_ONCE_KEY = -1;

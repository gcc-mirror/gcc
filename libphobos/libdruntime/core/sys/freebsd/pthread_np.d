/**
 * D header file for FreeBSD.
 *
 * Authors:   Martin Nowak
 */
module core.sys.freebsd.pthread_np;

version (FreeBSD):
extern (C) nothrow @nogc:

public import core.sys.posix.sys.types;
// TODO: add full core.sys.freebsd.sys.cpuset;
public import core.sys.freebsd.sys._cpuset;
public import core.sys.posix.time;

enum __BSD_VISIBLE = true;

alias pthread_switch_routine_t = void function(pthread_t, pthread_t);

int pthread_attr_setcreatesuspend_np(pthread_attr_t *);
int pthread_attr_get_np(pthread_t, pthread_attr_t *);
int pthread_attr_getaffinity_np(const(pthread_attr_t)*, size_t, cpuset_t *);
int pthread_attr_setaffinity_np(pthread_attr_t *, size_t, const(cpuset_t)*);
int pthread_getaffinity_np(pthread_t, size_t, cpuset_t *);
int pthread_getthreadid_np();
int pthread_main_np();
int pthread_multi_np();
int pthread_mutexattr_getkind_np(pthread_mutexattr_t);
int pthread_mutexattr_setkind_np(pthread_mutexattr_t *, int);
void pthread_resume_all_np();
int pthread_resume_np(pthread_t);
void pthread_set_name_np(pthread_t, const(char)*);
int pthread_mutex_getspinloops_np(pthread_mutex_t *mutex, int *count);
int pthread_mutex_setspinloops_np(pthread_mutex_t *mutex, int count);
int pthread_mutex_getyieldloops_np(pthread_mutex_t *mutex, int *count);
int pthread_mutex_setyieldloops_np(pthread_mutex_t *mutex, int count);
int pthread_mutex_isowned_np(pthread_mutex_t *mutex);
int pthread_setaffinity_np(pthread_t, size_t, const(cpuset_t)*);
int pthread_single_np();
void pthread_suspend_all_np();
int pthread_suspend_np(pthread_t);
int pthread_switch_add_np(pthread_switch_routine_t);
int pthread_switch_delete_np(pthread_switch_routine_t);
int pthread_timedjoin_np(pthread_t, void **, const(timespec)*);

..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable, Implementation specific setting

.. _gomp_rtems_thread_pools:

GOMP_RTEMS_THREAD_POOLS -- Set the RTEMS specific thread pools
**************************************************************

Description:
  This environment variable is only used on the RTEMS real-time operating system.
  It determines the scheduler instance specific thread pools.  The format for
  :envvar:`GOMP_RTEMS_THREAD_POOLS` is a list of optional
  ``<thread-pool-count>[$<priority>]@<scheduler-name>`` configurations
  separated by ``:`` where:

  * ``<thread-pool-count>`` is the thread pool count for this scheduler
    instance.

  * ``$<priority>`` is an optional priority for the worker threads of a
    thread pool according to ``pthread_setschedparam``.  In case a priority
    value is omitted, then a worker thread will inherit the priority of the OpenMP
    primary thread that created it.  The priority of the worker thread is not
    changed after creation, even if a new OpenMP primary thread using the worker has
    a different priority.

  * ``@<scheduler-name>`` is the scheduler instance name according to the
    RTEMS application configuration.

  In case no thread pool configuration is specified for a scheduler instance,
  then each OpenMP primary thread of this scheduler instance will use its own
  dynamically allocated thread pool.  To limit the worker thread count of the
  thread pools, each OpenMP primary thread must call ``omp_set_num_threads``.

Example:
  Lets suppose we have three scheduler instances ``IO``, ``WRK0``, and
  ``WRK1`` with :envvar:`GOMP_RTEMS_THREAD_POOLS` set to
  ``"1@WRK0:3$4@WRK1"``.  Then there are no thread pool restrictions for
  scheduler instance ``IO``.  In the scheduler instance ``WRK0`` there is
  one thread pool available.  Since no priority is specified for this scheduler
  instance, the worker thread inherits the priority of the OpenMP primary thread
  that created it.  In the scheduler instance ``WRK1`` there are three thread
  pools available and their worker threads run at priority four.

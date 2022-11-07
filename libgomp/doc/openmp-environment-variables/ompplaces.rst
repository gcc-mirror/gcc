..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable

.. _omp_places:

OMP_PLACES -- Specifies on which CPUs the theads should be placed
*****************************************************************

Description:
  The thread placement can be either specified using an abstract name or by an
  explicit list of the places.  The abstract names ``threads``, ``cores``,
  ``sockets``, ``ll_caches`` and ``numa_domains`` can be optionally
  followed by a positive number in parentheses, which denotes the how many places
  shall be created.  With ``threads`` each place corresponds to a single
  hardware thread; ``cores`` to a single core with the corresponding number of
  hardware threads; with ``sockets`` the place corresponds to a single
  socket; with ``ll_caches`` to a set of cores that shares the last level
  cache on the device; and ``numa_domains`` to a set of cores for which their
  closest memory on the device is the same memory and at a similar distance from
  the cores.  The resulting placement can be shown by setting the
  :envvar:`OMP_DISPLAY_ENV` environment variable.

  Alternatively, the placement can be specified explicitly as comma-separated
  list of places.  A place is specified by set of nonnegative numbers in curly
  braces, denoting the hardware threads.  The curly braces can be omitted
  when only a single number has been specified.  The hardware threads
  belonging to a place can either be specified as comma-separated list of
  nonnegative thread numbers or using an interval.  Multiple places can also be
  either specified by a comma-separated list of places or by an interval.  To
  specify an interval, a colon followed by the count is placed after
  the hardware thread number or the place.  Optionally, the length can be
  followed by a colon and the stride number -- otherwise a unit stride is
  assumed.  Placing an exclamation mark (``!``) directly before a curly
  brace or numbers inside the curly braces (excluding intervals) will
  exclude those hardware threads.

  For instance, the following specifies the same places list:
  ``"{0,1,2}, {3,4,6}, {7,8,9}, {10,11,12}"`` ;
  ``"{0:3}, {3:3}, {7:3}, {10:3}"`` ; and ``"{0:2}:4:3"``.

  If :envvar:`OMP_PLACES` and :envvar:`GOMP_CPU_AFFINITY` are unset and
  :envvar:`OMP_PROC_BIND` is either unset or ``false``, threads may be moved
  between CPUs following no placement policy.

See also:
  :ref:`OMP_PROC_BIND`, :ref:`GOMP_CPU_AFFINITY`, :ref:`omp_get_proc_bind`,
  :ref:`OMP_DISPLAY_ENV`

Reference:
  :openmp:`4.5`, Section 4.5
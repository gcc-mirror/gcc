..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable

.. _gomp_cpu_affinity:

GOMP_CPU_AFFINITY -- Bind threads to specific CPUs
**************************************************

Description:
  Binds threads to specific CPUs.  The variable should contain a space-separated
  or comma-separated list of CPUs.  This list may contain different kinds of
  entries: either single CPU numbers in any order, a range of CPUs (M-N)
  or a range with some stride (M-N:S).  CPU numbers are zero based.  For example,
  ``GOMP_CPU_AFFINITY="0 3 1-2 4-15:2"`` will bind the initial thread
  to CPU 0, the second to CPU 3, the third to CPU 1, the fourth to
  CPU 2, the fifth to CPU 4, the sixth through tenth to CPUs 6, 8, 10, 12,
  and 14 respectively and then start assigning back from the beginning of
  the list.  ``GOMP_CPU_AFFINITY=0`` binds all threads to CPU 0.

  There is no libgomp library routine to determine whether a CPU affinity
  specification is in effect.  As a workaround, language-specific library
  functions, e.g., ``getenv`` in C or ``GET_ENVIRONMENT_VARIABLE`` in
  Fortran, may be used to query the setting of the ``GOMP_CPU_AFFINITY``
  environment variable.  A defined CPU affinity on startup cannot be changed
  or disabled during the runtime of the application.

  If both :envvar:`GOMP_CPU_AFFINITY` and :envvar:`OMP_PROC_BIND` are set,
  :envvar:`OMP_PROC_BIND` has a higher precedence.  If neither has been set and
  :envvar:`OMP_PROC_BIND` is unset, or when :envvar:`OMP_PROC_BIND` is set to
  ``FALSE``, the host system will handle the assignment of threads to CPUs.

See also:
  :ref:`OMP_PLACES`, :ref:`OMP_PROC_BIND`
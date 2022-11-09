..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable

.. _omp_proc_bind:

OMP_PROC_BIND -- Whether theads may be moved between CPUs
*********************************************************

Description:
  Specifies whether threads may be moved between processors.  If set to
  ``TRUE``, OpenMP theads should not be moved; if set to ``FALSE``
  they may be moved.  Alternatively, a comma separated list with the
  values ``PRIMARY``, ``MASTER``, ``CLOSE`` and ``SPREAD`` can
  be used to specify the thread affinity policy for the corresponding nesting
  level.  With ``PRIMARY`` and ``MASTER`` the worker threads are in the
  same place partition as the primary thread.  With ``CLOSE`` those are
  kept close to the primary thread in contiguous place partitions.  And
  with ``SPREAD`` a sparse distribution
  across the place partitions is used.  Specifying more than one item in the
  list will automatically enable nesting by default.

  When undefined, :envvar:`OMP_PROC_BIND` defaults to ``TRUE`` when
  :envvar:`OMP_PLACES` or :envvar:`GOMP_CPU_AFFINITY` is set and ``FALSE`` otherwise.

See also:
  :ref:`omp_get_proc_bind`, :ref:`GOMP_CPU_AFFINITY`,
  :ref:`OMP_NESTED`, :ref:`OMP_PLACES`

Reference:
  :openmp:`4.5`, Section 4.4

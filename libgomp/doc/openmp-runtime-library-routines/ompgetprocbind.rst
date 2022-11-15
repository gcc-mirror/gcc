..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_proc_bind:

omp_get_proc_bind -- Whether theads may be moved between CPUs
*************************************************************

Description:
  This functions returns the currently active thread affinity policy, which is
  set via :envvar:`OMP_PROC_BIND`.  Possible values are ``omp_proc_bind_false``,
  ``omp_proc_bind_true``, ``omp_proc_bind_primary``,
  ``omp_proc_bind_master``, ``omp_proc_bind_close`` and ``omp_proc_bind_spread``,
  where ``omp_proc_bind_master`` is an alias for ``omp_proc_bind_primary``.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``omp_proc_bind_t omp_get_proc_bind(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer(kind=omp_proc_bind_kind) function omp_get_proc_bind()``

See also:
  :ref:`OMP_PROC_BIND`, :ref:`OMP_PLACES`, :ref:`GOMP_CPU_AFFINITY`,

Reference:
  :openmp:`4.5`, Section 3.2.22.

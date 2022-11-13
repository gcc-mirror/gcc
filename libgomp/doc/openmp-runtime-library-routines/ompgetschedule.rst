..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_schedule:

omp_get_schedule -- Obtain the runtime scheduling method
********************************************************

Description:
  Obtain the runtime scheduling method.  The :samp:`{kind}` argument will be
  set to the value ``omp_sched_static``, ``omp_sched_dynamic``,
  ``omp_sched_guided`` or ``omp_sched_auto``.  The second argument,
  :samp:`{chunk_size}`, is set to the chunk size.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_get_schedule(omp_sched_t *kind, int *chunk_size);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_get_schedule(kind, chunk_size)``
     * -
       - ``integer(kind=omp_sched_kind) kind``
     * -
       - ``integer chunk_size``

See also:
  :ref:`omp_set_schedule`, :ref:`OMP_SCHEDULE`

Reference:
  :openmp:`4.5`, Section 3.2.13.
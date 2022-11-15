..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_set_schedule:

omp_set_schedule -- Set the runtime scheduling method
*****************************************************

Description:
  Sets the runtime scheduling method.  The :samp:`{kind}` argument can have the
  value ``omp_sched_static``, ``omp_sched_dynamic``,
  ``omp_sched_guided`` or ``omp_sched_auto``.  Except for
  ``omp_sched_auto``, the chunk size is set to the value of
  :samp:`{chunk_size}` if positive, or to the default value if zero or negative.
  For ``omp_sched_auto`` the :samp:`{chunk_size}` argument is ignored.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_set_schedule(omp_sched_t kind, int chunk_size);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_set_schedule(kind, chunk_size)``
     * -
       - ``integer(kind=omp_sched_kind) kind``
     * -
       - ``integer chunk_size``

See also:
  :ref:`omp_get_schedule`
  :ref:`OMP_SCHEDULE`

Reference:
  :openmp:`4.5`, Section 3.2.12.

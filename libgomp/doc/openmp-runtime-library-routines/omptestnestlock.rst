..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_test_nest_lock:

omp_test_nest_lock -- Test and set nested lock if available
***********************************************************

Description:
  Before setting a nested lock, the lock variable must be initialized by
  ``omp_init_nest_lock``.  Contrary to ``omp_set_nest_lock``,
  ``omp_test_nest_lock`` does not block if the lock is not available.
  If the lock is already held by the current thread, the new nesting count
  is returned.  Otherwise, the return value equals zero.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_test_nest_lock(omp_nest_lock_t *lock);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``logical function omp_test_nest_lock(nvar)``
     * -
       - ``integer(omp_nest_lock_kind), intent(inout) :: nvar``

See also:
  :ref:`omp_init_lock`, :ref:`omp_set_lock`, :ref:`omp_set_lock`

Reference:
  :openmp:`4.5`, Section 3.3.6.

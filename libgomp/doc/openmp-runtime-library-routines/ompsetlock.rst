..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_set_lock:

omp_set_lock -- Wait for and set simple lock
********************************************

Description:
  Before setting a simple lock, the lock variable must be initialized by
  ``omp_init_lock``.  The calling thread is blocked until the lock
  is available.  If the lock is already held by the current thread,
  a deadlock occurs.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_set_lock(omp_lock_t *lock);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_set_lock(svar)``
     * -
       - ``integer(omp_lock_kind), intent(inout) :: svar``

See also:
  :ref:`omp_init_lock`, :ref:`omp_test_lock`, :ref:`omp_unset_lock`

Reference:
  :openmp:`4.5`, Section 3.3.4.
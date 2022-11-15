..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_unset_lock:

omp_unset_lock -- Unset simple lock
***********************************

Description:
  A simple lock about to be unset must have been locked by ``omp_set_lock``
  or ``omp_test_lock`` before.  In addition, the lock must be held by the
  thread calling ``omp_unset_lock``.  Then, the lock becomes unlocked.  If one
  or more threads attempted to set the lock before, one of them is chosen to,
  again, set the lock to itself.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_unset_lock(omp_lock_t *lock);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_unset_lock(svar)``
     * -
       - ``integer(omp_lock_kind), intent(inout) :: svar``

See also:
  :ref:`omp_set_lock`, :ref:`omp_test_lock`

Reference:
  :openmp:`4.5`, Section 3.3.5.

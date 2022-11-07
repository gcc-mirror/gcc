..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_unset_nest_lock:

omp_unset_nest_lock -- Unset nested lock
****************************************

Description:
  A nested lock about to be unset must have been locked by ``omp_set_nested_lock``
  or ``omp_test_nested_lock`` before.  In addition, the lock must be held by the
  thread calling ``omp_unset_nested_lock``.  If the nesting count drops to zero, the
  lock becomes unlocked.  If one ore more threads attempted to set the lock before,
  one of them is chosen to, again, set the lock to itself.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_unset_nest_lock(omp_nest_lock_t *lock);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_unset_nest_lock(nvar)``
     * -
       - ``integer(omp_nest_lock_kind), intent(inout) :: nvar``

See also:
  :ref:`omp_set_nest_lock`

Reference:
  :openmp:`4.5`, Section 3.3.5.
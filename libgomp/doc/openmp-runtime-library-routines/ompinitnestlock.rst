..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_init_nest_lock:

omp_init_nest_lock -- Initialize nested lock
********************************************

Description:
  Initialize a nested lock.  After initialization, the lock is in
  an unlocked state and the nesting count is set to zero.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_init_nest_lock(omp_nest_lock_t *lock);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_init_nest_lock(nvar)``
     * -
       - ``integer(omp_nest_lock_kind), intent(out) :: nvar``

See also:
  :ref:`omp_destroy_nest_lock`

Reference:
  :openmp:`4.5`, Section 3.3.1.
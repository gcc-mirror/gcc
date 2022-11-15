..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_destroy_nest_lock:

omp_destroy_nest_lock -- Destroy nested lock
********************************************

Description:
  Destroy a nested lock.  In order to be destroyed, a nested lock must be
  in the unlocked state and its nesting count must equal zero.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_destroy_nest_lock(omp_nest_lock_t *);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_destroy_nest_lock(nvar)``
     * -
       - ``integer(omp_nest_lock_kind), intent(inout) :: nvar``

See also:
  :ref:`omp_init_lock`

Reference:
  :openmp:`4.5`, Section 3.3.3.

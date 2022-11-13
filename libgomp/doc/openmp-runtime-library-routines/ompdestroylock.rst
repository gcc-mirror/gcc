..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_destroy_lock:

omp_destroy_lock -- Destroy simple lock
***************************************

Description:
  Destroy a simple lock.  In order to be destroyed, a simple lock must be
  in the unlocked state.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_destroy_lock(omp_lock_t *lock);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_destroy_lock(svar)``
     * -
       - ``integer(omp_lock_kind), intent(inout) :: svar``

See also:
  :ref:`omp_init_lock`

Reference:
  :openmp:`4.5`, Section 3.3.3.
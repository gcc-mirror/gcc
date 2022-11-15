..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_test_lock:

omp_test_lock -- Test and set simple lock if available
******************************************************

Description:
  Before setting a simple lock, the lock variable must be initialized by
  ``omp_init_lock``.  Contrary to ``omp_set_lock``, ``omp_test_lock``
  does not block if the lock is not available.  This function returns
  ``true`` upon success, ``false`` otherwise.  Here, ``true`` and
  ``false`` represent their language-specific counterparts.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_test_lock(omp_lock_t *lock);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``logical function omp_test_lock(svar)``
     * -
       - ``integer(omp_lock_kind), intent(inout) :: svar``

See also:
  :ref:`omp_init_lock`, :ref:`omp_set_lock`, :ref:`omp_set_lock`

Reference:
  :openmp:`4.5`, Section 3.3.6.

..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_set_dynamic:

omp_set_dynamic -- Enable/disable dynamic teams
***********************************************

Description:
  Enable or disable the dynamic adjustment of the number of threads
  within a team.  The function takes the language-specific equivalent
  of ``true`` and ``false``, where ``true`` enables dynamic
  adjustment of team sizes and ``false`` disables it.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_set_dynamic(int dynamic_threads);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_set_dynamic(dynamic_threads)``
     * -
       - ``logical, intent(in) :: dynamic_threads``

See also:
  :ref:`OMP_DYNAMIC`, :ref:`omp_get_dynamic`

Reference:
  :openmp:`4.5`, Section 3.2.7.

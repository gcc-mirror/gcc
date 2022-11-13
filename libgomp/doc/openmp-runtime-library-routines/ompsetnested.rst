..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_set_nested:

omp_set_nested -- Enable/disable nested parallel regions
********************************************************

Description:
  Enable or disable nested parallel regions, i.e., whether team members
  are allowed to create new teams.  The function takes the language-specific
  equivalent of ``true`` and ``false``, where ``true`` enables
  dynamic adjustment of team sizes and ``false`` disables it.

  Enabling nested parallel regions will also set the maximum number of
  active nested regions to the maximum supported.  Disabling nested parallel
  regions will set the maximum number of active nested regions to one.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_set_nested(int nested);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_set_nested(nested)``
     * -
       - ``logical, intent(in) :: nested``

See also:
  :ref:`omp_get_nested`, :ref:`omp_set_max_active_levels`,
  :ref:`OMP_MAX_ACTIVE_LEVELS`, :ref:`OMP_NESTED`

Reference:
  :openmp:`4.5`, Section 3.2.10.
..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_supported_active_levels:

omp_get_supported_active_levels -- Maximum number of active regions supported
*****************************************************************************

Description:
  This function returns the maximum number of nested, active parallel regions
  supported by this implementation.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_supported_active_levels(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_supported_active_levels()``

See also:
  :ref:`omp_get_max_active_levels`, :ref:`omp_set_max_active_levels`

Reference:
  :openmp:`5.0`, Section 3.2.15.

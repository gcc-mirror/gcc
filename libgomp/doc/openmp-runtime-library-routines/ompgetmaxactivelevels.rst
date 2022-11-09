..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_max_active_levels:

omp_get_max_active_levels -- Current maximum number of active regions
*********************************************************************

Description:
  This function obtains the maximum allowed number of nested, active parallel regions.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_max_active_levels(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_max_active_levels()``

See also:
  :ref:`omp_set_max_active_levels`, :ref:`omp_get_active_level`

Reference:
  :openmp:`4.5`, Section 3.2.16.

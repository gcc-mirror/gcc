..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_num_teams:

omp_get_num_teams -- Number of teams
************************************

Description:
  Returns the number of teams in the current team region.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_num_teams(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_num_teams()``

Reference:
  :openmp:`4.5`, Section 3.2.32.
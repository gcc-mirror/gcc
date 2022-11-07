..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_team_num:

omp_get_team_num -- Get team number
***********************************

Description:
  Returns the team number of the calling thread.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_team_num(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_team_num()``

Reference:
  :openmp:`4.5`, Section 3.2.33.
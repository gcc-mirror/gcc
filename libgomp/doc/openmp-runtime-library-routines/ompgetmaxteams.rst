..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_max_teams:

omp_get_max_teams -- Maximum number of teams of teams region
************************************************************

Description:
  Return the maximum number of teams used for the teams region
  that does not use the clause ``num_teams``.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_max_teams(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_max_teams()``

See also:
  :ref:`omp_set_num_teams`, :ref:`omp_get_num_teams`

Reference:
  :openmp:`5.1`, Section 3.4.4.
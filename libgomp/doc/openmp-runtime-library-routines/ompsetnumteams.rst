..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_set_num_teams:

omp_set_num_teams -- Set upper teams limit for teams construct
**************************************************************

Description:
  Specifies the upper bound for number of teams created by the teams construct
  which does not specify a ``num_teams`` clause.  The
  argument of ``omp_set_num_teams`` shall be a positive integer.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_set_num_teams(int num_teams);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_set_num_teams(num_teams)``
     * -
       - ``integer, intent(in) :: num_teams``

See also:
  :ref:`OMP_NUM_TEAMS`, :ref:`omp_get_num_teams`, :ref:`omp_get_max_teams`

Reference:
  :openmp:`5.1`, Section 3.4.3.

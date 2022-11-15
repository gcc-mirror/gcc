..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_set_teams_thread_limit:

omp_set_teams_thread_limit -- Set upper thread limit for teams construct
************************************************************************

Description:
  Specifies the upper bound for number of threads that will be available
  for each team created by the teams construct which does not specify a
  ``thread_limit`` clause.  The argument of
  ``omp_set_teams_thread_limit`` shall be a positive integer.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_set_teams_thread_limit(int thread_limit);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_set_teams_thread_limit(thread_limit)``
     * -
       - ``integer, intent(in) :: thread_limit``

See also:
  :ref:`OMP_TEAMS_THREAD_LIMIT`, :ref:`omp_get_teams_thread_limit`, :ref:`omp_get_thread_limit`

Reference:
  :openmp:`5.1`, Section 3.4.5.

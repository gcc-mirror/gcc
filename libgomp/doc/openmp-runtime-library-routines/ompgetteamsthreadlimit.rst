..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_teams_thread_limit:

omp_get_teams_thread_limit -- Maximum number of threads imposed by teams
************************************************************************

Description:
  Return the maximum number of threads that will be able to participate in
  each team created by a teams construct.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_teams_thread_limit(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_teams_thread_limit()``

See also:
  :ref:`omp_set_teams_thread_limit`, :ref:`OMP_TEAMS_THREAD_LIMIT`

Reference:
  :openmp:`5.1`, Section 3.4.6.
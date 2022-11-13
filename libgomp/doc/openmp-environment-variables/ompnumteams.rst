..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable

.. _omp_num_teams:

OMP_NUM_TEAMS -- Specifies the number of teams to use by teams region
*********************************************************************

Description:
  Specifies the upper bound for number of teams to use in teams regions
  without explicit ``num_teams`` clause.  The value of this variable shall
  be a positive integer.  If undefined it defaults to 0 which means
  implementation defined upper bound.

See also:
  :ref:`omp_set_num_teams`

Reference:
  :openmp:`5.1`, Section 6.23
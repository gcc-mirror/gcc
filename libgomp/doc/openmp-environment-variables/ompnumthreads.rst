..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable, Implementation specific setting

.. _omp_num_threads:

OMP_NUM_THREADS -- Specifies the number of threads to use
*********************************************************

Description:
  Specifies the default number of threads to use in parallel regions.  The
  value of this variable shall be a comma-separated list of positive integers;
  the value specifies the number of threads to use for the corresponding nested
  level.  Specifying more than one item in the list will automatically enable
  nesting by default.  If undefined one thread per CPU is used.

See also:
  :ref:`omp_set_num_threads`, :ref:`OMP_NESTED`

Reference:
  :openmp:`4.5`, Section 4.2

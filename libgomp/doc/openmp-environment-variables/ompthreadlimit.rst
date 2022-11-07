..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Environment Variable

.. _omp_thread_limit:

OMP_THREAD_LIMIT -- Set the maximum number of threads
*****************************************************

Description:
  Specifies the number of threads to use for the whole program.  The
  value of this variable shall be a positive integer.  If undefined,
  the number of threads is not limited.

See also:
  :ref:`OMP_NUM_THREADS`, :ref:`omp_get_thread_limit`

Reference:
  :openmp:`4.5`, Section 4.10
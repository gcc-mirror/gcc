..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_set_num_threads:

omp_set_num_threads -- Set upper team size limit
************************************************

Description:
  Specifies the number of threads used by default in subsequent parallel
  sections, if those do not specify a ``num_threads`` clause.  The
  argument of ``omp_set_num_threads`` shall be a positive integer.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_set_num_threads(int num_threads);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_set_num_threads(num_threads)``
     * -
       - ``integer, intent(in) :: num_threads``

See also:
  :ref:`OMP_NUM_THREADS`, :ref:`omp_get_num_threads`, :ref:`omp_get_max_threads`

Reference:
  :openmp:`4.5`, Section 3.2.1.
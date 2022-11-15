..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _enabling-openmp:

Enabling OpenMP
---------------

To activate the OpenMP extensions for C/C++ and Fortran, the compile-time
flag :command:`-fopenmp` must be specified.  This enables the OpenMP directive
``#pragma omp`` in C/C++ and ``!$omp`` directives in free form,
``c$omp``, ``*$omp`` and ``!$omp`` directives in fixed form,
``!$`` conditional compilation sentinels in free form and ``c$``,
``*$`` and ``!$`` sentinels in fixed form, for Fortran.  The flag also
arranges for automatic linking of the OpenMP runtime library
(:ref:`runtime-library-routines`).

A complete description of all OpenMP directives may be found in the
`OpenMP Application Program Interface <https://www.openmp.org>`_ manuals.
See also :ref:`openmp-implementation-status`.

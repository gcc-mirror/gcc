..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_in_parallel:

omp_in_parallel -- Whether a parallel region is active
******************************************************

Description:
  This function returns ``true`` if currently running in parallel,
  ``false`` otherwise.  Here, ``true`` and ``false`` represent
  their language-specific counterparts.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_in_parallel(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``logical function omp_in_parallel()``

Reference:
  :openmp:`4.5`, Section 3.2.6.
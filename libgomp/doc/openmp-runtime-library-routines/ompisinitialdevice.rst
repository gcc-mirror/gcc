..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_is_initial_device:

omp_is_initial_device -- Whether executing on the host device
*************************************************************

Description:
  This function returns ``true`` if currently running on the host device,
  ``false`` otherwise.  Here, ``true`` and ``false`` represent
  their language-specific counterparts.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_is_initial_device(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``logical function omp_is_initial_device()``

Reference:
  :openmp:`4.5`, Section 3.2.34.
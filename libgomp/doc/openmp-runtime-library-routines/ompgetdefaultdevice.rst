..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_default_device:

omp_get_default_device -- Get the default device for target regions
*******************************************************************

Description:
  Get the default device for target regions without device clause.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_default_device(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_default_device()``

See also:
  :ref:`OMP_DEFAULT_DEVICE`, :ref:`omp_set_default_device`

Reference:
  :openmp:`4.5`, Section 3.2.30.

..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_set_default_device:

omp_set_default_device -- Set the default device for target regions
*******************************************************************

Description:
  Set the default device for target regions without device clause.  The argument
  shall be a nonnegative device number.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_set_default_device(int device_num);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_set_default_device(device_num)``
     * -
       - ``integer device_num``

See also:
  :ref:`OMP_DEFAULT_DEVICE`, :ref:`omp_get_default_device`

Reference:
  :openmp:`4.5`, Section 3.2.29.
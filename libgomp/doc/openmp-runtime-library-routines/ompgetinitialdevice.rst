..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_initial_device:

omp_get_initial_device -- Return device number of initial device
****************************************************************

Description:
  This function returns a device number that represents the host device.
  For OpenMP 5.1, this must be equal to the value returned by the
  ``omp_get_num_devices`` function.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_initial_device(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_initial_device()``

See also:
  :ref:`omp_get_num_devices`

Reference:
  :openmp:`4.5`, Section 3.2.35.

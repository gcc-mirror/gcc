..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_num_procs:

omp_get_num_procs -- Number of processors online
************************************************

Description:
  Returns the number of processors online on that device.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_num_procs(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_num_procs()``

Reference:
  :openmp:`4.5`, Section 3.2.5.
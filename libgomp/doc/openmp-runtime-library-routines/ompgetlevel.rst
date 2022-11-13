..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_level:

omp_get_level -- Obtain the current nesting level
*************************************************

Description:
  This function returns the nesting level for the parallel blocks,
  which enclose the calling call.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_level(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_level()``

See also:
  :ref:`omp_get_active_level`

Reference:
  :openmp:`4.5`, Section 3.2.17.
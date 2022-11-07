..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_wtick:

omp_get_wtick -- Get timer precision
************************************

Description:
  Gets the timer precision, i.e., the number of seconds between two
  successive clock ticks.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``double omp_get_wtick(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``double precision function omp_get_wtick()``

See also:
  :ref:`omp_get_wtime`

Reference:
  :openmp:`4.5`, Section 3.4.2.
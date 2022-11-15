..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_thread_num:

omp_get_thread_num -- Current thread ID
***************************************

Description:
  Returns a unique thread identification number within the current team.
  In a sequential parts of the program, ``omp_get_thread_num``
  always returns 0.  In parallel regions the return value varies
  from 0 to ``omp_get_num_threads`` -1 inclusive.  The return
  value of the primary thread of a team is always 0.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_thread_num(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_thread_num()``

See also:
  :ref:`omp_get_num_threads`, :ref:`omp_get_ancestor_thread_num`

Reference:
  :openmp:`4.5`, Section 3.2.4.

..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_ancestor_thread_num:

omp_get_ancestor_thread_num -- Ancestor thread ID
*************************************************

Description:
  This function returns the thread identification number for the given
  nesting level of the current thread.  For values of :samp:`{level}` outside
  zero to ``omp_get_level`` -1 is returned; if :samp:`{level}` is
  ``omp_get_level`` the result is identical to ``omp_get_thread_num``.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_ancestor_thread_num(int level);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_ancestor_thread_num(level)``
     * -
       - ``integer level``

See also:
  :ref:`omp_get_level`, :ref:`omp_get_thread_num`, :ref:`omp_get_team_size`

Reference:
  :openmp:`4.5`, Section 3.2.18.

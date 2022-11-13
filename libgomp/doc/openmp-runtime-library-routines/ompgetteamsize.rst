..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_team_size:

omp_get_team_size -- Number of threads in a team
************************************************

Description:
  This function returns the number of threads in a thread team to which
  either the current thread or its ancestor belongs.  For values of :samp:`{level}`
  outside zero to ``omp_get_level``, -1 is returned; if :samp:`{level}` is zero,
  1 is returned, and for ``omp_get_level``, the result is identical
  to ``omp_get_num_threads``.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_team_size(int level);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_team_size(level)``
     * -
       - ``integer level``

See also:
  :ref:`omp_get_num_threads`, :ref:`omp_get_level`, :ref:`omp_get_ancestor_thread_num`

Reference:
  :openmp:`4.5`, Section 3.2.19.
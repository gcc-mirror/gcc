..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_num_threads:

omp_get_num_threads -- Size of the active team
**********************************************

Description:
  Returns the number of threads in the current team.  In a sequential section of
  the program ``omp_get_num_threads`` returns 1.

  The default team size may be initialized at startup by the
  :envvar:`OMP_NUM_THREADS` environment variable.  At runtime, the size
  of the current team may be set either by the ``NUM_THREADS``
  clause or by ``omp_set_num_threads``.  If none of the above were
  used to define a specific value and :envvar:`OMP_DYNAMIC` is disabled,
  one thread per CPU online is used.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_num_threads(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_num_threads()``

See also:
  :ref:`omp_get_max_threads`, :ref:`omp_set_num_threads`, :ref:`OMP_NUM_THREADS`

Reference:
  :openmp:`4.5`, Section 3.2.2.

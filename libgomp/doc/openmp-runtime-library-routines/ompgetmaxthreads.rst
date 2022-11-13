..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_max_threads:

omp_get_max_threads -- Maximum number of threads of parallel region
*******************************************************************

Description:
  Return the maximum number of threads used for the current parallel region
  that does not use the clause ``num_threads``.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_max_threads(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_max_threads()``

See also:
  :ref:`omp_set_num_threads`, :ref:`omp_set_dynamic`, :ref:`omp_get_thread_limit`

Reference:
  :openmp:`4.5`, Section 3.2.3.
..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_dynamic:

omp_get_dynamic -- Dynamic teams setting
****************************************

Description:
  This function returns ``true`` if enabled, ``false`` otherwise.
  Here, ``true`` and ``false`` represent their language-specific
  counterparts.

  The dynamic team setting may be initialized at startup by the
  :envvar:`OMP_DYNAMIC` environment variable or at runtime using
  ``omp_set_dynamic``.  If undefined, dynamic adjustment is
  disabled by default.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_dynamic(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``logical function omp_get_dynamic()``

See also:
  :ref:`omp_set_dynamic`, :ref:`OMP_DYNAMIC`

Reference:
  :openmp:`4.5`, Section 3.2.8.

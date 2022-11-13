..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_nested:

omp_get_nested -- Nested parallel regions
*****************************************

Description:
  This function returns ``true`` if nested parallel regions are
  enabled, ``false`` otherwise.  Here, ``true`` and ``false``
  represent their language-specific counterparts.

  The state of nested parallel regions at startup depends on several
  environment variables.  If :envvar:`OMP_MAX_ACTIVE_LEVELS` is defined
  and is set to greater than one, then nested parallel regions will be
  enabled.  If not defined, then the value of the :envvar:`OMP_NESTED`
  environment variable will be followed if defined.  If neither are
  defined, then if either :envvar:`OMP_NUM_THREADS` or :envvar:`OMP_PROC_BIND`
  are defined with a list of more than one value, then nested parallel
  regions are enabled.  If none of these are defined, then nested parallel
  regions are disabled by default.

  Nested parallel regions can be enabled or disabled at runtime using
  ``omp_set_nested``, or by setting the maximum number of nested
  regions with ``omp_set_max_active_levels`` to one to disable, or
  above one to enable.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_nested(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``logical function omp_get_nested()``

See also:
  :ref:`omp_set_max_active_levels`, :ref:`omp_set_nested`,
  :ref:`OMP_MAX_ACTIVE_LEVELS`, :ref:`OMP_NESTED`

Reference:
  :openmp:`4.5`, Section 3.2.11.
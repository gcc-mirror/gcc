..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _omp_get_cancellation:

omp_get_cancellation -- Whether cancellation support is enabled
***************************************************************

Description:
  This function returns ``true`` if cancellation is activated, ``false``
  otherwise.  Here, ``true`` and ``false`` represent their language-specific
  counterparts.  Unless :envvar:`OMP_CANCELLATION` is set true, cancellations are
  deactivated.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_cancellation(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``logical function omp_get_cancellation()``

See also:
  :ref:`OMP_CANCELLATION`

Reference:
  :openmp:`4.5`, Section 3.2.9.

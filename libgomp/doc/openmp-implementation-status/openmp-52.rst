..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _openmp-5.2:

OpenMP 5.2
**********

New features listed in Appendix B of the OpenMP specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1
   :widths: 50 10 25

   * - Description
     - Status
     - Comments

   * - ``omp_in_explicit_task`` routine and *explicit-task-var* ICV
     - Y
     -
   * - ``omp`` / ``ompx`` / ``omx`` sentinels and ``omp_`` / ``ompx_`` namespaces
     - N/A
     - warning for ``ompx/omx`` sentinels [#f1]_
   * - Clauses on ``end`` directive can be on directive
     - N
     -
   * - Deprecation of no-argument ``destroy`` clause on ``depobj``
     - N
     -
   * - ``linear`` clause syntax changes and ``step`` modifier
     - Y
     -
   * - Deprecation of minus operator for reductions
     - N
     -
   * - Deprecation of separating ``map`` modifiers without comma
     - N
     -
   * - ``declare mapper`` with iterator and ``present`` modifiers
     - N
     -
   * - If a matching mapped list item is not found in the data environment, the pointer retains its original value
     - N
     -
   * - New ``enter`` clause as alias for ``to`` on declare target directive
     - Y
     -
   * - Deprecation of ``to`` clause on declare target directive
     - N
     -
   * - Extended list of directives permitted in Fortran pure procedures
     - N
     -
   * - New ``allocators`` directive for Fortran
     - N
     -
   * - Deprecation of ``allocate`` directive for Fortran allocatables/pointers
     - N
     -
   * - Optional paired ``end`` directive with ``dispatch``
     - N
     -
   * - New ``memspace`` and ``traits`` modifiers for ``uses_allocators``
     - N
     -
   * - Deprecation of traits array following the allocator_handle expression in ``uses_allocators``
     - N
     -
   * - New ``otherwise`` clause as alias for ``default`` on metadirectives
     - N
     -
   * - Deprecation of ``default`` clause on metadirectives
     - N
     -
   * - Deprecation of delimited form of ``declare target``
     - N
     -
   * - Reproducible semantics changed for ``order(concurrent)``
     - N
     -
   * - ``allocate`` and ``firstprivate`` clauses on ``scope``
     - Y
     -
   * - ``ompt_callback_work``
     - N
     -
   * - Default map-type for ``map`` clause in ``target enter/exit data``
     - Y
     -
   * - New ``doacross`` clause as alias for ``depend`` with ``source`` / ``sink`` modifier
     - Y
     -
   * - Deprecation of ``depend`` with ``source`` / ``sink`` modifier
     - N
     -
   * - ``omp_cur_iteration`` keyword
     - Y
     -

.. [#f1] The ``ompx`` sentinel as C/C++ pragma and C++ attributes are warned for with ``-Wunknown-pragmas`` (implied by ``-Wall``) and ``-Wattributes`` (enabled by default), respectively; for Fortran free-source code, there is a warning enabled by default and, for fixed-source code, the ``omx`` sentinel is warned for with with ``-Wsurprising`` (enabled by ``-Wall``).  Unknown clauses are always rejected with an error.

Other new OpenMP 5.2 features
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1
   :widths: 50 10 25

   * - Description
     - Status
     - Comments

   * - For Fortran, optional comma between directive and clause
     - N
     -
   * - Conforming device numbers and ``omp_initial_device`` and ``omp_invalid_device`` enum/PARAMETER
     - Y
     -
   * - Initial value of *default-device-var* ICV with ``OMP_TARGET_OFFLOAD=mandatory``
     - N
     -
   * - *interop_types* in any position of the modifier list for the ``init`` clause of the ``interop`` construct
     - N
     -

.. -
   OpenMP Runtime Library Routines
   -
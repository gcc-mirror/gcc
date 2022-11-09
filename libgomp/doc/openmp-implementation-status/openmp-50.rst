..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _openmp-5.0:

OpenMP 5.0
**********

New features listed in Appendix B of the OpenMP specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. This list is sorted as in OpenMP 5.1's B.3 not as in OpenMP 5.0's B.2

.. list-table::
   :header-rows: 1
   :widths: 50 10 25

   * - Description
     - Status
     - Comments

   * - Array shaping
     - N
     -
   * - Array sections with non-unit strides in C and C++
     - N
     -
   * - Iterators
     - Y
     -
   * - ``metadirective`` directive
     - N
     -
   * - ``declare variant`` directive
     - P
     - *simd* traits not handled correctly
   * - *target-offload-var* ICV and ``OMP_TARGET_OFFLOAD`` env variable
     - Y
     -
   * - Nested-parallel changes to *max-active-levels-var* ICV
     - Y
     -
   * - ``requires`` directive
     - P
     - complete but no non-host devices provides ``unified_address``, ``unified_shared_memory`` or ``reverse_offload``
   * - ``teams`` construct outside an enclosing target region
     - Y
     -
   * - Non-rectangular loop nests
     - Y
     -
   * - ``!=`` as relational-op in canonical loop form for C/C++
     - Y
     -
   * - ``nonmonotonic`` as default loop schedule modifier for worksharing-loop constructs
     - Y
     -
   * - Collapse of associated loops that are imperfectly nested loops
     - N
     -
   * - Clauses ``if``, ``nontemporal`` and ``order(concurrent)`` in ``simd`` construct
     - Y
     -
   * - ``atomic`` constructs in ``simd``
     - Y
     -
   * - ``loop`` construct
     - Y
     -
   * - ``order(concurrent)`` clause
     - Y
     -
   * - ``scan`` directive and ``in_scan`` modifier for the ``reduction`` clause
     - Y
     -
   * - ``in_reduction`` clause on ``task`` constructs
     - Y
     -
   * - ``in_reduction`` clause on ``target`` constructs
     - P
     - ``nowait`` only stub
   * - ``task_reduction`` clause with ``taskgroup``
     - Y
     -
   * - ``task`` modifier to ``reduction`` clause
     - Y
     -
   * - ``affinity`` clause to ``task`` construct
     - Y
     - Stub only
   * - ``detach`` clause to ``task`` construct
     - Y
     -
   * - ``omp_fulfill_event`` runtime routine
     - Y
     -
   * - ``reduction`` and ``in_reduction`` clauses on ``taskloop`` and ``taskloop simd`` constructs
     - Y
     -
   * - ``taskloop`` construct cancelable by ``cancel`` construct
     - Y
     -
   * - ``mutexinoutset`` *dependence-type* for ``depend`` clause
     - Y
     -
   * - Predefined memory spaces, memory allocators, allocator traits
     - Y
     - Some are only stubs
   * - Memory management routines
     - Y
     -
   * - ``allocate`` directive
     - N
     -
   * - ``allocate`` clause
     - P
     - Initial support
   * - ``use_device_addr`` clause on ``target data``
     - Y
     -
   * - ``ancestor`` modifier on ``device`` clause
     - Y
     - See comment for ``requires``
   * - Implicit declare target directive
     - Y
     -
   * - Discontiguous array section with ``target update`` construct
     - N
     -
   * - C/C++'s lvalue expressions in ``to``, ``from`` and ``map`` clauses
     - N
     -
   * - C/C++'s lvalue expressions in ``depend`` clauses
     - Y
     -
   * - Nested ``declare target`` directive
     - Y
     -
   * - Combined ``master`` constructs
     - Y
     -
   * - ``depend`` clause on ``taskwait``
     - Y
     -
   * - Weak memory ordering clauses on ``atomic`` and ``flush`` construct
     - Y
     -
   * - ``hint`` clause on the ``atomic`` construct
     - Y
     - Stub only
   * - ``depobj`` construct and depend objects
     - Y
     -
   * - Lock hints were renamed to synchronization hints
     - Y
     -
   * - ``conditional`` modifier to ``lastprivate`` clause
     - Y
     -
   * - Map-order clarifications
     - P
     -
   * - ``close`` *map-type-modifier*
     - Y
     -
   * - Mapping C/C++ pointer variables and to assign the address of device memory mapped by an array section
     - P
     -
   * - Mapping of Fortran pointer and allocatable variables, including pointer and allocatable components of variables
     - P
     - Mapping of vars with allocatable components unsupported
   * - ``defaultmap`` extensions
     - Y
     -
   * - ``declare mapper`` directive
     - N
     -
   * - ``omp_get_supported_active_levels`` routine
     - Y
     -
   * - Runtime routines and environment variables to display runtime thread affinity information
     - Y
     -
   * - ``omp_pause_resource`` and ``omp_pause_resource_all`` runtime routines
     - Y
     -
   * - ``omp_get_device_num`` runtime routine
     - Y
     -
   * - OMPT interface
     - N
     -
   * - OMPD interface
     - N
     -

Other new OpenMP 5.0 features
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1
   :widths: 50 10 25

   * - Description
     - Status
     - Comments

   * - Supporting C++'s range-based for loop
     - Y
     -

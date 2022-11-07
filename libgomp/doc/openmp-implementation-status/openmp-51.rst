..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _openmp-5.1:

OpenMP 5.1
**********

New features listed in Appendix B of the OpenMP specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1
   :widths: 50 10 25

   * - Description
     - Status
     - Comments

   * - OpenMP directive as C++ attribute specifiers
     - Y
     -
   * - ``omp_all_memory`` reserved locator
     - Y
     -
   * - *target_device trait* in OpenMP Context
     - N
     -
   * - ``target_device`` selector set in context selectors
     - N
     -
   * - C/C++'s ``declare variant`` directive: elision support of preprocessed code
     - N
     -
   * - ``declare variant`` : new clauses ``adjust_args`` and ``append_args``
     - N
     -
   * - ``dispatch`` construct
     - N
     -
   * - device-specific ICV settings with environment variables
     - Y
     -
   * - ``assume`` directive
     - Y
     -
   * - ``nothing`` directive
     - Y
     -
   * - ``error`` directive
     - Y
     -
   * - ``masked`` construct
     - Y
     -
   * - ``scope`` directive
     - Y
     -
   * - Loop transformation constructs
     - N
     -
   * - ``strict`` modifier in the ``grainsize`` and ``num_tasks`` clauses of the ``taskloop`` construct
     - Y
     -
   * - ``align`` clause/modifier in ``allocate`` directive/clause and ``allocator`` directive
     - P
     - C/C++ on clause only
   * - ``thread_limit`` clause to ``target`` construct
     - Y
     -
   * - ``has_device_addr`` clause to ``target`` construct
     - Y
     -
   * - Iterators in ``target update`` motion clauses and ``map`` clauses
     - N
     -
   * - Indirect calls to the device version of a procedure or function in ``target`` regions
     - N
     -
   * - ``interop`` directive
     - N
     -
   * - ``omp_interop_t`` object support in runtime routines
     - N
     -
   * - ``nowait`` clause in ``taskwait`` directive
     - Y
     -
   * - Extensions to the ``atomic`` directive
     - Y
     -
   * - ``seq_cst`` clause on a ``flush`` construct
     - Y
     -
   * - ``inoutset`` argument to the ``depend`` clause
     - Y
     -
   * - ``private`` and ``firstprivate`` argument to ``default`` clause in C and C++
     - Y
     -
   * - ``present`` argument to ``defaultmap`` clause
     - N
     -
   * - ``omp_set_num_teams``, ``omp_set_teams_thread_limit``, ``omp_get_max_teams``, ``omp_get_teams_thread_limit`` runtime routines
     - Y
     -
   * - ``omp_target_is_accessible`` runtime routine
     - Y
     -
   * - ``omp_target_memcpy_async`` and ``omp_target_memcpy_rect_async`` runtime routines
     - Y
     -
   * - ``omp_get_mapped_ptr`` runtime routine
     - Y
     -
   * - ``omp_calloc``, ``omp_realloc``, ``omp_aligned_alloc`` and ``omp_aligned_calloc`` runtime routines
     - Y
     -
   * - ``omp_alloctrait_key_t`` enum: ``omp_atv_serialized`` added, ``omp_atv_default`` changed
     - Y
     -
   * - ``omp_display_env`` runtime routine
     - Y
     -
   * - ``ompt_scope_endpoint_t`` enum: ``ompt_scope_beginend``
     - N
     -
   * - ``ompt_sync_region_t`` enum additions
     - N
     -
   * - ``ompt_state_t`` enum: ``ompt_state_wait_barrier_implementation`` and ``ompt_state_wait_barrier_teams``
     - N
     -
   * - ``ompt_callback_target_data_op_emi_t``, ``ompt_callback_target_emi_t``, ``ompt_callback_target_map_emi_t`` and ``ompt_callback_target_submit_emi_t``
     - N
     -
   * - ``ompt_callback_error_t`` type
     - N
     -
   * - ``OMP_PLACES`` syntax extensions
     - Y
     -
   * - ``OMP_NUM_TEAMS`` and ``OMP_TEAMS_THREAD_LIMIT`` environment variables
     - Y
     -

Other new OpenMP 5.1 features
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1
   :widths: 50 10 25

   * - Description
     - Status
     - Comments

   * - Support of strictly structured blocks in Fortran
     - Y
     -
   * - Support of structured block sequences in C/C++
     - Y
     -
   * - ``unconstrained`` and ``reproducible`` modifiers on ``order`` clause
     - Y
     -
   * - Support ``begin/end declare target`` syntax in C/C++
     - Y
     -
   * - Pointer predetermined firstprivate getting initialized to address of matching mapped list item per 5.1, Sect. 2.21.7.2
     - N
     -
   * - For Fortran, diagnose placing declarative before/between ``USE``, ``IMPORT``, and ``IMPLICIT`` as invalid
     - N
     -
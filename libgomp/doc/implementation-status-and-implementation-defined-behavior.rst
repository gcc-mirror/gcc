..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Implementation Status and Implementation-Defined Behavior
*********************************************************

We're implementing the OpenACC Profiling Interface as defined by the
OpenACC 2.6 specification.  We're clarifying some aspects here as
*implementation-defined behavior*, while they're still under
discussion within the OpenACC Technical Committee.

This implementation is tuned to keep the performance impact as low as
possible for the (very common) case that the Profiling Interface is
not enabled.  This is relevant, as the Profiling Interface affects all
the *hot* code paths (in the target code, not in the offloaded
code).  Users of the OpenACC Profiling Interface can be expected to
understand that performance will be impacted to some degree once the
Profiling Interface has gotten enabled: for example, because of the
*runtime* (libgomp) calling into a third-party *library* for
every event that has been registered.

We're not yet accounting for the fact that OpenACC events may
occur during event processing.
We just handle one case specially, as required by CUDA 9.0
:command:`nvprof`, that ``acc_get_device_type``
(:ref:`acc_get_device_type`)) may be called from
``acc_ev_device_init_start``, ``acc_ev_device_init_end``
callbacks.

We're not yet implementing initialization via a
``acc_register_library`` function that is either statically linked
in, or dynamically via :envvar:`LD_PRELOAD`.
Initialization via ``acc_register_library`` functions dynamically
loaded via the :envvar:`ACC_PROFLIB` environment variable does work, as
does directly calling ``acc_prof_register``,
``acc_prof_unregister``, ``acc_prof_lookup``.

As currently there are no inquiry functions defined, calls to
``acc_prof_lookup`` will always return ``NULL``.

There aren't separate *start*, *stop* events defined for the
event types ``acc_ev_create``, ``acc_ev_delete``,
``acc_ev_alloc``, ``acc_ev_free``.  It's not clear if these
should be triggered before or after the actual device-specific call is
made.  We trigger them after.

Remarks about data provided to callbacks:

acc_prof_info.event_type
  It's not clear if for *nested* event callbacks (for example,
  ``acc_ev_enqueue_launch_start`` as part of a parent compute
  construct), this should be set for the nested event
  (``acc_ev_enqueue_launch_start``), or if the value of the parent
  construct should remain (``acc_ev_compute_construct_start``).  In
  this implementation, the value will generally correspond to the
  innermost nested event type.

acc_prof_info.device_type
  * For ``acc_ev_compute_construct_start``, and in presence of an
    ``if`` clause with *false* argument, this will still refer to
    the offloading device type.
    It's not clear if that's the expected behavior.

  * Complementary to the item before, for
    ``acc_ev_compute_construct_end``, this is set to
    ``acc_device_host`` in presence of an ``if`` clause with
    *false* argument.
    It's not clear if that's the expected behavior.

acc_prof_info.thread_id
  Always ``-1`` ; not yet implemented.

acc_prof_info.async
  * Not yet implemented correctly for
    ``acc_ev_compute_construct_start``.

  * In a compute construct, for host-fallback
    execution/ ``acc_device_host`` it will always be
    ``acc_async_sync``.
    It's not clear if that's the expected behavior.

  * For ``acc_ev_device_init_start`` and ``acc_ev_device_init_end``,
    it will always be ``acc_async_sync``.
    It's not clear if that's the expected behavior.

acc_prof_info.async_queue
  There is no limited number of asynchronous queues in libgomp.
  This will always have the same value as ``acc_prof_info.async``.

acc_prof_info.src_file
  Always ``NULL`` ; not yet implemented.

acc_prof_info.func_name
  Always ``NULL`` ; not yet implemented.

acc_prof_info.line_no
  Always ``-1`` ; not yet implemented.

acc_prof_info.end_line_no
  Always ``-1`` ; not yet implemented.

acc_prof_info.func_line_no
  Always ``-1`` ; not yet implemented.

acc_prof_info.func_end_line_no
  Always ``-1`` ; not yet implemented.

acc_event_info.event_type, acc_event_info.*.event_type
  Relating to ``acc_prof_info.event_type`` discussed above, in this
  implementation, this will always be the same value as
  ``acc_prof_info.event_type``.

acc_event_info.\*.parent_construct
  * Will be ``acc_construct_parallel`` for all OpenACC compute
    constructs as well as many OpenACC Runtime API calls; should be the
    one matching the actual construct, or
    ``acc_construct_runtime_api``, respectively.

  * Will be ``acc_construct_enter_data`` or
    ``acc_construct_exit_data`` when processing variable mappings
    specified in OpenACC *declare* directives; should be
    ``acc_construct_declare``.

  * For implicit ``acc_ev_device_init_start``,
    ``acc_ev_device_init_end``, and explicit as well as implicit
    ``acc_ev_alloc``, ``acc_ev_free``,
    ``acc_ev_enqueue_upload_start``, ``acc_ev_enqueue_upload_end``,
    ``acc_ev_enqueue_download_start``, and
    ``acc_ev_enqueue_download_end``, will be
    ``acc_construct_parallel`` ; should reflect the real parent
    construct.

acc_event_info.\*.implicit
  For ``acc_ev_alloc``, ``acc_ev_free``,
  ``acc_ev_enqueue_upload_start``, ``acc_ev_enqueue_upload_end``,
  ``acc_ev_enqueue_download_start``, and
  ``acc_ev_enqueue_download_end``, this currently will be ``1``
  also for explicit usage.

acc_event_info.data_event.var_name
  Always ``NULL`` ; not yet implemented.

acc_event_info.data_event.host_ptr
  For ``acc_ev_alloc``, and ``acc_ev_free``, this is always
  ``NULL``.

typedef union acc_api_info
  ... as printed in 5.2.3. Third Argument: API-Specific
  Information.  This should obviously be ``typedef struct
  acc_api_info``.

acc_api_info.device_api
  Possibly not yet implemented correctly for
  ``acc_ev_compute_construct_start``,
  ``acc_ev_device_init_start``, ``acc_ev_device_init_end`` :
  will always be ``acc_device_api_none`` for these event types.
  For ``acc_ev_enter_data_start``, it will be
  ``acc_device_api_none`` in some cases.

acc_api_info.device_type
  Always the same as ``acc_prof_info.device_type``.

acc_api_info.vendor
  Always ``-1`` ; not yet implemented.

acc_api_info.device_handle
  Always ``NULL`` ; not yet implemented.

acc_api_info.context_handle
  Always ``NULL`` ; not yet implemented.

acc_api_info.async_handle
  Always ``NULL`` ; not yet implemented.

Remarks about certain event types:

acc_ev_device_init_start, acc_ev_device_init_end
  *
    .. See 'DEVICE_INIT_INSIDE_COMPUTE_CONSTRUCT' in
       'libgomp.oacc-c-c++-common/acc_prof-kernels-1.c',
       'libgomp.oacc-c-c++-common/acc_prof-parallel-1.c'.

    When a compute construct triggers implicit
    ``acc_ev_device_init_start`` and ``acc_ev_device_init_end``
    events, they currently aren't *nested within* the corresponding
    ``acc_ev_compute_construct_start`` and
    ``acc_ev_compute_construct_end``, but they're currently observed
    *before* ``acc_ev_compute_construct_start``.
    It's not clear what to do: the standard asks us provide a lot of
    details to the ``acc_ev_compute_construct_start`` callback, without
    (implicitly) initializing a device before?

  * Callbacks for these event types will not be invoked for calls to the
    ``acc_set_device_type`` and ``acc_set_device_num`` functions.
    It's not clear if they should be.

acc_ev_enter_data_start, acc_ev_enter_data_end, acc_ev_exit_data_start, acc_ev_exit_data_end
  * Callbacks for these event types will also be invoked for OpenACC
    *host_data* constructs.
    It's not clear if they should be.

  * Callbacks for these event types will also be invoked when processing
    variable mappings specified in OpenACC *declare* directives.
    It's not clear if they should be.

Callbacks for the following event types will be invoked, but dispatch
and information provided therein has not yet been thoroughly reviewed:

* ``acc_ev_alloc``

* ``acc_ev_free``

* ``acc_ev_update_start``, ``acc_ev_update_end``

* ``acc_ev_enqueue_upload_start``, ``acc_ev_enqueue_upload_end``

* ``acc_ev_enqueue_download_start``, ``acc_ev_enqueue_download_end``

During device initialization, and finalization, respectively,
callbacks for the following event types will not yet be invoked:

* ``acc_ev_alloc``

* ``acc_ev_free``

Callbacks for the following event types have not yet been implemented,
so currently won't be invoked:

* ``acc_ev_device_shutdown_start``, ``acc_ev_device_shutdown_end``

* ``acc_ev_runtime_shutdown``

* ``acc_ev_create``, ``acc_ev_delete``

* ``acc_ev_wait_start``, ``acc_ev_wait_end``

For the following runtime library functions, not all expected
callbacks will be invoked (mostly concerning implicit device
initialization):

* ``acc_get_num_devices``

* ``acc_set_device_type``

* ``acc_get_device_type``

* ``acc_set_device_num``

* ``acc_get_device_num``

* ``acc_init``

* ``acc_shutdown``

Aside from implicit device initialization, for the following runtime
library functions, no callbacks will be invoked for shared-memory
offloading devices (it's not clear if they should be):

* ``acc_malloc``

* ``acc_free``

* ``acc_copyin``, ``acc_present_or_copyin``, ``acc_copyin_async``

* ``acc_create``, ``acc_present_or_create``, ``acc_create_async``

* ``acc_copyout``, ``acc_copyout_async``, ``acc_copyout_finalize``, ``acc_copyout_finalize_async``

* ``acc_delete``, ``acc_delete_async``, ``acc_delete_finalize``, ``acc_delete_finalize_async``

* ``acc_update_device``, ``acc_update_device_async``

* ``acc_update_self``, ``acc_update_self_async``

* ``acc_map_data``, ``acc_unmap_data``

* ``acc_memcpy_to_device``, ``acc_memcpy_to_device_async``

* ``acc_memcpy_from_device``, ``acc_memcpy_from_device_async``

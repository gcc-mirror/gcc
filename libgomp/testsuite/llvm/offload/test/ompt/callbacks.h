#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

// Tool related code below
#include <omp-tools.h>

#define _TOOL_PREFIX ""

static const char *ompt_target_data_op_t_values[] = {
    /*  0 */ "",
    /*  1 */ "ompt_target_data_alloc",
    /*  2 */ "ompt_target_data_transfer_to_device",
    /*  3 */ "ompt_target_data_transfer_from_device",
    /*  4 */ "ompt_target_data_delete",
    /*  5 */ "ompt_target_data_associate",
    /*  6 */ "ompt_target_data_disassociate",
    /*  7 */ "ompt_target_data_transfer",
    /*  8 */ "ompt_target_data_memset",
    /*  9 */ "ompt_target_data_transfer_rect"
    /* 10 */ "",
    /* 11 */ "",
    /* 12 */ "",
    /* 13 */ "",
    /* 14 */ "",
    /* 15 */ "",
    /* 16 */ "",
    /* 17 */ "ompt_target_data_alloc_async",
    /* 18 */ "ompt_target_data_transfer_to_device_async",
    /* 19 */ "ompt_target_data_transfer_from_device_async",
    /* 20 */ "ompt_target_data_delete_async"
    /* 21 */ "",
    /* 22 */ "",
    /* 23 */ "ompt_target_data_transfer_async",
    /* 24 */ "ompt_target_data_memset_async",
    /* 25 */ "ompt_target_data_transfer_rect_async"
    
};

static const char *ompt_scope_endpoint_t_values[] = {
    "", "ompt_scope_begin", "ompt_scope_end", "ompt_scope_beginend"};

static const char *ompt_target_t_values[] = {
    /*  0 */ "",
    /*  1 */ "ompt_target",
    /*  2 */ "ompt_target_enter_data",
    /*  3 */ "ompt_target_exit_data",
    /*  4 */ "ompt_target_update",
    /*  5 */ "",
    /*  6 */ "",
    /*  7 */ "",
    /*  8 */ "",
    /*  9 */ "ompt_target_nowait",
    /* 10 */ "ompt_target_enter_data_nowait",
    /* 11 */ "ompt_target_exit_data_nowait",
    /* 12 */ "ompt_target_update_nowait"};

static const char *ompt_task_status_t_values[] = {
    "ompt_task_UNDEFINED",
    "ompt_task_complete", // 1
    "ompt_task_yield", // 2
    "ompt_task_cancel", // 3
    "ompt_task_detach", // 4
    "ompt_task_early_fulfill", // 5
    "ompt_task_late_fulfill", // 6
    "ompt_task_switch", // 7
    "ompt_taskwait_complete" // 8
};

static const char *ompt_dependence_type_t_values[36] = {
    "ompt_dependence_type_UNDEFINED",
    "ompt_dependence_type_in", // 1
    "ompt_dependence_type_out", // 2
    "ompt_dependence_type_inout", // 3
    "ompt_dependence_type_mutexinoutset", // 4
    "ompt_dependence_type_source", // 5
    "ompt_dependence_type_sink", // 6
    "ompt_dependence_type_inoutset", // 7
    "",
    "",
    "",
    "",
    "",
    "", // 8-13
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "", // 14-23
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "", // 24-33
    "ompt_dependence_type_out_all_memory", // 34
    "ompt_dependence_type_inout_all_memory" // 35
};


// For EMI callbacks
ompt_id_t next_op_id = 0x8000000000000001;

static void format_task_type(int type, char *buffer) {
  char *progress = buffer;
  if (type & ompt_task_initial)
    progress += sprintf(progress, "ompt_task_initial");
  if (type & ompt_task_implicit)
    progress += sprintf(progress, "ompt_task_implicit");
  if (type & ompt_task_explicit)
    progress += sprintf(progress, "ompt_task_explicit");
  if (type & ompt_task_target)
    progress += sprintf(progress, "ompt_task_target");
  if (type & ompt_task_taskwait)
    progress += sprintf(progress, "ompt_task_taskwait");
  if (type & ompt_task_undeferred)
    progress += sprintf(progress, "|ompt_task_undeferred");
  if (type & ompt_task_untied)
    progress += sprintf(progress, "|ompt_task_untied");
  if (type & ompt_task_final)
    progress += sprintf(progress, "|ompt_task_final");
  if (type & ompt_task_mergeable)
    progress += sprintf(progress, "|ompt_task_mergeable");
  if (type & ompt_task_merged)
    progress += sprintf(progress, "|ompt_task_merged");
}

// OMPT callbacks

// Synchronous callbacks
static void on_ompt_callback_device_initialize(int device_num, const char *type,
                                               ompt_device_t *device,
                                               ompt_function_lookup_t lookup,
                                               const char *documentation) {
  printf("Callback Init: device_num=%d type=%s device=%p lookup=%p doc=%p\n",
         device_num, type, device, lookup, documentation);
}

static void on_ompt_callback_device_finalize(int device_num) {
  printf("Callback Fini: device_num=%d\n", device_num);
}

static void on_ompt_callback_device_load(int device_num, const char *filename,
                                         int64_t offset_in_file,
                                         void *vma_in_file, size_t bytes,
                                         void *host_addr, void *device_addr,
                                         uint64_t module_id) {
  printf("Callback Load: device_num:%d module_id:%lu filename:%s host_adddr:%p "
         "device_addr:%p bytes:%lu\n",
         device_num, module_id, filename, host_addr, device_addr, bytes);
}
static void on_ompt_callback_device_unload(int device_num, uint64_t module_id) {
  printf("Callback Unload: device_num:%d module_id:%lu\n",
         device_num, module_id);
}


static void on_ompt_callback_target_data_op(
    ompt_id_t target_id, ompt_id_t host_op_id, ompt_target_data_op_t optype,
    void *src_addr, int src_device_num, void *dest_addr, int dest_device_num,
    size_t bytes, const void *codeptr_ra) {
  assert(codeptr_ra != 0 && "Unexpected null codeptr");
  printf("  Callback DataOp: target_id=%lu host_op_id=%lu optype=%s%s src=%p "
         "src_device_num=%d "
         "dest=%p dest_device_num=%d bytes=%lu code=%p\n",
         target_id, host_op_id, ompt_target_data_op_t_values[optype & ~ompt_target_data_implementation_specific], 
         (optype & ompt_target_data_implementation_specific ? "|ompt_target_data_implementation_specific" : ""), 
         src_addr, src_device_num, dest_addr, dest_device_num, bytes, codeptr_ra);
}

static void on_ompt_callback_target(ompt_target_t kind,
                                    ompt_scope_endpoint_t endpoint,
                                    int device_num, ompt_data_t *task_data,
                                    ompt_id_t target_id,
                                    const void *codeptr_ra) {
  assert(codeptr_ra != 0 && "Unexpected null codeptr");
  printf("Callback Target: target_id=%lu kind=%s endpoint=%s device_num=%d "
         "code=%p\n",
         target_id, ompt_target_t_values[kind],
         ompt_scope_endpoint_t_values[endpoint], device_num, codeptr_ra);
}

static void on_ompt_callback_target_submit(ompt_id_t target_id,
                                           ompt_id_t host_op_id,
                                           unsigned int requested_num_teams) {
  printf("  Callback Submit: target_id=%lu host_op_id=%lu req_num_teams=%d\n",
         target_id, host_op_id, requested_num_teams);
}

static void on_ompt_callback_target_map(ompt_id_t target_id,
                                        unsigned int nitems, void **host_addr,
                                        void **device_addr, size_t *bytes,
                                        unsigned int *mapping_flags,
                                        const void *codeptr_ra) {
  printf("Target map callback is unimplemented\n");
  abort();
}

static void on_ompt_callback_target_data_op_emi(
    ompt_scope_endpoint_t endpoint, ompt_data_t *target_task_data,
    ompt_data_t *target_data, ompt_id_t *host_op_id,
    int optype, void *src_addr, int src_device_num,
    void *dest_addr, int dest_device_num, size_t bytes,
    const void *codeptr_ra) {
  assert(codeptr_ra != 0 && "Unexpected null codeptr");
  if (endpoint == ompt_scope_begin)
    *host_op_id = next_op_id++;
  // target_task_data may be null, avoid dereferencing it
  uint64_t target_task_data_value =
      (target_task_data) ? target_task_data->value : 0;
  printf("  Callback DataOp EMI: endpoint=%s optype=%s%s target_task_data=%p "
         "(0x%lx) target_data=%p (0x%lx) host_op_id=%p (0x%lx) src=%p "
         "src_device_num=%d "
         "dest=%p dest_device_num=%d bytes=%lu code=%p\n",
         ompt_scope_endpoint_t_values[endpoint],
         ompt_target_data_op_t_values[optype & ~ompt_target_data_implementation_specific],
         (optype & ompt_target_data_implementation_specific ? "|ompt_target_data_implementation_specific" : ""),
         target_task_data, target_task_data_value, target_data, target_data->value, host_op_id,
         *host_op_id, src_addr, src_device_num, dest_addr, dest_device_num,
         bytes, codeptr_ra);
}

static void on_ompt_callback_target_emi(ompt_target_t kind,
                                        ompt_scope_endpoint_t endpoint,
                                        int device_num, ompt_data_t *task_data,
                                        ompt_data_t *target_task_data,
                                        ompt_data_t *target_data,
                                        const void *codeptr_ra) {
  assert(codeptr_ra != 0 && "Unexpected null codeptr");
  if (endpoint == ompt_scope_begin)
    target_data->value = next_op_id++;
  printf("Callback Target EMI: kind=%s endpoint=%s device_num=%d task_data=%p "
         "(0x%lx) target_task_data=%p (0x%lx) target_data=%p (0x%lx) code=%p\n",
         ompt_target_t_values[kind], ompt_scope_endpoint_t_values[endpoint],
         device_num, task_data, task_data ? task_data->value : 0,
         target_task_data, target_task_data ? target_task_data->value : 0,
         target_data, target_data->value, codeptr_ra);
}

static void on_ompt_callback_target_submit_emi(
    ompt_scope_endpoint_t endpoint, ompt_data_t *target_data,
    ompt_id_t *host_op_id, unsigned int requested_num_teams) {
  printf("  Callback Submit EMI: endpoint=%s  req_num_teams=%d target_data=%p "
         "(0x%lx) host_op_id=%p (0x%lx)\n",
         ompt_scope_endpoint_t_values[endpoint], requested_num_teams,
         target_data, target_data->value, host_op_id, *host_op_id);
}

static void on_ompt_callback_target_map_emi(ompt_data_t *target_data,
                                            unsigned int nitems,
                                            void **host_addr,
                                            void **device_addr, size_t *bytes,
                                            unsigned int *mapping_flags,
                                            const void *codeptr_ra) {
  printf("Target map emi callback is unimplemented\n");
  abort();
}

static void
on_ompt_callback_task_create(ompt_data_t *encountering_task_data,
                             const ompt_frame_t *encountering_task_frame,
                             ompt_data_t *new_task_data, int type,
                             int has_dependences, const void *codeptr_ra) {
  if (new_task_data->ptr)
    printf("0: new_task_data initially not null\n");
  new_task_data->value = next_op_id++;
  char buffer[2048];

  format_task_type(type, buffer);

  printf(
      "%" PRIu64 ":" _TOOL_PREFIX
      " ompt_event_task_create: parent_task_id=%" PRIx64
      ", parent_task_frame.exit=%p, parent_task_frame.reenter=%p, "
      "new_task_id=%" PRIx64
      ", codeptr_ra=%p, task_type=%s=%d, has_dependences=%s\n",
      0,
      encountering_task_data ? encountering_task_data->value : 0,
      encountering_task_frame ? encountering_task_frame->exit_frame.ptr : NULL,
      encountering_task_frame ? encountering_task_frame->enter_frame.ptr : NULL,
      new_task_data->value, codeptr_ra, buffer, type,
      has_dependences ? "yes" : "no");
}

static void on_ompt_callback_task_schedule(ompt_data_t *first_task_data,
                                           ompt_task_status_t prior_task_status,
                                           ompt_data_t *second_task_data) {
  printf("%" PRIu64 ":" _TOOL_PREFIX
         " ompt_event_task_schedule: first_task_id=%" PRIx64
         ", second_task_id=%" PRIx64 ", prior_task_status=%s=%d\n",
         0, first_task_data->value,
         (second_task_data ? second_task_data->value : -1),
         ompt_task_status_t_values[prior_task_status], prior_task_status);
  if (prior_task_status == ompt_task_complete ||
      prior_task_status == ompt_task_late_fulfill ||
      prior_task_status == ompt_taskwait_complete) {
    printf("%" PRIu64 ":" _TOOL_PREFIX " ompt_event_task_end: task_id=%" PRIx64
           "\n",
           0, first_task_data->value);
  }
}

static void on_ompt_callback_dependences(ompt_data_t *task_data,
                                         const ompt_dependence_t *deps,
                                         int ndeps) {
  char buffer[2048];
  char *progress = buffer;
  int i;
  for (i = 0; i < ndeps && progress < buffer + 2000; i++) {
    if (deps[i].dependence_type == ompt_dependence_type_source ||
        deps[i].dependence_type == ompt_dependence_type_sink)
      progress +=
          sprintf(progress, "(%" PRIu64 ", %s), ", deps[i].variable.value,
                  ompt_dependence_type_t_values[deps[i].dependence_type]);
    else
      progress +=
          sprintf(progress, "(%p, %s), ", deps[i].variable.ptr,
                  ompt_dependence_type_t_values[deps[i].dependence_type]);
  }
  if (ndeps > 0)
    progress[-2] = 0;
  printf("%" PRIu64 ":" _TOOL_PREFIX " ompt_event_dependences: task_id=%" PRIx64
         ", deps=[%s], ndeps=%d\n",
         0, task_data->value, buffer, ndeps);
}

static void on_ompt_callback_task_dependence(ompt_data_t *first_task_data,
                                             ompt_data_t *second_task_data) {
  printf("%" PRIu64 ":" _TOOL_PREFIX
         " ompt_event_task_dependence_pair: first_task_id=%" PRIx64
         ", second_task_id=%" PRIx64 "\n",
         0, first_task_data->value,
         second_task_data->value);
}

static void on_ompt_callback_implicit_task(ompt_scope_endpoint_t endpoint,
                                           ompt_data_t *parallel_data,
                                           ompt_data_t *task_data,
                                           unsigned int team_size,
                                           unsigned int thread_num, int flags) {
  switch (endpoint) {
  case ompt_scope_begin:
    if (task_data->ptr)
      printf("%s\n", "0: task_data initially not null");
    task_data->value = next_op_id++;

    // there is no parallel_begin callback for implicit parallel region
    // thus it is initialized in initial task
    if (flags & ompt_task_initial) {
      char buffer[2048];

      format_task_type(flags, buffer);
      // Only check initial task not created by teams construct
      if (team_size == 1 && thread_num == 1 && parallel_data->ptr)
        printf("%s\n", "0: parallel_data initially not null");
      parallel_data->value = next_op_id++;
      printf("%" PRIu64 ":" _TOOL_PREFIX
             " ompt_event_initial_task_begin: parallel_id=%" PRIx64
             ", task_id=%" PRIx64 ", actual_parallelism=%" PRIu32
             ", index=%" PRIu32 ", flags=%" PRIu32 "\n",
             0, parallel_data->value,
             task_data->value, team_size, thread_num, flags);
    } else {
      printf("%" PRIu64 ":" _TOOL_PREFIX
             " ompt_event_implicit_task_begin: parallel_id=%" PRIx64
             ", task_id=%" PRIx64 ", team_size=%" PRIu32 ", thread_num=%" PRIu32
             "\n",
             0, parallel_data->value,
             task_data->value, team_size, thread_num);
    }

    break;
  case ompt_scope_end:
    if (flags & ompt_task_initial) {
      printf("%" PRIu64 ":" _TOOL_PREFIX
             " ompt_event_initial_task_end: parallel_id=%" PRIx64
             ", task_id=%" PRIx64 ", actual_parallelism=%" PRIu32
             ", index=%" PRIu32 "\n",
             0,
             (parallel_data) ? parallel_data->value : 0, task_data->value,
             team_size, thread_num);
    } else {
      printf("%" PRIu64 ":" _TOOL_PREFIX
             " ompt_event_implicit_task_end: parallel_id=%" PRIx64
             ", task_id=%" PRIx64 ", team_size=%" PRIu32 ", thread_num=%" PRIu32
             "\n",
             0,
             (parallel_data) ? parallel_data->value : 0, task_data->value,
             team_size, thread_num);
    }
    break;
  case ompt_scope_beginend:
    printf("ompt_scope_beginend should never be passed to %s\n", __func__);
    exit(-1);
  }
}

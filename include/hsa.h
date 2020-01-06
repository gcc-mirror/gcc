/* HSA runtime API 1.0.1 representation description.
   Copyright (C) 2016-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.

The contents of the file was created by extracting data structures, enum,
typedef and other definitions from HSA Runtime Programmer’s Reference Manual
Version 1.0 (http://www.hsafoundation.com/standards/).

HTML version is provided on the following link:
http://www.hsafoundation.com/html/Content/Runtime/Topics/Runtime_title_page.htm
*/

#ifndef _HSA_H
#define _HSA_H 1

#define HSA_LARGE_MODEL 1

typedef struct hsa_signal_s { uint64_t handle; } hsa_signal_t;
typedef enum {
  HSA_QUEUE_TYPE_MULTI = 0,
  HSA_QUEUE_TYPE_SINGLE = 1
} hsa_queue_type_t;

typedef enum { HSA_PROFILE_BASE = 0, HSA_PROFILE_FULL = 1 } hsa_profile_t;
typedef struct hsa_region_s { uint64_t handle; } hsa_region_t;
typedef enum {
  HSA_EXECUTABLE_SYMBOL_INFO_TYPE = 0,
  HSA_EXECUTABLE_SYMBOL_INFO_NAME_LENGTH = 1,
  HSA_EXECUTABLE_SYMBOL_INFO_NAME = 2,
  HSA_EXECUTABLE_SYMBOL_INFO_MODULE_NAME_LENGTH = 3,
  HSA_EXECUTABLE_SYMBOL_INFO_MODULE_NAME = 4,
  HSA_EXECUTABLE_SYMBOL_INFO_AGENT = 20,
  HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_ADDRESS = 21,
  HSA_EXECUTABLE_SYMBOL_INFO_LINKAGE = 5,
  HSA_EXECUTABLE_SYMBOL_INFO_IS_DEFINITION = 17,
  HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_ALLOCATION = 6,
  HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_SEGMENT = 7,
  HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_ALIGNMENT = 8,
  HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_SIZE = 9,
  HSA_EXECUTABLE_SYMBOL_INFO_VARIABLE_IS_CONST = 10,
  HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_OBJECT = 22,
  HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_KERNARG_SEGMENT_SIZE = 11,
  HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_KERNARG_SEGMENT_ALIGNMENT = 12,
  HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_GROUP_SEGMENT_SIZE = 13,
  HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_PRIVATE_SEGMENT_SIZE = 14,
  HSA_EXECUTABLE_SYMBOL_INFO_KERNEL_DYNAMIC_CALLSTACK = 15,
  HSA_EXECUTABLE_SYMBOL_INFO_INDIRECT_FUNCTION_OBJECT = 23,
  HSA_EXECUTABLE_SYMBOL_INFO_INDIRECT_FUNCTION_CALL_CONVENTION = 16
} hsa_executable_symbol_info_t;
typedef enum {
  HSA_REGION_GLOBAL_FLAG_KERNARG = 1,
  HSA_REGION_GLOBAL_FLAG_FINE_GRAINED = 2,
  HSA_REGION_GLOBAL_FLAG_COARSE_GRAINED = 4
} hsa_region_global_flag_t;
typedef struct hsa_code_object_s { uint64_t handle; } hsa_code_object_t;
typedef enum {
  HSA_KERNEL_DISPATCH_PACKET_SETUP_WIDTH_DIMENSIONS = 2
} hsa_kernel_dispatch_packet_setup_width_t;
typedef enum {
  HSA_DEVICE_TYPE_CPU = 0,
  HSA_DEVICE_TYPE_GPU = 1,
  HSA_DEVICE_TYPE_DSP = 2
} hsa_device_type_t;
typedef enum {
  HSA_STATUS_SUCCESS = 0x0,
  HSA_STATUS_INFO_BREAK = 0x1,
  HSA_STATUS_ERROR = 0x1000,
  HSA_STATUS_ERROR_INVALID_ARGUMENT = 0x1001,
  HSA_STATUS_ERROR_INVALID_QUEUE_CREATION = 0x1002,
  HSA_STATUS_ERROR_INVALID_ALLOCATION = 0x1003,
  HSA_STATUS_ERROR_INVALID_AGENT = 0x1004,
  HSA_STATUS_ERROR_INVALID_REGION = 0x1005,
  HSA_STATUS_ERROR_INVALID_SIGNAL = 0x1006,
  HSA_STATUS_ERROR_INVALID_QUEUE = 0x1007,
  HSA_STATUS_ERROR_OUT_OF_RESOURCES = 0x1008,
  HSA_STATUS_ERROR_INVALID_PACKET_FORMAT = 0x1009,
  HSA_STATUS_ERROR_RESOURCE_FREE = 0x100A,
  HSA_STATUS_ERROR_NOT_INITIALIZED = 0x100B,
  HSA_STATUS_ERROR_REFCOUNT_OVERFLOW = 0x100C,
  HSA_STATUS_ERROR_INCOMPATIBLE_ARGUMENTS = 0x100D,
  HSA_STATUS_ERROR_INVALID_INDEX = 0x100E,
  HSA_STATUS_ERROR_INVALID_ISA = 0x100F,
  HSA_STATUS_ERROR_INVALID_ISA_NAME = 0x1017,
  HSA_STATUS_ERROR_INVALID_CODE_OBJECT = 0x1010,
  HSA_STATUS_ERROR_INVALID_EXECUTABLE = 0x1011,
  HSA_STATUS_ERROR_FROZEN_EXECUTABLE = 0x1012,
  HSA_STATUS_ERROR_INVALID_SYMBOL_NAME = 0x1013,
  HSA_STATUS_ERROR_VARIABLE_ALREADY_DEFINED = 0x1014,
  HSA_STATUS_ERROR_VARIABLE_UNDEFINED = 0x1015,
  HSA_STATUS_ERROR_EXCEPTION = 0x1016
} hsa_status_t;
typedef enum {
  HSA_EXTENSION_FINALIZER = 0,
  HSA_EXTENSION_IMAGES = 1
} hsa_extension_t;
typedef struct hsa_queue_s {
  hsa_queue_type_t type;
  uint32_t features;

#ifdef HSA_LARGE_MODEL
  void *base_address;
#elif defined HSA_LITTLE_ENDIAN
  void *base_address;
  uint32_t reserved0;
#else
  uint32_t reserved0;
  void *base_address;
#endif

  hsa_signal_t doorbell_signal;
  uint32_t size;
  uint32_t reserved1;
  uint64_t id;
} hsa_queue_t;
typedef struct hsa_agent_dispatch_packet_s {
  uint16_t header;
  uint16_t type;
  uint32_t reserved0;

#ifdef HSA_LARGE_MODEL
  void *return_address;
#elif defined HSA_LITTLE_ENDIAN
  void *return_address;
  uint32_t reserved1;
#else
  uint32_t reserved1;
  void *return_address;
#endif
  uint64_t arg[4];
  uint64_t reserved2;
  hsa_signal_t completion_signal;
} hsa_agent_dispatch_packet_t;
typedef enum {
  HSA_CODE_SYMBOL_INFO_TYPE = 0,
  HSA_CODE_SYMBOL_INFO_NAME_LENGTH = 1,
  HSA_CODE_SYMBOL_INFO_NAME = 2,
  HSA_CODE_SYMBOL_INFO_MODULE_NAME_LENGTH = 3,
  HSA_CODE_SYMBOL_INFO_MODULE_NAME = 4,
  HSA_CODE_SYMBOL_INFO_LINKAGE = 5,
  HSA_CODE_SYMBOL_INFO_IS_DEFINITION = 17,
  HSA_CODE_SYMBOL_INFO_VARIABLE_ALLOCATION = 6,
  HSA_CODE_SYMBOL_INFO_VARIABLE_SEGMENT = 7,
  HSA_CODE_SYMBOL_INFO_VARIABLE_ALIGNMENT = 8,
  HSA_CODE_SYMBOL_INFO_VARIABLE_SIZE = 9,
  HSA_CODE_SYMBOL_INFO_VARIABLE_IS_CONST = 10,
  HSA_CODE_SYMBOL_INFO_KERNEL_KERNARG_SEGMENT_SIZE = 11,
  HSA_CODE_SYMBOL_INFO_KERNEL_KERNARG_SEGMENT_ALIGNMENT = 12,
  HSA_CODE_SYMBOL_INFO_KERNEL_GROUP_SEGMENT_SIZE = 13,
  HSA_CODE_SYMBOL_INFO_KERNEL_PRIVATE_SEGMENT_SIZE = 14,
  HSA_CODE_SYMBOL_INFO_KERNEL_DYNAMIC_CALLSTACK = 15,
  HSA_CODE_SYMBOL_INFO_INDIRECT_FUNCTION_CALL_CONVENTION = 16
} hsa_code_symbol_info_t;
typedef enum {
  HSA_QUEUE_FEATURE_KERNEL_DISPATCH = 1,
  HSA_QUEUE_FEATURE_AGENT_DISPATCH = 2
} hsa_queue_feature_t;
typedef enum {
  HSA_VARIABLE_ALLOCATION_AGENT = 0,
  HSA_VARIABLE_ALLOCATION_PROGRAM = 1
} hsa_variable_allocation_t;
typedef enum {
  HSA_FENCE_SCOPE_NONE = 0,
  HSA_FENCE_SCOPE_AGENT = 1,
  HSA_FENCE_SCOPE_SYSTEM = 2
} hsa_fence_scope_t;
typedef struct hsa_agent_s { uint64_t handle; } hsa_agent_t;
typedef enum { HSA_CODE_OBJECT_TYPE_PROGRAM = 0 } hsa_code_object_type_t;
typedef enum {
  HSA_SIGNAL_CONDITION_EQ = 0,
  HSA_SIGNAL_CONDITION_NE = 1,
  HSA_SIGNAL_CONDITION_LT = 2,
  HSA_SIGNAL_CONDITION_GTE = 3
} hsa_signal_condition_t;
typedef enum {
  HSA_EXECUTABLE_STATE_UNFROZEN = 0,
  HSA_EXECUTABLE_STATE_FROZEN = 1
} hsa_executable_state_t;
typedef enum {
  HSA_ENDIANNESS_LITTLE = 0,
  HSA_ENDIANNESS_BIG = 1
} hsa_endianness_t;
typedef enum {
  HSA_MACHINE_MODEL_SMALL = 0,
  HSA_MACHINE_MODEL_LARGE = 1
} hsa_machine_model_t;
typedef enum {
  HSA_AGENT_INFO_NAME = 0,
  HSA_AGENT_INFO_VENDOR_NAME = 1,
  HSA_AGENT_INFO_FEATURE = 2,
  HSA_AGENT_INFO_MACHINE_MODEL = 3,
  HSA_AGENT_INFO_PROFILE = 4,
  HSA_AGENT_INFO_DEFAULT_FLOAT_ROUNDING_MODE = 5,
  HSA_AGENT_INFO_BASE_PROFILE_DEFAULT_FLOAT_ROUNDING_MODES = 23,
  HSA_AGENT_INFO_FAST_F16_OPERATION = 24,
  HSA_AGENT_INFO_WAVEFRONT_SIZE = 6,
  HSA_AGENT_INFO_WORKGROUP_MAX_DIM = 7,
  HSA_AGENT_INFO_WORKGROUP_MAX_SIZE = 8,
  HSA_AGENT_INFO_GRID_MAX_DIM = 9,
  HSA_AGENT_INFO_GRID_MAX_SIZE = 10,
  HSA_AGENT_INFO_FBARRIER_MAX_SIZE = 11,
  HSA_AGENT_INFO_QUEUES_MAX = 12,
  HSA_AGENT_INFO_QUEUE_MIN_SIZE = 13,
  HSA_AGENT_INFO_QUEUE_MAX_SIZE = 14,
  HSA_AGENT_INFO_QUEUE_TYPE = 15,
  HSA_AGENT_INFO_NODE = 16,
  HSA_AGENT_INFO_DEVICE = 17,
  HSA_AGENT_INFO_CACHE_SIZE = 18,
  HSA_AGENT_INFO_ISA = 19,
  HSA_AGENT_INFO_EXTENSIONS = 20,
  HSA_AGENT_INFO_VERSION_MAJOR = 21,
  HSA_AGENT_INFO_VERSION_MINOR = 22
} hsa_agent_info_t;
typedef struct hsa_barrier_and_packet_s {
  uint16_t header;
  uint16_t reserved0;
  uint32_t reserved1;
  hsa_signal_t dep_signal[5];
  uint64_t reserved2;
  hsa_signal_t completion_signal;
} hsa_barrier_and_packet_t;
typedef struct hsa_dim3_s {
  uint32_t x;
  uint32_t y;
  uint32_t z;
} hsa_dim3_t;
typedef enum {
  HSA_ACCESS_PERMISSION_RO = 1,
  HSA_ACCESS_PERMISSION_WO = 2,
  HSA_ACCESS_PERMISSION_RW = 3
} hsa_access_permission_t;
typedef enum {
  HSA_AGENT_FEATURE_KERNEL_DISPATCH = 1,
  HSA_AGENT_FEATURE_AGENT_DISPATCH = 2
} hsa_agent_feature_t;
typedef enum {
  HSA_WAIT_STATE_BLOCKED = 0,
  HSA_WAIT_STATE_ACTIVE = 1
} hsa_wait_state_t;
typedef struct hsa_executable_s { uint64_t handle; } hsa_executable_t;
typedef enum {
  HSA_REGION_SEGMENT_GLOBAL = 0,
  HSA_REGION_SEGMENT_READONLY = 1,
  HSA_REGION_SEGMENT_PRIVATE = 2,
  HSA_REGION_SEGMENT_GROUP = 3
} hsa_region_segment_t;
typedef enum {
  HSA_REGION_INFO_SEGMENT = 0,
  HSA_REGION_INFO_GLOBAL_FLAGS = 1,
  HSA_REGION_INFO_SIZE = 2,
  HSA_REGION_INFO_ALLOC_MAX_SIZE = 4,
  HSA_REGION_INFO_RUNTIME_ALLOC_ALLOWED = 5,
  HSA_REGION_INFO_RUNTIME_ALLOC_GRANULE = 6,
  HSA_REGION_INFO_RUNTIME_ALLOC_ALIGNMENT = 7
} hsa_region_info_t;
typedef enum {
  HSA_ISA_INFO_NAME_LENGTH = 0,
  HSA_ISA_INFO_NAME = 1,
  HSA_ISA_INFO_CALL_CONVENTION_COUNT = 2,
  HSA_ISA_INFO_CALL_CONVENTION_INFO_WAVEFRONT_SIZE = 3,
  HSA_ISA_INFO_CALL_CONVENTION_INFO_WAVEFRONTS_PER_COMPUTE_UNIT = 4
} hsa_isa_info_t;
typedef enum {
  HSA_VARIABLE_SEGMENT_GLOBAL = 0,
  HSA_VARIABLE_SEGMENT_READONLY = 1
} hsa_variable_segment_t;
typedef struct hsa_callback_data_s { uint64_t handle; } hsa_callback_data_t;
typedef enum {
  HSA_SYMBOL_KIND_VARIABLE = 0,
  HSA_SYMBOL_KIND_KERNEL = 1,
  HSA_SYMBOL_KIND_INDIRECT_FUNCTION = 2
} hsa_symbol_kind_t;
typedef struct hsa_kernel_dispatch_packet_s {
  uint16_t header;
  uint16_t setup;
  uint16_t workgroup_size_x;
  uint16_t workgroup_size_y;
  uint16_t workgroup_size_z;
  uint16_t reserved0;
  uint32_t grid_size_x;
  uint32_t grid_size_y;
  uint32_t grid_size_z;
  uint32_t private_segment_size;
  uint32_t group_segment_size;
  uint64_t kernel_object;

#ifdef HSA_LARGE_MODEL
  void *kernarg_address;
#elif defined HSA_LITTLE_ENDIAN
  void *kernarg_address;
  uint32_t reserved1;
#else
  uint32_t reserved1;
  void *kernarg_address;
#endif
  uint64_t reserved2;
  hsa_signal_t completion_signal;
} hsa_kernel_dispatch_packet_t;
typedef enum {
  HSA_PACKET_TYPE_VENDOR_SPECIFIC = 0,
  HSA_PACKET_TYPE_INVALID = 1,
  HSA_PACKET_TYPE_KERNEL_DISPATCH = 2,
  HSA_PACKET_TYPE_BARRIER_AND = 3,
  HSA_PACKET_TYPE_AGENT_DISPATCH = 4,
  HSA_PACKET_TYPE_BARRIER_OR = 5
} hsa_packet_type_t;
typedef enum {
  HSA_PACKET_HEADER_TYPE = 0,
  HSA_PACKET_HEADER_BARRIER = 8,
  HSA_PACKET_HEADER_ACQUIRE_FENCE_SCOPE = 9,
  HSA_PACKET_HEADER_RELEASE_FENCE_SCOPE = 11
} hsa_packet_header_t;
typedef struct hsa_isa_s { uint64_t handle; } hsa_isa_t;
typedef enum {
  HSA_DEFAULT_FLOAT_ROUNDING_MODE_DEFAULT = 0,
  HSA_DEFAULT_FLOAT_ROUNDING_MODE_ZERO = 1,
  HSA_DEFAULT_FLOAT_ROUNDING_MODE_NEAR = 2
} hsa_default_float_rounding_mode_t;
typedef struct hsa_code_symbol_s { uint64_t handle; } hsa_code_symbol_t;
typedef struct hsa_executable_symbol_s {
  uint64_t handle;
} hsa_executable_symbol_t;
#ifdef HSA_LARGE_MODEL
typedef int64_t hsa_signal_value_t;
#else
typedef int32_t hsa_signal_value_t;
#endif
typedef enum {
  HSA_EXCEPTION_POLICY_BREAK = 1,
  HSA_EXCEPTION_POLICY_DETECT = 2
} hsa_exception_policy_t;
typedef enum {
  HSA_SYSTEM_INFO_VERSION_MAJOR = 0,
  HSA_SYSTEM_INFO_VERSION_MINOR = 1,
  HSA_SYSTEM_INFO_TIMESTAMP = 2,
  HSA_SYSTEM_INFO_TIMESTAMP_FREQUENCY = 3,
  HSA_SYSTEM_INFO_SIGNAL_MAX_WAIT = 4,
  HSA_SYSTEM_INFO_ENDIANNESS = 5,
  HSA_SYSTEM_INFO_MACHINE_MODEL = 6,
  HSA_SYSTEM_INFO_EXTENSIONS = 7
} hsa_system_info_t;
typedef enum {
  HSA_EXECUTABLE_INFO_PROFILE = 1,
  HSA_EXECUTABLE_INFO_STATE = 2
} hsa_executable_info_t;
typedef enum {
  HSA_KERNEL_DISPATCH_PACKET_SETUP_DIMENSIONS = 0
} hsa_kernel_dispatch_packet_setup_t;
typedef enum {
  HSA_PACKET_HEADER_WIDTH_TYPE = 8,
  HSA_PACKET_HEADER_WIDTH_BARRIER = 1,
  HSA_PACKET_HEADER_WIDTH_ACQUIRE_FENCE_SCOPE = 2,
  HSA_PACKET_HEADER_WIDTH_RELEASE_FENCE_SCOPE = 2
} hsa_packet_header_width_t;
typedef enum {
  HSA_CODE_OBJECT_INFO_VERSION = 0,
  HSA_CODE_OBJECT_INFO_TYPE = 1,
  HSA_CODE_OBJECT_INFO_ISA = 2,
  HSA_CODE_OBJECT_INFO_MACHINE_MODEL = 3,
  HSA_CODE_OBJECT_INFO_PROFILE = 4,
  HSA_CODE_OBJECT_INFO_DEFAULT_FLOAT_ROUNDING_MODE = 5
} hsa_code_object_info_t;
typedef struct hsa_barrier_or_packet_s {
  uint16_t header;
  uint16_t reserved0;
  uint32_t reserved1;
  hsa_signal_t dep_signal[5];
  uint64_t reserved2;
  hsa_signal_t completion_signal;
} hsa_barrier_or_packet_t;
typedef enum {
  HSA_SYMBOL_KIND_LINKAGE_MODULE = 0,
  HSA_SYMBOL_KIND_LINKAGE_PROGRAM = 1,
} hsa_symbol_kind_linkage_t;
hsa_status_t hsa_executable_validate(hsa_executable_t executable,
                                     uint32_t *result);
uint64_t hsa_queue_add_write_index_acq_rel(const hsa_queue_t *queue,
                                           uint64_t value);

uint64_t hsa_queue_add_write_index_acquire(const hsa_queue_t *queue,
                                           uint64_t value);

uint64_t hsa_queue_add_write_index_relaxed(const hsa_queue_t *queue,
                                           uint64_t value);

uint64_t hsa_queue_add_write_index_release(const hsa_queue_t *queue,
                                           uint64_t value);
hsa_status_t hsa_shut_down();
void hsa_signal_add_acq_rel(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_add_acquire(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_add_relaxed(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_add_release(hsa_signal_t signal, hsa_signal_value_t value);
hsa_status_t hsa_executable_readonly_variable_define(
    hsa_executable_t executable, hsa_agent_t agent, const char *variable_name,
    void *address);
hsa_status_t hsa_agent_extension_supported(uint16_t extension,
                                           hsa_agent_t agent,
                                           uint16_t version_major,
                                           uint16_t version_minor,
                                           bool *result);
hsa_signal_value_t hsa_signal_load_acquire(hsa_signal_t signal);

hsa_signal_value_t hsa_signal_load_relaxed(hsa_signal_t signal);
hsa_status_t hsa_executable_get_info(hsa_executable_t executable,
                                     hsa_executable_info_t attribute,
                                     void *value);
hsa_status_t hsa_iterate_agents(hsa_status_t (*callback)(hsa_agent_t agent,
                                                         void *data),
                                void *data);
void hsa_signal_subtract_acq_rel(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_subtract_acquire(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_subtract_relaxed(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_subtract_release(hsa_signal_t signal, hsa_signal_value_t value);
hsa_status_t
hsa_executable_symbol_get_info(hsa_executable_symbol_t executable_symbol,
                               hsa_executable_symbol_info_t attribute,
                               void *value);
void hsa_signal_xor_acq_rel(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_xor_acquire(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_xor_relaxed(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_xor_release(hsa_signal_t signal, hsa_signal_value_t value);
hsa_status_t hsa_code_object_get_info(hsa_code_object_t code_object,
                                      hsa_code_object_info_t attribute,
                                      void *value);
hsa_status_t hsa_code_object_deserialize(void *serialized_code_object,
                                         size_t serialized_code_object_size,
                                         const char *options,
                                         hsa_code_object_t *code_object);
hsa_status_t hsa_status_string(hsa_status_t status, const char **status_string);
hsa_status_t hsa_code_object_get_symbol(hsa_code_object_t code_object,
                                        const char *symbol_name,
                                        hsa_code_symbol_t *symbol);
void hsa_signal_store_relaxed(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_store_release(hsa_signal_t signal, hsa_signal_value_t value);
hsa_status_t hsa_signal_destroy(hsa_signal_t signal);
hsa_status_t hsa_system_get_extension_table(uint16_t extension,
                                            uint16_t version_major,
                                            uint16_t version_minor,
                                            void *table);
hsa_status_t hsa_agent_iterate_regions(
    hsa_agent_t agent,
    hsa_status_t (*callback)(hsa_region_t region, void *data), void *data);
hsa_status_t hsa_executable_agent_global_variable_define(
    hsa_executable_t executable, hsa_agent_t agent, const char *variable_name,
    void *address);
hsa_status_t hsa_queue_create(hsa_agent_t agent, uint32_t size,
                              hsa_queue_type_t type,
                              void (*callback)(hsa_status_t status,
                                               hsa_queue_t *source, void *data),
                              void *data, uint32_t private_segment_size,
                              uint32_t group_segment_size, hsa_queue_t **queue);
hsa_status_t hsa_isa_compatible(hsa_isa_t code_object_isa, hsa_isa_t agent_isa,
                                bool *result);
hsa_status_t hsa_code_object_serialize(
    hsa_code_object_t code_object,
    hsa_status_t (*alloc_callback)(size_t size, hsa_callback_data_t data,
                                   void **address),
    hsa_callback_data_t callback_data, const char *options,
    void **serialized_code_object, size_t *serialized_code_object_size);
hsa_status_t hsa_region_get_info(hsa_region_t region,
                                 hsa_region_info_t attribute, void *value);
hsa_status_t hsa_executable_freeze(hsa_extension_t executable,
                                   const char *options);
hsa_status_t hsa_system_extension_supported(uint16_t extension,
                                            uint16_t version_major,
                                            uint16_t version_minor,
                                            bool *result);
hsa_signal_value_t hsa_signal_wait_acquire(hsa_signal_t signal,
                                           hsa_signal_condition_t condition,
                                           hsa_signal_value_t compare_value,
                                           uint64_t timeout_hint,
                                           hsa_wait_state_t wait_state_hint);

hsa_signal_value_t hsa_signal_wait_relaxed(hsa_signal_t signal,
                                           hsa_signal_condition_t condition,
                                           hsa_signal_value_t compare_value,
                                           uint64_t timeout_hint,
                                           hsa_wait_state_t wait_state_hint);
hsa_status_t hsa_memory_copy(void *dst, const void *src, size_t size);
hsa_status_t hsa_memory_free(void *ptr);
hsa_status_t hsa_queue_destroy(hsa_queue_t *queue);
hsa_status_t hsa_isa_from_name(const char *name, hsa_isa_t *isa);
hsa_status_t hsa_isa_get_info(hsa_isa_t isa, hsa_isa_info_t attribute,
                              uint32_t index, void *value);
hsa_status_t hsa_signal_create(hsa_signal_value_t initial_value,
                               uint32_t num_consumers,
                               const hsa_agent_t *consumers,
                               hsa_signal_t *signal);
hsa_status_t hsa_code_symbol_get_info(hsa_code_symbol_t code_symbol,
                                      hsa_code_symbol_info_t attribute,
                                      void *value);
hsa_signal_value_t hsa_signal_cas_acq_rel(hsa_signal_t signal,
                                          hsa_signal_value_t expected,
                                          hsa_signal_value_t value);

hsa_signal_value_t hsa_signal_cas_acquire(hsa_signal_t signal,
                                          hsa_signal_value_t expected,
                                          hsa_signal_value_t value);

hsa_signal_value_t hsa_signal_cas_relaxed(hsa_signal_t signal,
                                          hsa_signal_value_t expected,
                                          hsa_signal_value_t value);

hsa_signal_value_t hsa_signal_cas_release(hsa_signal_t signal,
                                          hsa_signal_value_t expected,
                                          hsa_signal_value_t value);
hsa_status_t hsa_code_object_iterate_symbols(
    hsa_code_object_t code_object,
    hsa_status_t (*callback)(hsa_code_object_t code_object,
                             hsa_code_symbol_t symbol, void *data),
    void *data);
void hsa_queue_store_read_index_relaxed(const hsa_queue_t *queue,
                                        uint64_t value);

void hsa_queue_store_read_index_release(const hsa_queue_t *queue,
                                        uint64_t value);
hsa_status_t hsa_memory_assign_agent(void *ptr, hsa_agent_t agent,
                                     hsa_access_permission_t access);
hsa_status_t hsa_queue_inactivate(hsa_queue_t *queue);
hsa_status_t hsa_executable_get_symbol(hsa_executable_t executable,
                                       const char *module_name,
                                       const char *symbol_name,
                                       hsa_agent_t agent,
                                       int32_t call_convention,
                                       hsa_executable_symbol_t *symbol);
uint64_t hsa_queue_cas_write_index_acq_rel(const hsa_queue_t *queue,
                                           uint64_t expected, uint64_t value);

uint64_t hsa_queue_cas_write_index_acquire(const hsa_queue_t *queue,
                                           uint64_t expected, uint64_t value);

uint64_t hsa_queue_cas_write_index_relaxed(const hsa_queue_t *queue,
                                           uint64_t expected, uint64_t value);

uint64_t hsa_queue_cas_write_index_release(const hsa_queue_t *queue,
                                           uint64_t expected, uint64_t value);
void hsa_signal_and_acq_rel(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_and_acquire(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_and_relaxed(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_and_release(hsa_signal_t signal, hsa_signal_value_t value);
uint64_t hsa_queue_load_read_index_acquire(const hsa_queue_t *queue);

uint64_t hsa_queue_load_read_index_relaxed(const hsa_queue_t *queue);
hsa_status_t hsa_executable_load_code_object(hsa_executable_t executable,
                                             hsa_agent_t agent,
                                             hsa_code_object_t code_object,
                                             const char *options);
uint64_t hsa_queue_load_write_index_acquire(const hsa_queue_t *queue);

uint64_t hsa_queue_load_write_index_relaxed(const hsa_queue_t *queue);
hsa_status_t hsa_agent_get_exception_policies(hsa_agent_t agent,
                                              hsa_profile_t profile,
                                              uint16_t *mask);
hsa_status_t hsa_memory_deregister(void *ptr, size_t size);
void hsa_signal_or_acq_rel(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_or_acquire(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_or_relaxed(hsa_signal_t signal, hsa_signal_value_t value);

void hsa_signal_or_release(hsa_signal_t signal, hsa_signal_value_t value);
hsa_status_t hsa_soft_queue_create(hsa_region_t region, uint32_t size,
                                   hsa_queue_type_t type, uint32_t features,
                                   hsa_signal_t doorbell_signal,
                                   hsa_queue_t **queue);
hsa_status_t hsa_executable_iterate_symbols(
    hsa_executable_t executable,
    hsa_status_t (*callback)(hsa_executable_t executable,
                             hsa_executable_symbol_t symbol, void *data),
    void *data);
hsa_status_t hsa_memory_register(void *ptr, size_t size);
void hsa_queue_store_write_index_relaxed(const hsa_queue_t *queue,
                                         uint64_t value);

void hsa_queue_store_write_index_release(const hsa_queue_t *queue,
                                         uint64_t value);
hsa_status_t hsa_executable_global_variable_define(hsa_executable_t executable,
                                                   const char *variable_name,
                                                   void *address);
hsa_status_t hsa_executable_destroy(hsa_executable_t executable);
hsa_status_t hsa_code_object_destroy(hsa_code_object_t code_object);
hsa_status_t hsa_memory_allocate(hsa_region_t region, size_t size, void **ptr);
hsa_signal_value_t hsa_signal_exchange_acq_rel(hsa_signal_t signal,
                                               hsa_signal_value_t value);

hsa_signal_value_t hsa_signal_exchange_acquire(hsa_signal_t signal,
                                               hsa_signal_value_t value);

hsa_signal_value_t hsa_signal_exchange_relaxed(hsa_signal_t signal,
                                               hsa_signal_value_t value);

hsa_signal_value_t hsa_signal_exchange_release(hsa_signal_t signal,
                                               hsa_signal_value_t value);
hsa_status_t hsa_agent_get_info(hsa_agent_t agent, hsa_agent_info_t attribute,
                                void *value);
hsa_status_t hsa_init();
hsa_status_t hsa_system_get_info(hsa_system_info_t attribute, void *value);
hsa_status_t hsa_executable_create(hsa_profile_t profile,
                                   hsa_executable_state_t executable_state,
                                   const char *options,
                                   hsa_executable_t *executable);

#endif /* _HSA_H */

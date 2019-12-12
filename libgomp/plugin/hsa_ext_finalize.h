/* HSA Extensions API 1.0.1 representation description.
   Copyright (C) 2016-2019 Free Software Foundation, Inc.

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


#ifndef _HSA_EXT_FINALIZE_H
#define _HSA_EXT_FINALIZE_H 1

struct BrigModuleHeader;
typedef struct BrigModuleHeader *BrigModule_t;

typedef enum {
  HSA_EXT_IMAGE_GEOMETRY_1D = 0,
  HSA_EXT_IMAGE_GEOMETRY_2D = 1,
  HSA_EXT_IMAGE_GEOMETRY_3D = 2,
  HSA_EXT_IMAGE_GEOMETRY_1DA = 3,
  HSA_EXT_IMAGE_GEOMETRY_2DA = 4,
  HSA_EXT_IMAGE_GEOMETRY_1DB = 5,
  HSA_EXT_IMAGE_GEOMETRY_2DDEPTH = 6,
  HSA_EXT_IMAGE_GEOMETRY_2DADEPTH = 7
} hsa_ext_image_geometry_t;

typedef enum {
  HSA_EXT_IMAGE_CHANNEL_TYPE_SNORM_INT8 = 0,
  HSA_EXT_IMAGE_CHANNEL_TYPE_SNORM_INT16 = 1,
  HSA_EXT_IMAGE_CHANNEL_TYPE_UNORM_INT8 = 2,
  HSA_EXT_IMAGE_CHANNEL_TYPE_UNORM_INT16 = 3,
  HSA_EXT_IMAGE_CHANNEL_TYPE_UNORM_INT24 = 4,
  HSA_EXT_IMAGE_CHANNEL_TYPE_UNORM_SHORT_555 = 5,
  HSA_EXT_IMAGE_CHANNEL_TYPE_UNORM_SHORT_565 = 6,
  HSA_EXT_IMAGE_CHANNEL_TYPE_UNORM_SHORT_101010 = 7,
  HSA_EXT_IMAGE_CHANNEL_TYPE_SIGNED_INT8 = 8,
  HSA_EXT_IMAGE_CHANNEL_TYPE_SIGNED_INT16 = 9,
  HSA_EXT_IMAGE_CHANNEL_TYPE_SIGNED_INT32 = 10,
  HSA_EXT_IMAGE_CHANNEL_TYPE_UNSIGNED_INT8 = 11,
  HSA_EXT_IMAGE_CHANNEL_TYPE_UNSIGNED_INT16 = 12,
  HSA_EXT_IMAGE_CHANNEL_TYPE_UNSIGNED_INT32 = 13,
  HSA_EXT_IMAGE_CHANNEL_TYPE_HALF_FLOAT = 14,
  HSA_EXT_IMAGE_CHANNEL_TYPE_FLOAT = 15
} hsa_ext_image_channel_type_t;

typedef enum {
  HSA_EXT_IMAGE_CHANNEL_ORDER_A = 0,
  HSA_EXT_IMAGE_CHANNEL_ORDER_R = 1,
  HSA_EXT_IMAGE_CHANNEL_ORDER_RX = 2,
  HSA_EXT_IMAGE_CHANNEL_ORDER_RG = 3,
  HSA_EXT_IMAGE_CHANNEL_ORDER_RGX = 4,
  HSA_EXT_IMAGE_CHANNEL_ORDER_RA = 5,
  HSA_EXT_IMAGE_CHANNEL_ORDER_RGB = 6,
  HSA_EXT_IMAGE_CHANNEL_ORDER_RGBX = 7,
  HSA_EXT_IMAGE_CHANNEL_ORDER_RGBA = 8,
  HSA_EXT_IMAGE_CHANNEL_ORDER_BGRA = 9,
  HSA_EXT_IMAGE_CHANNEL_ORDER_ARGB = 10,
  HSA_EXT_IMAGE_CHANNEL_ORDER_ABGR = 11,
  HSA_EXT_IMAGE_CHANNEL_ORDER_SRGB = 12,
  HSA_EXT_IMAGE_CHANNEL_ORDER_SRGBX = 13,
  HSA_EXT_IMAGE_CHANNEL_ORDER_SRGBA = 14,
  HSA_EXT_IMAGE_CHANNEL_ORDER_SBGRA = 15,
  HSA_EXT_IMAGE_CHANNEL_ORDER_INTENSITY = 16,
  HSA_EXT_IMAGE_CHANNEL_ORDER_LUMINANCE = 17,
  HSA_EXT_IMAGE_CHANNEL_ORDER_DEPTH = 18,
  HSA_EXT_IMAGE_CHANNEL_ORDER_DEPTH_STENCIL = 19
} hsa_ext_image_channel_order_t;

typedef struct hsa_ext_image_format_s
{
  hsa_ext_image_channel_type_t channel_type;
  hsa_ext_image_channel_order_t channel_order;
} hsa_ext_image_format_t;

typedef struct hsa_ext_sampler_s
{
  uint64_t handle;
} hsa_ext_sampler_t;
typedef struct hsa_ext_image_data_info_s
{
  size_t size;
  size_t alignment;
} hsa_ext_image_data_info_t;
typedef enum {
  HSA_EXT_SAMPLER_ADDRESSING_MODE_UNDEFINED = 0,
  HSA_EXT_SAMPLER_ADDRESSING_MODE_CLAMP_TO_EDGE = 1,
  HSA_EXT_SAMPLER_ADDRESSING_MODE_CLAMP_TO_BORDER = 2,
  HSA_EXT_SAMPLER_ADDRESSING_MODE_REPEAT = 3,
  HSA_EXT_SAMPLER_ADDRESSING_MODE_MIRRORED_REPEAT = 4
} hsa_ext_sampler_addressing_mode_t;
typedef struct hsa_ext_image_s
{
  uint64_t handle;
} hsa_ext_image_t;
typedef enum {
  HSA_EXT_IMAGE_CAPABILITY_NOT_SUPPORTED = 0x0,
  HSA_EXT_IMAGE_CAPABILITY_READ_ONLY = 0x1,
  HSA_EXT_IMAGE_CAPABILITY_WRITE_ONLY = 0x2,
  HSA_EXT_IMAGE_CAPABILITY_READ_WRITE = 0x4,
  HSA_EXT_IMAGE_CAPABILITY_READ_MODIFY_WRITE = 0x8,
  HSA_EXT_IMAGE_CAPABILITY_ACCESS_INVARIANT_DATA_LAYOUT = 0x10
} hsa_ext_image_capability_t;
typedef struct hsa_ext_control_directives_s
{
  uint64_t control_directives_mask;
  uint16_t break_exceptions_mask;
  uint16_t detect_exceptions_mask;
  uint32_t max_dynamic_group_size;
  uint64_t max_flat_grid_size;
  uint32_t max_flat_workgroup_size;
  uint32_t reserved1;
  uint64_t required_grid_size[3];
  hsa_dim3_t required_workgroup_size;
  uint8_t required_dim;
  uint8_t reserved2[75];
} hsa_ext_control_directives_t;
typedef enum {
  HSA_EXT_SAMPLER_FILTER_MODE_NEAREST = 0,
  HSA_EXT_SAMPLER_FILTER_MODE_LINEAR = 1
} hsa_ext_sampler_filter_mode_t;

typedef enum {
  HSA_EXT_SAMPLER_COORDINATE_MODE_UNNORMALIZED = 0,
  HSA_EXT_SAMPLER_COORDINATE_MODE_NORMALIZED = 1
} hsa_ext_sampler_coordinate_mode_t;
typedef enum {
  HSA_EXT_FINALIZER_CALL_CONVENTION_AUTO = -1
} hsa_ext_finalizer_call_convention_t;
typedef struct hsa_ext_program_s
{
  uint64_t handle;
} hsa_ext_program_t;
typedef struct hsa_ext_image_descriptor_s
{
  hsa_ext_image_geometry_t geometry;
  size_t width;
  size_t height;
  size_t depth;
  size_t array_size;
  hsa_ext_image_format_t format;
} hsa_ext_image_descriptor_t;
typedef enum {
  HSA_EXT_PROGRAM_INFO_MACHINE_MODEL = 0,
  HSA_EXT_PROGRAM_INFO_PROFILE = 1,
  HSA_EXT_PROGRAM_INFO_DEFAULT_FLOAT_ROUNDING_MODE = 2
} hsa_ext_program_info_t;
typedef BrigModule_t hsa_ext_module_t;
typedef struct hsa_ext_sampler_descriptor_s
{
  hsa_ext_sampler_coordinate_mode_t coordinate_mode;
  hsa_ext_sampler_filter_mode_t filter_mode;
  hsa_ext_sampler_addressing_mode_t address_mode;
} hsa_ext_sampler_descriptor_t;

typedef struct hsa_ext_image_region_s
{
  hsa_dim3_t offset;
  hsa_dim3_t range;
} hsa_ext_image_region_t;
hsa_status_t hsa_ext_image_export (hsa_agent_t agent, hsa_ext_image_t src_image,
				   void *dst_memory, size_t dst_row_pitch,
				   size_t dst_slice_pitch,
				   const hsa_ext_image_region_t *image_region);
hsa_status_t hsa_ext_program_add_module (hsa_ext_program_t program,
					 hsa_ext_module_t module);
hsa_status_t hsa_ext_program_iterate_modules (
  hsa_ext_program_t program,
  hsa_status_t (*callback) (hsa_ext_program_t program, hsa_ext_module_t module,
			    void *data),
  void *data);
hsa_status_t hsa_ext_program_create (
  hsa_machine_model_t machine_model, hsa_profile_t profile,
  hsa_default_float_rounding_mode_t default_float_rounding_mode,
  const char *options, hsa_ext_program_t *program);
hsa_status_t
hsa_ext_image_data_get_info (hsa_agent_t agent,
			     const hsa_ext_image_descriptor_t *image_descriptor,
			     hsa_access_permission_t access_permission,
			     hsa_ext_image_data_info_t *image_data_info);

hsa_status_t hsa_ext_image_import (hsa_agent_t agent, const void *src_memory,
				   size_t src_row_pitch, size_t src_slice_pitch,
				   hsa_ext_image_t dst_image,
				   const hsa_ext_image_region_t *image_region);
hsa_status_t hsa_ext_program_get_info (hsa_ext_program_t program,
				       hsa_ext_program_info_t attribute,
				       void *value);
enum
{
  HSA_EXT_STATUS_ERROR_IMAGE_FORMAT_UNSUPPORTED = 0x3000,
  HSA_EXT_STATUS_ERROR_IMAGE_SIZE_UNSUPPORTED = 0x3001
};
hsa_status_t hsa_ext_image_destroy (hsa_agent_t agent, hsa_ext_image_t image);
hsa_status_t hsa_ext_image_get_capability (
  hsa_agent_t agent, hsa_ext_image_geometry_t geometry,
  const hsa_ext_image_format_t *image_format, uint32_t *capability_mask);
enum
{
  HSA_EXT_STATUS_ERROR_INVALID_PROGRAM = 0x2000,
  HSA_EXT_STATUS_ERROR_INVALID_MODULE = 0x2001,
  HSA_EXT_STATUS_ERROR_INCOMPATIBLE_MODULE = 0x2002,
  HSA_EXT_STATUS_ERROR_MODULE_ALREADY_INCLUDED = 0x2003,
  HSA_EXT_STATUS_ERROR_SYMBOL_MISMATCH = 0x2004,
  HSA_EXT_STATUS_ERROR_FINALIZATION_FAILED = 0x2005,
  HSA_EXT_STATUS_ERROR_DIRECTIVE_MISMATCH = 0x2006
};
hsa_status_t hsa_ext_sampler_destroy (hsa_agent_t agent,
				      hsa_ext_sampler_t sampler);
hsa_status_t hsa_ext_program_finalize (
  hsa_ext_program_t program, hsa_isa_t isa, int32_t call_convention,
  hsa_ext_control_directives_t control_directives, const char *options,
  hsa_code_object_type_t code_object_type, hsa_code_object_t *code_object);
hsa_status_t hsa_ext_image_create (
  hsa_agent_t agent, const hsa_ext_image_descriptor_t *image_descriptor,
  const void *image_data, hsa_access_permission_t access_permission,
  hsa_ext_image_t *image);
hsa_status_t hsa_ext_program_destroy (hsa_ext_program_t program);
hsa_status_t hsa_ext_image_copy (hsa_agent_t agent, hsa_ext_image_t src_image,
				 const hsa_dim3_t *src_offset,
				 hsa_ext_image_t dst_image,
				 const hsa_dim3_t *dst_offset,
				 const hsa_dim3_t *range);
hsa_status_t hsa_ext_image_clear (hsa_agent_t agent, hsa_ext_image_t image,
				  const void *data,
				  const hsa_ext_image_region_t *image_region);
enum
{
  HSA_EXT_AGENT_INFO_IMAGE_1D_MAX_ELEMENTS = 0x3000,
  HSA_EXT_AGENT_INFO_IMAGE_1DA_MAX_ELEMENTS = 0x3001,
  HSA_EXT_AGENT_INFO_IMAGE_1DB_MAX_ELEMENTS = 0x3002,
  HSA_EXT_AGENT_INFO_IMAGE_2D_MAX_ELEMENTS = 0x3003,
  HSA_EXT_AGENT_INFO_IMAGE_2DA_MAX_ELEMENTS = 0x3004,
  HSA_EXT_AGENT_INFO_IMAGE_2DDEPTH_MAX_ELEMENTS = 0x3005,
  HSA_EXT_AGENT_INFO_IMAGE_2DADEPTH_MAX_ELEMENTS = 0x3006,
  HSA_EXT_AGENT_INFO_IMAGE_3D_MAX_ELEMENTS = 0x3007,
  HSA_EXT_AGENT_INFO_IMAGE_ARRAY_MAX_LAYERS = 0x3008,
  HSA_EXT_AGENT_INFO_MAX_IMAGE_RD_HANDLES = 0x3009,
  HSA_EXT_AGENT_INFO_MAX_IMAGE_RORW_HANDLES = 0x300A,
  HSA_EXT_AGENT_INFO_MAX_SAMPLER_HANDLERS = 0x300B
};
hsa_status_t
hsa_ext_sampler_create (hsa_agent_t agent,
			const hsa_ext_sampler_descriptor_t *sampler_descriptor,
			hsa_ext_sampler_t *sampler);

#endif /* _HSA_EXT_FINALIZE_H */

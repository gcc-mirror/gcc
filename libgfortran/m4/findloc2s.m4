dnl Support macros for findloc.
dnl This file is part of the GNU Fortran Runtime Library (libgfortran)
dnl Distributed under the GNU GPL with exception.  See COPYING for details.
include(iparm.m4)dnl
define(header1,`index_type findloc2_'atype_code` ('atype` * const restrict array,
			   const 'atype_name` * restrict value, GFC_LOGICAL_4 back,
			   gfc_charlen_type len_array, gfc_charlen_type len_value);
export_proto(findloc2_'atype_code`);

index_type
findloc2_'atype_code` ('atype` * const restrict array, const 'atype_name` * restrict value,
		      GFC_LOGICAL_4 back,
		      gfc_charlen_type len_array, gfc_charlen_type len_value)')dnl
dnl
define(header2,`index_type mfindloc2_'atype_code` ('atype` * const restrict array,
			 const 'atype_name` * restrict value,
			 gfc_array_l1 *const restrict mask, GFC_LOGICAL_4 back,
			 gfc_charlen_type len_array, gfc_charlen_type len_value);
export_proto(mfindloc2_'atype_code`);

index_type
mfindloc2_'atype_code` ('atype` * const restrict array,
			   const 'atype_name` * restrict value, gfc_array_l1 *const restrict mask,
			   GFC_LOGICAL_4 back, gfc_charlen_type len_array,
			   gfc_charlen_type len_value)')dnl
dnl
define(header3,`index_type sfindloc2_'atype_code` ('atype` * const restrict array,
			 const 'atype_name` * restrict value,
			 GFC_LOGICAL_4 *const restrict mask, GFC_LOGICAL_4 back,
			 gfc_charlen_type len_array, gfc_charlen_type len_value);
export_proto(sfindloc2_'atype_code`);

index_type
sfindloc2_'atype_code` ('atype` * const restrict array,
			   const 'atype_name` * restrict value, GFC_LOGICAL_4 *const restrict mask,
			   GFC_LOGICAL_4 back, gfc_charlen_type len_array,
			   gfc_charlen_type len_value)')dnl
dnl
define(comparison,ifelse(atype_kind,4,dnl
`compare_string_char4 (len_array, src, len_value, value) == 0',dnl
`compare_string (len_array, (char *) src, len_value, (char *) value) == 0'))dnl
define(len_arg,`len_array, len_value')dnl
define(base_mult,`len_array')dnl
include(ifindloc2.m4)dnl

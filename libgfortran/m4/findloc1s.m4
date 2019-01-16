dnl Support macros for findloc.
dnl This file is part of the GNU Fortran Runtime Library (libgfortran)
dnl Distributed under the GNU GPL with exception.  See COPYING for details.
include(iparm.m4)dnl
define(header1,`extern void findloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
		         'atype` * const restrict array, 'atype_name` *const restrict value,
			 const 'index_type` * restrict pdim, GFC_LOGICAL_4 back,
			 gfc_charlen_type len_array, gfc_charlen_type len_value);
export_proto(findloc1_'atype_code`);

extern void
findloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
	    'atype` * const restrict array, 'atype_name` *const restrict value,
	    const 'index_type` * restrict pdim, GFC_LOGICAL_4 back,
	    gfc_charlen_type len_array, gfc_charlen_type len_value)')dnl
dnl
define(header2,`extern void mfindloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
		         'atype` * const restrict array, 'atype_name` *const restrict value,
			 const 'index_type` * restrict pdim, gfc_array_l1 *const restrict mask,
			 GFC_LOGICAL_4 back, gfc_charlen_type len_array, gfc_charlen_type len_value);
export_proto(mfindloc1_'atype_code`);

extern void
mfindloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
	    'atype` * const restrict array, 'atype_name` *const restrict value,
	    const 'index_type` * restrict pdim, gfc_array_l1 *const restrict mask,
	    GFC_LOGICAL_4 back, gfc_charlen_type len_array, gfc_charlen_type len_value)')dnl
define(header3,`extern void sfindloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
		         'atype` * const restrict array, 'atype_name` *const restrict value,
			 const 'index_type` * restrict pdim, GFC_LOGICAL_4 *const restrict mask,
			 GFC_LOGICAL_4 back, gfc_charlen_type len_array, gfc_charlen_type len_value);
export_proto(sfindloc1_'atype_code`);

extern void
sfindloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
	    'atype` * const restrict array, 'atype_name` *const restrict value,
	    const 'index_type` * restrict pdim, GFC_LOGICAL_4 *const restrict  mask,
	    GFC_LOGICAL_4 back, gfc_charlen_type len_array, gfc_charlen_type len_value)')dnl
define(comparison,ifelse(atype_kind,4,dnl
`compare_string_char4 (len_array, src, len_value, value) == 0',dnl
`compare_string (len_array, (char *) src, len_value, (char *) value) == 0'))dnl
define(len_arg,`, len_array, len_value')dnl
define(base_mult,`len_array')dnl
include(ifindloc1.m4)dnl

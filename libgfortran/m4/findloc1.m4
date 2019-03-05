dnl Support macros for findloc.
dnl This file is part of the GNU Fortran Runtime Library (libgfortran)
dnl Distributed under the GNU GPL with exception.  See COPYING for details.
include(iparm.m4)dnl
define(header1,`extern void findloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
		         'atype` * const restrict array, 'atype_name` value,
			 const 'index_type` * restrict pdim, GFC_LOGICAL_4 back);
export_proto(findloc1_'atype_code`);

extern void
findloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
	    'atype` * const restrict array, 'atype_name` value,
	    const 'index_type` * restrict pdim, GFC_LOGICAL_4 back)')dnl
dnl
define(header2,`extern void mfindloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
		         'atype` * const restrict array, 'atype_name` value,
			 const 'index_type` * restrict pdim, gfc_array_l1 *const restrict mask,
			 GFC_LOGICAL_4 back);
export_proto(mfindloc1_'atype_code`);

extern void
mfindloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
	    'atype` * const restrict array, 'atype_name` value,
	    const 'index_type` * restrict pdim, gfc_array_l1 *const restrict mask,
	    GFC_LOGICAL_4 back)')dnl
define(header3,`extern void sfindloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
		         'atype` * const restrict array, 'atype_name` value,
			 const 'index_type` * restrict pdim, GFC_LOGICAL_4 *const restrict mask,
			 GFC_LOGICAL_4 back);
export_proto(sfindloc1_'atype_code`);

extern void
sfindloc1_'atype_code` (gfc_array_index_type * const restrict retarray,
	    'atype` * const restrict array, 'atype_name` value,
	    const 'index_type` * restrict pdim, GFC_LOGICAL_4 *const restrict  mask,
	    GFC_LOGICAL_4 back)')dnl
define(comparison,`*src == value')dnl
define(len_arg,`')dnl
define(base_mult,1)dnl
include(ifindloc1.m4)dnl

dnl Support macros for findloc.
dnl This file is part of the GNU Fortran Runtime Library (libgfortran)
dnl Distributed under the GNU GPL with exception.  See COPYING for details.
include(iparm.m4)dnl
define(header1,`extern void findloc0_'atype_code` (gfc_array_index_type * const restrict retarray,
       	    		'atype` * const restrict array, 'atype_name` value,
			 GFC_LOGICAL_4);
export_proto(findloc0_'atype_code`);

void
findloc0_'atype_code` (gfc_array_index_type * const restrict retarray,
    	    'atype` * const restrict array, 'atype_name` value,
	    GFC_LOGICAL_4 back)')dnl
dnl
define(header2,`extern void mfindloc0_'atype_code` (gfc_array_index_type * const restrict retarray,
       	    		'atype` * const restrict array, 'atype_name` value,
			 gfc_array_l1 *const restrict, GFC_LOGICAL_4);
export_proto(mfindloc0_'atype_code`);

void
mfindloc0_'atype_code` (gfc_array_index_type * const restrict retarray,
    	    'atype` * const restrict array, 'atype_name` value,
	    gfc_array_l1 *const restrict mask, GFC_LOGICAL_4 back)')
dnl
define(header3,`extern void sfindloc0_'atype_code` (gfc_array_index_type * const restrict retarray,
       	    		'atype` * const restrict array, 'atype_name` value,
			 GFC_LOGICAL_4 *, GFC_LOGICAL_4);
export_proto(sfindloc0_'atype_code`);

void
sfindloc0_'atype_code` (gfc_array_index_type * const restrict retarray,
    	    'atype` * const restrict array, 'atype_name` value,
	    GFC_LOGICAL_4 * mask, GFC_LOGICAL_4 back)')dnl
dnl
define(comparison,`*base == value')dnl
define(len_arg,`')dnl
define(base_mult,1)dnl
include(ifindloc0.m4)dnl

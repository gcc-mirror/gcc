/* Area:		ffi_prep_cif
   Purpose:		Test error return for bad typedefs.
   Limitations:	none.
   PR:			none.
   Originator:	Blake Chaffin 6/6/2007	 */

/* { dg-do run { xfail mips*-*-* arm*-*-* strongarm*-*-* xscale*-*-* i*86-*-linux-* x86_64-*-linux-* } } */
#include "ffitest.h"

int main (void)
{
	ffi_cif cif;
#ifndef USING_MMAP
	static ffi_closure cl;
#endif
	ffi_closure *pcl;
	ffi_type* arg_types[1];

#ifdef USING_MMAP
	pcl = allocate_mmap (sizeof(ffi_closure));
#else
	pcl = &cl;
#endif

	arg_types[0] = NULL;

	ffi_type	badType	= ffi_type_void;

	badType.size = 0;

	CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 0, &badType,
		arg_types) == FFI_BAD_TYPEDEF);

	exit(0);
}

/* Area:		ffi_prep_cif, ffi_prep_closure
   Purpose:		Test error return for bad ABIs.
   Limitations:	none.
   PR:			none.
   Originator:	Blake Chaffin 6/6/2007	 */

/* { dg-do run { xfail mips*-*-* arm*-*-* strongarm*-*-* xscale*-*-* i*86-*-linux-* x86_64-*-linux-* } } */
#include "ffitest.h"

static void
dummy_fn(ffi_cif* cif __UNUSED__, void* resp __UNUSED__, 
	 void** args __UNUSED__, void* userdata __UNUSED__)
{}

int main (void)
{
	ffi_cif cif;
#ifndef USING_MMAP
	static ffi_closure cl;
#endif
	ffi_closure *pcl;
	void* args[1];
	ffi_type* arg_types[1];

#ifdef USING_MMAP
	pcl = allocate_mmap (sizeof(ffi_closure));
#else
	pcl = &cl;
#endif

	arg_types[0] = NULL;
	args[0] = NULL;

	CHECK(ffi_prep_cif(&cif, 255, 0, &ffi_type_void,
		arg_types) == FFI_BAD_ABI);

	CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 0, &ffi_type_void,
		arg_types) == FFI_OK);

	cif.abi= 255;

	CHECK(ffi_prep_closure(pcl, &cif, dummy_fn, NULL) == FFI_BAD_ABI);

	exit(0);
}

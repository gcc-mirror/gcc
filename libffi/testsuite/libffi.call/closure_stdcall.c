/* Area:	closure_call (stdcall convention)
   Purpose:	Check handling when caller expects stdcall callee
   Limitations:	none.
   PR:		none.
   Originator:	<twalljava@dev.java.net> */

/* { dg-do run { target i?86-*-cygwin* i?86-*-mingw* } } */
#include "ffitest.h"

static void
closure_test_stdcall(ffi_cif* cif __UNUSED__, void* resp, void** args,
		 void* userdata)
{
  *(ffi_arg*)resp =
    (int)*(int *)args[0] + (int)(*(int *)args[1])
    + (int)(*(int *)args[2])  + (int)(*(int *)args[3])
    + (int)(intptr_t)userdata;

  printf("%d %d %d %d: %d\n",
	 (int)*(int *)args[0], (int)(*(int *)args[1]),
	 (int)(*(int *)args[2]), (int)(*(int *)args[3]),
         (int)*(ffi_arg *)resp);

}

typedef int (__stdcall *closure_test_type0)(int, int, int, int);

int main (void)
{
  ffi_cif cif;
#ifndef USING_MMAP
  static ffi_closure cl;
#endif
  ffi_closure *pcl;
  ffi_type * cl_arg_types[17];
  int res;
  void* sp_pre;
  void* sp_post;
  char buf[1024];

#ifdef USING_MMAP
  pcl = allocate_mmap (sizeof(ffi_closure));
#else
  pcl = &cl;
#endif

  cl_arg_types[0] = &ffi_type_uint;
  cl_arg_types[1] = &ffi_type_uint;
  cl_arg_types[2] = &ffi_type_uint;
  cl_arg_types[3] = &ffi_type_uint;
  cl_arg_types[4] = NULL;

  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_STDCALL, 4,
		     &ffi_type_sint, cl_arg_types) == FFI_OK);

  CHECK(ffi_prep_closure(pcl, &cif, closure_test_stdcall,
			 (void *) 3 /* userdata */) == FFI_OK);

  asm volatile (" movl %%esp,%0" : "=g" (sp_pre));
  res = (*(closure_test_type0)pcl)(0, 1, 2, 3);
  asm volatile (" movl %%esp,%0" : "=g" (sp_post));
  /* { dg-output "0 1 2 3: 9" } */

  printf("res: %d\n",res);
  /* { dg-output "\nres: 9" } */

  sprintf(buf, "mismatch: pre=%p vs post=%p", sp_pre, sp_post);
  printf("stack pointer %s\n", (sp_pre == sp_post ? "match" : buf));
  /* { dg-output "\nstack pointer match" } */
  exit(0);
}

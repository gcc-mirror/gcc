#include <stdlib.h>
#include <stdio.h>


void __vtv_verify_fail (void **, void*) __attribute__((visibility ("default")));

void
__vtv_verify_fail (void **hash_table, const void *vtbl_ptr)
{
  fprintf (stdout, "Executing alternative failure routine.\n");
}

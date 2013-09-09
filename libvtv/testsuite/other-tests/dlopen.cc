#include <stdlib.h>
#include <dlfcn.h>
#include <stdio.h>



typedef void (*voidfn)(void);

int failures = 0;

void
__vtv_verify_fail (void **data_set_ptr, const void *vtbl_pointer)
{
  failures++;
  return;
}


int main()
{
  char so_name[] = "so0.so";
  void * dlhandle = dlopen(so_name, RTLD_NOW);
  if (!dlhandle)
    {
      fprintf(stderr, "dlopen %s error: %s\n", so_name, dlerror());
      exit(1);
    }
  voidfn so_entry = (voidfn)dlsym(dlhandle, "so_entry_0");
  if (!so_entry)
    {
      fprintf(stderr, "dlopen %s dlsym error: %s\n", so_name, dlerror());
      exit(2);
    }

  so_entry();

  dlclose(dlhandle);
}

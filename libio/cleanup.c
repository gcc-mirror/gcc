#include "libioP.h"
#if _G_HAVE_ATEXIT
#include <stdlib.h>

typedef void (*voidfunc) __P((void));

static void
_IO_register_cleanup ()
{
  atexit ((voidfunc)_IO_cleanup);
  _IO_cleanup_registration_needed = 0;
}

void (*_IO_cleanup_registration_needed)() = _IO_register_cleanup;
#else
void (*_IO_cleanup_registration_needed)() = NULL;
#endif /* _G_HAVE_ATEXIT */

/* darwin.cc - class loader stuff for Darwin.  */

/* Copyright (C) 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <jvm.h>

/* In theory, we should be able to do:
   #include <mach-o/getsect.h>
   #include <mach-o/dyld.h>

   but all the types in these headers changed between Panther and Tiger,
   so the only way to be avoid type mismatches is to declare the routines
   ourself.  */

#include <stdint.h>
struct mach_header;
extern "C" void _dyld_register_func_for_add_image
  (void (*func)(const struct mach_header *mh, intptr_t vmaddr_slide));
extern "C" void _dyld_register_func_for_remove_image
  (void (*func)(const struct mach_header *mh, intptr_t vmaddr_slide));
extern "C" char *getsectdatafromheader
(const struct mach_header *mhp, const char *segname, const char *sectname,
 uint32_t *size);

/* When a new image is loaded, look to see if it has a jcr section
   and if so register the classes listed in it.  */

static void
darwin_java_register_dyld_add_image_hook (const struct mach_header *mh,
					  intptr_t slide)
{
  char *fde;
  uint32_t sz;

  fde = getsectdatafromheader (mh, "__DATA", "jcr", &sz);
  if (! fde)
    return;
  
  /* As far as I can tell, you're only supposed to load shared
     libraries while having a lock on java.lang.Class.  So there's
     no need to synchronize on anything here.  (I'm not sure how exactly
     you can ensure this given lazy library loading.  FIXME.)  */
 
  _Jv_RegisterClasses_Counted ((const jclass *) (fde + slide),
			       sz / sizeof (jclass *));
}

static struct darwin_constructor_s{
  darwin_constructor_s() 
  {
    _dyld_register_func_for_add_image 
      (darwin_java_register_dyld_add_image_hook);
    /* At present, you mustn't unload any java plugin.  */
  };
} darwin_constructor;

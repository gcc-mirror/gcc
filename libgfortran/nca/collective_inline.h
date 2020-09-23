#include "collective_subroutine.h"

static inline void
finish_collective_subroutine (collsub_iface *ci) 
{
  collsub_sync (ci);
}

#if 0
static inline void *
get_obj_ptr (void *buffer, int image) 
{
  return (char *) + curr_size * image;
}

/* If obj is NULL, copy the object from the entry in this image.  */
static inline void
copy_to (void *buffer, void *obj, int image)
{
  if (obj == 0)
    obj = get_obj_ptr (this_image.image_num);
  memcpy (get_obj_ptr (image), obj, curr_size);
}

static inline void
copy_out (void *buffer, void *obj, int image)
{
  memcpy (obj, get_obj_ptr (image), curr_size);
}

static inline void
copy_from (void *buffer, int image) 
{
  copy_out (get_obj_ptr (this_image.image_num), image);
}

static inline void
copy_in (void *buffer, void *obj)
{
  copy_to (obj, this_image.image_num);
}
#endif

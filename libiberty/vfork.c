/* Emulate vfork using just plain fork, for systems without a real vfork.
   This function is in the public domain. */

#include "ansidecl.h"

extern int fork PARAMS ((void));

int
vfork ()
{
  return (fork ());
}

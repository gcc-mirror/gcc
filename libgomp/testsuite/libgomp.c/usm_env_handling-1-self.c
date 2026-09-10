/* { dg-do run }  */

/* Check that a global static variable is consistently updated on the device. */

// PR middle-end/127273 for an ICE fix
// PR middle-end/122205 for converting enter to link for self_maps

#define USE_SELF_MAPS 1
#include "usm_env_handling-1.c"

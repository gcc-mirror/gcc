/* Verify OpenACC 'firstprivate' mappings.  */

/* { dg-additional-options "-Wno-psabi" } as apparently we're doing funny
   things with vector arguments.  */

/* PR middle-end/48591 */
/* PR other/71064 */
/* Set to 0 for offloading targets not supporting long double.  */
#if defined(ACC_DEVICE_TYPE_nvidia) || defined(ACC_DEVICE_TYPE_radeon)
# define DO_LONG_DOUBLE 0
#else
# define DO_LONG_DOUBLE 1
#endif

#include "../../../gcc/testsuite/c-c++-common/goacc/firstprivate-mappings-1.c"

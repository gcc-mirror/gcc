/* Verify OpenACC 'firstprivate' mappings for C++ reference types.  */

/* PR middle-end/48591 */
/* PR other/71064 */
/* Set to 0 for offloading targets not supporting long double.  */
#if defined(ACC_DEVICE_TYPE_nvidia) || defined(ACC_DEVICE_TYPE_gcn)
# define DO_LONG_DOUBLE 0
#else
# define DO_LONG_DOUBLE 1
#endif

#include "../../../gcc/testsuite/g++.dg/goacc/firstprivate-mappings-1.C"

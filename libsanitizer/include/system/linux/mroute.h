#include <linux/version.h>
/* <linux/mroute.h> before 2.6.26 included <linux/in.h>
   which clashes with userspace headers.  */
#if LINUX_VERSION_CODE < 132634
#define _LINUX_IN_H
#include <linux/types.h>
#endif
#include_next <linux/mroute.h>

#include <linux/version.h>
/* <linux/mroute6.h> has been added in 2.6.26 */
#if LINUX_VERSION_CODE >= 132634
#include_next <linux/mroute6.h>
#endif

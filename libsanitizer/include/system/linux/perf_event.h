#include <linux/version.h>
/* <linux/perf_event.h> has been added in 2.6.32 */
#if LINUX_VERSION_CODE >= 132640
#include_next <linux/perf_event.h>
#else
#define perf_event_attr __sanitizer_perf_event_attr
#endif

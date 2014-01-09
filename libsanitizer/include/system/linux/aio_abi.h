#include <linux/version.h>
#include_next <linux/aio_abi.h>
/* IOCB_CMD_PREADV/PWRITEV has been added in 2.6.19 */
#if LINUX_VERSION_CODE < 132627
#define IOCB_CMD_PREADV 7
#define IOCB_CMD_PWRITEV 8
#endif

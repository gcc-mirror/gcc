/* This gives the effect of

	subroutine exit(rc)
	integer*4 rc
	stop
	end

 * with the added side effect of supplying rc as the program's exit code.
 */

#include "f2c.h"
#undef abs
#undef min
#undef max
#include <stdlib.h>
#ifdef __cplusplus
extern "C" {
#endif
extern void f_exit(void);

 void
G77_exit_0 (integer *rc)
{
#ifdef NO_ONEXIT
	f_exit();
#endif
	exit(*rc);
	}
#ifdef __cplusplus
}
#endif

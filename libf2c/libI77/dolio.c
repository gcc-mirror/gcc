#include "config.h"
#include "f2c.h"

#ifdef __cplusplus
extern "C" {
#endif
extern int (*f__lioproc)(ftnint*, char*, ftnlen, ftnint);

integer do_lio(ftnint *type, ftnint *number, char *ptr, ftnlen len)
{
	return((*f__lioproc)(number,ptr,len,*type));
}
#ifdef __cplusplus
	}
#endif

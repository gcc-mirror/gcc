#include "config.h"
#include "f2c.h"

ftnlen f__typesize[] = { 0, 0, sizeof(shortint), sizeof(integer),
			sizeof(real), sizeof(doublereal),
			sizeof(complex), sizeof(doublecomplex),
			sizeof(logical), sizeof(char),
			0, sizeof(integer1),
			sizeof(logical1), sizeof(shortlogical),
#ifdef Allow_TYQUAD
			sizeof(longint),
#endif
			0};

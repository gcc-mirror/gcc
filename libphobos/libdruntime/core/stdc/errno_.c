/**
 * This file contains wrapper functions for macro-defined C routines.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/stdc/errno.c)
 */

#include <errno.h>


int getErrno()
{
    return errno;
}


int setErrno( int val )
{
    errno = val;
    return val;
}

/* date_.f -- translated by f2c (version 19961001).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;

/* Subroutine */ int G77_date_y2kbug_0 (char *buf, ftnlen buf_len)
{
    /* System generated locals */
    address a__1[5];
    longint i__1;
    integer i__2[5];
    char ch__1[24];

    /* Builtin functions */
    /* Subroutine */ int s_copy(), s_cat();

    /* Local variables */
    static char cbuf[24];
    extern longint G77_time_0 ();
    extern /* Character */ VOID G77_ctime_0 ();

    i__1 = G77_time_0 ();
    G77_ctime_0 (ch__1, 24L, &i__1);
    s_copy(cbuf, ch__1, 24L, 24L);
/* Writing concatenation */
    i__2[0] = 2, a__1[0] = cbuf + 8;
    i__2[1] = 1, a__1[1] = "-";
    i__2[2] = 3, a__1[2] = cbuf + 4;
    i__2[3] = 1, a__1[3] = "-";
    i__2[4] = 2, a__1[4] = cbuf + 22;
    s_cat(buf, a__1, i__2, &c__5, buf_len);
    return 0;
} /* date_ */


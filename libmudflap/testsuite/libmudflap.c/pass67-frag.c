/* PR middle-end/35314 */
/* { dg-do compile } */
/* { dg-options "-fmudflap" } */

#include <setjmp.h>

jmp_buf buf;

void
foo (volatile char *p)
{
  if (__builtin_setjmp (buf))
    *p;
}

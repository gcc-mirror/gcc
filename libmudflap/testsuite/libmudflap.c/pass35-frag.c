#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char end [];   /* Any old symbol we're sure will be defined. */
/* { dg-warning "cannot track lifetime of `end'" "cannot track lifetime" { target *-*-* } 0 } */

int main ()
{
/* dummy register */
__mf_register ((void *) end, 1, __MF_TYPE_GUESS, "end");
char z = end[0];
return z & 0;
}

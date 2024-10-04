// { dg-do compile { target *-*-linux* } }
// { dg-add-options no_pch }

#define _FORTIFY_SOURCE 2
// Now we can define the macros to poison uses of non-reserved names:
#include "names.cc"

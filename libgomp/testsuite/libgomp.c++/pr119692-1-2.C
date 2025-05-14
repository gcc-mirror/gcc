/* PR119692 "C++ 'typeinfo', 'vtable' vs. OpenACC, OpenMP 'target' offloading" */

/* { dg-additional-options -DDEFAULT=defaultmap(none) }
   Fails to compile.
   { dg-do compile } */

#include "pr119692-1-1.C"

/* { dg-bogus {error: '_ZTV2C1' not specified in enclosing 'target'} PR119692 { xfail *-*-* } 0 }
   { dg-bogus {error: '_ZTI2C2' not specified in enclosing 'target'} PR119692 { xfail *-*-* } 0 }
   { dg-bogus {error: '_ZTI2C1' not specified in enclosing 'target'} PR119692 { xfail *-*-* } 0 } */

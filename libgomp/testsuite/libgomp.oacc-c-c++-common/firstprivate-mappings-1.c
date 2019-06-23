/* Verify OpenACC 'firstprivate' mappings.  */

/* { dg-additional-options "-Wno-psabi" } as apparently we're doing funny
   things with vector arguments.  */

#include "../../../gcc/testsuite/c-c++-common/goacc/firstprivate-mappings-1.c"

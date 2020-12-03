/* OpenACC 'cache' directive.  */

/* See also corresponding C/C++ variant '../libgomp.oacc-c-c++-common/cache-1.c'.  */

#include "../../../gcc/testsuite/g++.dg/goacc/cache-1.C"

int
main (int argc, char *argv[])
{
  test<0> ();

  return 0;
}

// Test that "begin declare variant" in a module interface is
// visible to things that import the module.

// { dg-additional-sources "bdv_module1_main.C" }
// { dg-additional-options "-fmodules" }

export module bdv_module1;

export int
test ()
{
  return 0;
}

#if _OPENMP
#pragma omp begin declare variant match(construct={parallel})
export int
test ()
{
  return 1;
}
#pragma omp end declare variant
#endif

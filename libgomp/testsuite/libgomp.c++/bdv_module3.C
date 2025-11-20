// Test that "begin declare variant" in a module interface is
// visible to things that import the module, and that it works in
// conjunction with additional "begin declare variant"s local
// to a module implementation TU.

// { dg-additional-sources "bdv_module3_impl.C bdv_module3_main.C" }
// { dg-additional-options "-fmodules" }

export module bdv_module3;

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

export void doit ();

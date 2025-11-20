// Test that "begin declare variant" in a module implementation unit is
// visible only in that unit.

// { dg-additional-sources "bdv_module2_impl.C bdv_module2_main.C" }
// { dg-additional-options "-fmodules" }

export module bdv_module2;

export int
test ()
{
  return 0;
}

export void doit ();

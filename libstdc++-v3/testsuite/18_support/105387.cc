// { dg-do run }

#include <stdexcept>
#include <cxxabi.h>
#include <testsuite_hooks.h>

// Test cases for PR libstdc++/105387

// This test is to trigger undefined behavior if the bug 105387 is present
// in the code. Note, however, given that the bug is present, this test runs
// into undefined behavior which can also mean that it passes.
// It has been observed to fail quite reliably on x86_64-linux-gnu but only
// fail sporadically on Xtensa, depending on the code placement.
void portable_test()
{
  bool exception_thrown = false;
  try {
    throw std::runtime_error("test");
  } catch (const char *e) {
    VERIFY(false);
  } catch (const std::exception &e) {
    exception_thrown = true;
  }
  VERIFY(exception_thrown);
}

// This test relies on the types defined in the files typeinfo and cxxabi.h
// It is therefore less portable then the test case above but should be
// guaranteed to fail if the implementation has the bug 105387.
//
// This test case checks that __pbase_type_info::__do_catch() behaves
// correctly when called with a non-pointer type info object as argument.
// In particular, __pbase_type_info::__do_catch() should not cast
// the given type object into a pointer type and try to access the
// extended fields.

void non_portable_test()
{
  // Create a zero-initialized buffer for allocation of the type object
  unsigned char buffer [sizeof(__cxxabiv1::__fundamental_type_info) * 2] = {};

  // Use placement-new to create the fundamental type info object in the
  // first half of the buffer. Whenever that type info object will be
  // casted to a pointer type info object, the extended fields of the
  // pointer type info object will be in the second half of the buffer
  // and hence be guaranteed zero.
  __cxxabiv1::__fundamental_type_info *p_fund_info
    = new(buffer) __cxxabiv1::__fundamental_type_info("fund_type");

  __cxxabiv1::__pointer_type_info ptr_info("ptr_type", 0, p_fund_info);

  // __do_catch is declared protected in __pointer_type_info, but public in
  // type_info, so we upcast it here
  std::type_info *abstract_ptr_info = static_cast<std::type_info*>(&ptr_info);
  VERIFY(abstract_ptr_info->__do_catch(p_fund_info, 0, 1) == false);
}

int main()
{
  portable_test();
  non_portable_test();
  return 0;
}

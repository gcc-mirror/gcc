// { dg-do compile { target c++23 } }

#include <memory>
#include <testsuite_hooks.h>

std::unique_ptr<int> intptr;

void
val_inout(void** p)
{
  // The smart pointer should have been released by the inout_ptr_t constructor
  VERIFY( intptr == nullptr );
  // The initial value of the pointer should be non-null.
  VERIFY( *(int*)*p == 5678 );
  // Although the implementation is allowed to access the unique_ptr's
  // pointer directly, it can't do that here because the unique_ptr stores
  // an int* and we are reading from and writing to a void*.
  VERIFY( intptr.get() != *p );

  // Return a heap-allocated int in *p.
  *p = (void*) new int(999);
}

void
test_inout_ptr_void()
{
  intptr.reset(new int(5678));
  val_inout(std::inout_ptr<void*>(intptr));
  VERIFY( *intptr == 999 );
}

int main()
{
  test_inout_ptr_void();
}

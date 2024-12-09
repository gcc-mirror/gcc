// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

#include <memory>
#include <testsuite_tr1.h>

using namespace __gnu_test;

void
test01()
{
  std::shared_ptr<ClassType> ptr;
  std::shared_ptr<const ClassType> ptr2 = ptr;

#if __cpp_lib_shared_ptr_arrays >= 201611L
  std::shared_ptr<ClassType[10]> ptr_array;
  std::shared_ptr<ClassType[]> ptr_array2 = ptr_array;
  std::shared_ptr<ClassType const []> ptr_array3 = ptr_array;
#endif
}

void
test02()
{
  std::shared_ptr<IncompleteClass> ptr;
  std::shared_ptr<const IncompleteClass> ptr2 = ptr;

#if __cpp_lib_shared_ptr_arrays >= 201611L
  std::shared_ptr<IncompleteClass[10]> ptr_array;
  std::shared_ptr<IncompleteClass[]> ptr_array2 = ptr_array;
  std::shared_ptr<IncompleteClass const []> ptr_array3 = ptr_array;
#endif
}

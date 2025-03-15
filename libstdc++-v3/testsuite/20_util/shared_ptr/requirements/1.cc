// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

#include <memory>
#include <testsuite_tr1.h>
#include <type_traits>

using namespace __gnu_test;

template<template<class> class SmartPtr>
void
test01()
{
  SmartPtr<ClassType> ptr;
  SmartPtr<const ClassType> ptr2 = ptr;

#if __cpp_lib_shared_ptr_arrays >= 201611L
  SmartPtr<ClassType[10]> ptr_array;
  SmartPtr<ClassType[]> ptr_array2 = ptr_array;
  SmartPtr<ClassType const []> ptr_array3 = ptr_array;
#endif
}

template<template<class> class SmartPtr>
void
test02()
{
  SmartPtr<IncompleteClass> ptr;
  SmartPtr<const IncompleteClass> ptr2 = ptr;

#if __cpp_lib_shared_ptr_arrays >= 201611L
  SmartPtr<IncompleteClass[10]> ptr_array;
  SmartPtr<IncompleteClass[]> ptr_array2 = ptr_array;
  SmartPtr<IncompleteClass const []> ptr_array3 = ptr_array;
#endif
}

template<template<class> class SmartPtr>
void
test03()
{
  static_assert( std::is_convertible<SmartPtr<int>, SmartPtr<const int>>::value);
  static_assert(!std::is_convertible<SmartPtr<const int>, SmartPtr<int>>::value);
  static_assert( std::is_convertible<SmartPtr<ClassType>, SmartPtr<const ClassType>>::value);
  static_assert(!std::is_convertible<SmartPtr<const ClassType>, SmartPtr<ClassType>>::value);
  static_assert( std::is_convertible<SmartPtr<IncompleteClass>, SmartPtr<const IncompleteClass>>::value);
  static_assert(!std::is_convertible<SmartPtr<const IncompleteClass>, SmartPtr<IncompleteClass>>::value);
  static_assert( std::is_convertible<SmartPtr<void>, SmartPtr<const void>>::value);
  static_assert(!std::is_convertible<SmartPtr<const void>, SmartPtr<void>>::value);

  static_assert( std::is_convertible<SmartPtr<int>, SmartPtr<void>>::value);
  static_assert(!std::is_convertible<SmartPtr<void>, SmartPtr<int>>::value);
  static_assert( std::is_convertible<SmartPtr<int>, SmartPtr<const void>>::value);
  static_assert( std::is_convertible<SmartPtr<const int>, SmartPtr<const void>>::value);
  static_assert(!std::is_convertible<SmartPtr<const int>, SmartPtr<void>>::value);
  static_assert(!std::is_convertible<SmartPtr<const void>, SmartPtr<const int>>::value);
  static_assert( std::is_convertible<SmartPtr<ClassType>, SmartPtr<void>>::value);
  static_assert( std::is_convertible<SmartPtr<ClassType>, SmartPtr<const void>>::value);

  static_assert(!std::is_convertible<SmartPtr<int*>, SmartPtr<const int*>>::value);
  static_assert( std::is_convertible<SmartPtr<int*>, SmartPtr<const int* const>>::value);
  static_assert(!std::is_convertible<SmartPtr<const int*>, SmartPtr<int*>>::value);
  static_assert(!std::is_convertible<SmartPtr<const int* const>, SmartPtr<int*>>::value);

  static_assert(!std::is_convertible<SmartPtr<ClassType*>, SmartPtr<const ClassType*>>::value);
  static_assert( std::is_convertible<SmartPtr<ClassType*>, SmartPtr<const ClassType* const>>::value);
  static_assert(!std::is_convertible<SmartPtr<const ClassType*>, SmartPtr<ClassType*>>::value);
  static_assert(!std::is_convertible<SmartPtr<const ClassType* const>, SmartPtr<ClassType*>>::value);

  static_assert(!std::is_convertible<SmartPtr<void*>, SmartPtr<const void*>>::value);
  static_assert( std::is_convertible<SmartPtr<void*>, SmartPtr<const void* const>>::value);
  static_assert(!std::is_convertible<SmartPtr<const void*>, SmartPtr<void*>>::value);
  static_assert(!std::is_convertible<SmartPtr<const void* const>, SmartPtr<void*>>::value);

#if __cpp_lib_shared_ptr_arrays >= 201611L
  static_assert( std::is_convertible<SmartPtr<int[10]>, SmartPtr<int[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<int[]>, SmartPtr<int[10]>>::value);
  static_assert( std::is_convertible<SmartPtr<int[10]>, SmartPtr<int const[]>>::value);
  static_assert( std::is_convertible<SmartPtr<int[10]>, SmartPtr<int const[10]>>::value);
  static_assert( std::is_convertible<SmartPtr<int[]>, SmartPtr<int const[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<int const[]>, SmartPtr<int[]>>::value);
  static_assert( std::is_convertible<SmartPtr<int const[10]>, SmartPtr<int const[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<int const[]>, SmartPtr<int const[10]>>::value);

  static_assert( std::is_convertible<SmartPtr<ClassType[10]>, SmartPtr<ClassType[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<ClassType[]>, SmartPtr<ClassType[10]>>::value);
  static_assert( std::is_convertible<SmartPtr<ClassType[10]>, SmartPtr<ClassType const[]>>::value);
  static_assert( std::is_convertible<SmartPtr<ClassType[10]>, SmartPtr<ClassType const[10]>>::value);
  static_assert( std::is_convertible<SmartPtr<ClassType[]>, SmartPtr<ClassType const[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<ClassType const[]>, SmartPtr<ClassType[]>>::value);
  static_assert( std::is_convertible<SmartPtr<ClassType const[10]>, SmartPtr<ClassType const[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<ClassType const[]>, SmartPtr<ClassType const[10]>>::value);

  static_assert( std::is_convertible<SmartPtr<IncompleteClass[10]>, SmartPtr<IncompleteClass[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<IncompleteClass[]>, SmartPtr<IncompleteClass[10]>>::value);
  static_assert( std::is_convertible<SmartPtr<IncompleteClass[10]>, SmartPtr<IncompleteClass const[]>>::value);
  static_assert( std::is_convertible<SmartPtr<IncompleteClass[10]>, SmartPtr<IncompleteClass const[10]>>::value);
  static_assert( std::is_convertible<SmartPtr<IncompleteClass[]>, SmartPtr<IncompleteClass const[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<IncompleteClass const[]>, SmartPtr<IncompleteClass[]>>::value);
  static_assert( std::is_convertible<SmartPtr<IncompleteClass const[10]>, SmartPtr<IncompleteClass const[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<IncompleteClass const[]>, SmartPtr<IncompleteClass const[10]>>::value);

  static_assert( std::is_convertible<SmartPtr<int*[10]>, SmartPtr<int*[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<int*[]>, SmartPtr<int*[10]>>::value);
  static_assert( std::is_convertible<SmartPtr<int*[10]>, SmartPtr<int* const[10]>>::value);
  static_assert( std::is_convertible<SmartPtr<int*[10]>, SmartPtr<int* const[]>>::value);

  static_assert(!std::is_convertible<SmartPtr<int*[]>, SmartPtr<void*[]>>::value);
  static_assert(!std::is_convertible<SmartPtr<int*[]>, SmartPtr<void const *[]>>::value);
#endif
}

int
main()
{
  test01<std::shared_ptr>();
  test01<std::weak_ptr>();

  test02<std::shared_ptr>();
  test02<std::weak_ptr>();

  test03<std::shared_ptr>();
  test03<std::weak_ptr>();
}

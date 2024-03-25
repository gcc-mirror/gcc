// { dg-options "-fno-lifetime-dse -O0" }
// { dg-do run { target c++20 } }
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

// C++20 20.11.3.7 shared_ptr Creation [util.smartptr.shared.create]

#include <memory>

#ifndef __cpp_lib_smart_ptr_for_overwrite
# error "Feature-test macro for make_shared_for_overwrite missing in <memory>"
#elif __cpp_lib_smart_ptr_for_overwrite < 202002L
# error "Feature-test macro for make_shared_for_overwrite has wrong value in <memory>"
#endif

#include <cstring>
#include <testsuite_hooks.h>

int counter = 0;

template<typename T>
struct Alloc : std::allocator<T>
{
  Alloc() = default;

  template<typename U>
    Alloc(const Alloc<U>&) { }

  T* allocate(std::size_t n)
  {
    ++counter;
    void* p = std::allocator<T>::allocate(n);
    // need -fno-lifetime-dse to check for these values later.
    std::memset(p, 0xff, n * sizeof(T));
    return (T*)p;
  }

  void construct(auto*, auto&&...)
  {
    // The objects must be default-initialized, not using this function.
    VERIFY( ! "allocator_traits::construct" );
  }

  void destroy(auto*)
  {
    // The objects must be destroyed by ~T(), not using this function.
    VERIFY( ! "allocator_traits::destroy" );
  }
};

void
test01()
{
  Alloc<int> a;
  const int expected = 0xffffffff;

  std::shared_ptr<int> p1 = std::allocate_shared_for_overwrite<int>(a);
  VERIFY( counter == 1 );
  VERIFY( *p1 == expected );
  std::shared_ptr<int[44]> p2 = std::allocate_shared_for_overwrite<int[44]>(a);
  VERIFY( counter == 2 );
  VERIFY( p2[0] == expected );
  p2.reset();
  std::shared_ptr<int[]> p3 = std::allocate_shared_for_overwrite<int[]>(a, 88);
  VERIFY( counter == 3 );
  VERIFY( p3[0] == expected );
  VERIFY( p3[87] == expected );
  std::shared_ptr<int[3][4]> p4 = std::allocate_shared_for_overwrite<int[3][4]>(a);
  VERIFY( counter == 4 );
  VERIFY( p4[0][0] == expected );
  VERIFY( p4[2][3] == expected );
  std::shared_ptr<int[][5]> p5 = std::allocate_shared_for_overwrite<int[][5]>(a, 6);
  VERIFY( counter == 5 );
  VERIFY( p5[0][0] == expected );
  VERIFY( p5[5][4] == expected );

  struct BigBoi { int x[100]; };
  std::shared_ptr<BigBoi> p6 = std::allocate_shared_for_overwrite<BigBoi>(a);
  VERIFY( counter == 6 );
  VERIFY( p6->x[0] == expected );
  std::shared_ptr<BigBoi[22]> p7 = std::allocate_shared_for_overwrite<BigBoi[22]>(a);
  VERIFY( counter == 7 );
  VERIFY( p7[0].x[0] == expected );
  VERIFY( p7[21].x[99] == expected );
  std::shared_ptr<BigBoi[]> p8 = std::allocate_shared_for_overwrite<BigBoi[]>(a, 11);
  VERIFY( counter == 8 );
  VERIFY( p8[0].x[0] == expected );
  VERIFY( p8[10].x[10] == expected );
}

void
test02()
{
  // These aren't created by the custom allocator, so we can't check that the
  // memory was left uninitialized. Just dereference them.

  std::shared_ptr<int> p1 = std::make_shared_for_overwrite<int>();
  (void) *p1;
  std::shared_ptr<int[44]> p2 = std::make_shared_for_overwrite<int[44]>();
  (void) p2[0];
  std::shared_ptr<int[]> p3 = std::make_shared_for_overwrite<int[]>(88);
  (void) p3[0];
  (void) p3[87];
  std::shared_ptr<int[3][4]> p4 = std::make_shared_for_overwrite<int[3][4]>();
  (void) p4[0][0];
  (void) p4[2][3];
  std::shared_ptr<int[][5]> p5 = std::make_shared_for_overwrite<int[][5]>(6);
  (void) p5[0][0];
  (void) p5[5][4];

  struct BigBoi { int x[100]; };
  std::shared_ptr<BigBoi> p6 = std::make_shared_for_overwrite<BigBoi>();
  (void) p6->x[0];
  std::shared_ptr<BigBoi[22]> p7 = std::make_shared_for_overwrite<BigBoi[22]>();
  (void) p7[0].x[0];
  (void) p7[21].x[99];
  std::shared_ptr<BigBoi[]> p8 = std::make_shared_for_overwrite<BigBoi[]>(11);
  (void) p8[0].x[0];
  (void) p8[10].x[10];
}

void
test03()
{
  // Type with non-trivial initialization should still be default-initialized.
  struct NonTriv
  {
    int init = 0xbb;
    int uninit;
  };
  std::shared_ptr<NonTriv> a = std::make_shared_for_overwrite<NonTriv>();
  VERIFY( a->init == 0xbb );
  std::shared_ptr<NonTriv[]> b = std::make_shared_for_overwrite<NonTriv[2]>();
  VERIFY( b[1].init == 0xbb );
  std::shared_ptr<NonTriv[]> c = std::make_shared_for_overwrite<NonTriv[]>(2);
  VERIFY( c[1].init == 0xbb );
}

int
main()
{
  test01();
  test02();
  test03();
}

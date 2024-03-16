// { dg-do run { target c++20 } }
// { dg-add-options no_pch }

// C++20 20.11.3.7 shared_ptr Creation [util.smartptr.shared.create]

#include <memory>

#ifndef __cpp_lib_shared_ptr_arrays
# error "Feature-test macro for make_shared arrays missing in <memory>"
#elif __cpp_lib_shared_ptr_arrays < 201707L
# error "Feature-test macro for make_shared arrays has wrong value in <memory>"
#endif

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
    return std::allocator<T>::allocate(n);
  }
};

void
test01()
{
  Alloc<int> a;

  std::shared_ptr<int[]> p1 = std::allocate_shared<int[]>(a, 24);
  VERIFY( counter == 1 );
  VERIFY( p1[23] == 0 );
  std::shared_ptr<int[48]> p2 = std::allocate_shared<int[48]>(a);
  VERIFY( counter == 2 );
  VERIFY( p2[47] == 0 );

  std::shared_ptr<int[][12]> p3 = std::allocate_shared<int[][12]>(a, 3);
  VERIFY( counter == 3 );
  VERIFY( p3[2][11] == 0 );
  std::shared_ptr<int[4][5]> p4 = std::allocate_shared<int[4][5]>(a);
  VERIFY( counter == 4 );
  VERIFY( p4[3][4] == 0 );
}

void
test02()
{
  std::shared_ptr<int[]> p1 = std::make_shared<int[]>(24);
  VERIFY( p1[23] == 0 );
  std::shared_ptr<int[48]> p2 = std::make_shared<int[48]>();
  VERIFY( p2[47] == 0 );

  std::shared_ptr<int[][12]> p3 = std::make_shared<int[][12]>(3);
  VERIFY( p3[2][11] == 0 );
  std::shared_ptr<int[4][5]> p4 = std::make_shared<int[4][5]>();
  VERIFY( p4[3][4] == 0 );
}

#include <vector>
#include <cstdint>

std::vector<std::uintptr_t> addresses;

void
test03()
{
  // Verify construction and destruction order
  struct Addressed
  {
    Addressed() { addresses.push_back(me()); }
    ~Addressed() { VERIFY( addresses.back() == me() ); addresses.pop_back(); }

    std::uintptr_t me() const { return reinterpret_cast<std::uintptr_t>(this); }
  };

  auto check = [](auto shptr) {
    std::uintptr_t last = 0;
    for (auto a : addresses)
    {
      VERIFY( a > last );
      last = a;
    }
    shptr.reset();
    return addresses.empty();
  };

  VERIFY( check(std::make_shared<Addressed[][2]>(3)) );
  VERIFY( check(std::make_shared<Addressed[4][2]>()) );
}

void
test04()
{
  // Verify initial value
  auto p1 = std::make_shared<int[]>(3, 9);
  VERIFY( p1[0] == 9 && p1[1] == 9 && p1[2] == 9 );

  auto p2 = std::make_shared<int[2]>(4);
  VERIFY( p2[0] == 4 && p2[1] == 4 );

  auto p3 = std::make_shared<int[][3]>(10, {1,2,3});
  const auto& p3_0 = p3[0];
  VERIFY( p3_0[0] == 1 && p3_0[1] == 2 && p3_0[2] == 3 );
  for (int i = 1; i < 10; ++i)
    for (int j = 0; j < 3; ++j)
      VERIFY( p3[i][j] == p3_0[j] );

  auto p4 = std::make_shared<int[10][3]>({4,5,6});
  const auto& p4_0 = p4[0];
  VERIFY( p4_0[0] == 4 && p4_0[1] == 5 && p4_0[2] == 6 );
  for (int i = 1; i < 10; ++i)
    for (int j = 0; j < 3; ++j)
      VERIFY( p4[i][j] == p4_0[j] );

  auto p5 = std::make_shared<int[][3][2]>(10, {{1,2},{3,4},{5,6}});
  const auto& p5_0 = p5[0];
  VERIFY( p5_0[0][0] == 1 && p5_0[0][1] == 2 );
  VERIFY( p5_0[1][0] == 3 && p5_0[1][1] == 4 );
  VERIFY( p5_0[2][0] == 5 && p5_0[2][1] == 6 );
  for (int i = 1; i < 10; ++i)
    for (int j = 0; j < 3; ++j)
      for (int k = 0; k < 2; ++k)
	VERIFY( p5[i][j][k] == p5_0[j][k] );

  auto p6 = std::make_shared<int[4][3][2]>({{7,8},{9,10},{11,12}});
  const auto& p6_0 = p6[0];
  VERIFY( p6_0[0][0] ==  7 && p6_0[0][1] ==  8 );
  VERIFY( p6_0[1][0] ==  9 && p6_0[1][1] == 10 );
  VERIFY( p6_0[2][0] == 11 && p6_0[2][1] == 12 );
  for (int i = 1; i < 4; ++i)
    for (int j = 0; j < 3; ++j)
      for (int k = 0; k < 2; ++k)
	VERIFY( p6[i][j][k] == p6_0[j][k] );
}

void
test05()
{
  // Examples from the standard
  using namespace std;

  // Example 2
  {
    shared_ptr<double[]> p = make_shared<double[]>(1024);
    // shared_ptr to a value-initialized double[1024]
    for (int i = 0; i < 1024; ++i)
      VERIFY( p[i] == 0.0 );

    shared_ptr<double[][2][2]> q = make_shared<double[][2][2]>(6);
    // shared_ptr to a value-initialized double[6][2][2]
    for (int i = 0; i < 6; ++i)
      for (auto& j : q[i])
	for (auto& k : j)
	  VERIFY( k == 0.0 );
  }

  // Example 3
  {
    shared_ptr<double[1024]> p = make_shared<double[1024]>();
    // shared_ptr to a value-initialized double[1024]
    for (int i = 0; i < 1024; ++i)
      VERIFY( p[i] == 0.0 );

    shared_ptr<double[6][2][2]> q = make_shared<double[6][2][2]>();
    // shared_ptr to a value-initialized double[6][2][2]
    for (int i = 0; i < 6; ++i)
      for (auto& j : q[i])
	for (auto& k : j)
	  VERIFY( k == 0.0 );
  }

  // Example 4
  {
    shared_ptr<double[]> p = make_shared<double[]>(1024, 1.0);
    // shared_ptr to a double[1024], where each element is 1.0
    for (int i = 0; i < 1024; ++i)
      VERIFY( p[i] == 1.0 );

    shared_ptr<double[][2]> q = make_shared<double[][2]>(6, {1.0, 0.0});
    // shared_ptr to a double[6][2], where each double[2] element is {1.0, 0.0}
    for (int i = 0; i < 6; ++i)
      VERIFY( q[i][0] == 1.0 && q[i][1] == 0.0 );

    shared_ptr<vector<int>[]> r = make_shared<vector<int>[]>(4, {1, 2});
    // shared_ptr to a vector<int>[4], where each vector has contents {1, 2}
    for (int i = 0; i < 4; ++i)
      VERIFY( r[i] == vector<int>({1, 2}) );
  }

  // Example 5
  {
    shared_ptr<double[1024]> p = make_shared<double[1024]>(1.0);
    // shared_ptr to a double[1024], where each element is 1.0
    for (int i = 0; i < 1024; ++i)
      VERIFY( p[i] == 1.0 );

    shared_ptr<double[6][2]> q = make_shared<double[6][2]>({1.0, 0.0});
    // shared_ptr to a double[6][2], where each double[2] element is {1.0, 0.0}
    for (int i = 0; i < 6; ++i)
      VERIFY( q[i][0] == 1.0 && q[i][1] == 0.0 );

    shared_ptr<vector<int>[4]> r = make_shared<vector<int>[4]>({1, 2});
    // shared_ptr to a vector<int>[4], where each vector has contents {1, 2}
    for (int i = 0; i < 4; ++i)
      VERIFY( r[i] == vector<int>({1, 2}) );
  }
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
}

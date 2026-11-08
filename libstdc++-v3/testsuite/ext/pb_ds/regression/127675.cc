// { dg-options "-D_GLIBCXX_DEBUG" }
// { dg-do run }

#include <ext/pb_ds/assoc_container.hpp>
#include <ext/throw_allocator.h>

static bool is_odd(int i) { return i & 1; }

template<class T, class U, class, class>
struct Node_update
{
  typedef int metadata_type;
  void operator()(U, T) const { }
};

static void
test_erase_if_exception_safety()
{
  using namespace __gnu_pbds;
  typedef __gnu_cxx::throw_allocator_limit<int> Alloc;
  typedef tree<int, null_type, std::less<int>, ov_tree_tag, Node_update, Alloc> Set;

  for (int i = 0; i < 100; ++i)
  {
    Set s;
    s.insert(0);
    s.insert(1);
    s.insert(2);
    Alloc::limit_adjustor ladj(i);
    try
    {
      s.erase_if(is_odd); // throwing allocator triggered debug assertion
    }
    catch (const __gnu_cxx::forced_error&)
    {
    }
  }
}

static void
test_erase_last_element()
{
  using namespace __gnu_pbds;
  tree<int, int, std::less<int>, ov_tree_tag> m;
  m[1] = 1;
  m.erase(m.begin()); // erasing last element triggered debug assertion
}

int main()
{
  test_erase_if_exception_safety();
  test_erase_last_element();
}

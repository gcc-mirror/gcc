// { dg-do compile { target c++11 } }

#include <list>
#include <testsuite_allocator.h>

// An allocator that uses __gnu_cxx::_Pointer_adapter as its pointer type.
template class std::list<int, __gnu_test::CustomPointerAlloc<int>>;

// Unlike __gnu_cxx::_Pointer_adapter, this fancy pointer supports neither
// implicit nor explicit conversions from raw pointers. The constructor from
// a raw pointer is explicit and requires a second parameter. The only way for
// containers to construct one of these pointers is pointer_traits::pointer_to.
template<typename T>
struct Pointer : __gnu_test::PointerBase<Pointer<T>, T>
{
  using Base = __gnu_test::PointerBase<Pointer<T>, T>;

  Pointer() = default;
  Pointer(std::nullptr_t) : Base() { }
  explicit Pointer(T* p, int) : Base(p) { }

  // Allow conversions to const_pointer and to void_pointer
  template<typename U, typename = typename std::enable_if<
    (!std::is_const<U>::value && std::is_same<T, const U>::value)
    || (std::is_void<T>::value && std::is_convertible<U*, T*>::value)
    >::type>
    Pointer(const Pointer<U>& p) : Base(p.operator->()) { }

  template<typename U>
    static typename std::enable_if<std::is_same<U, T>::value, Pointer>::type
    pointer_to(U& t)
    { return Pointer(std::addressof(t), 1); }
};

// A minimal allocator that uses Pointer as its pointer type.
template<typename T>
struct Allocator
{
  using value_type = T;
  using pointer = Pointer<T>;

  Allocator() = default;
  template<typename U>
    Allocator(const Allocator<U>&) { }

  pointer allocate(std::size_t n)
  { return pointer(std::allocator<T>().allocate(n), 1); }

  void deallocate(pointer p, std::size_t n)
  {
    std::allocator<T>().deallocate(p.operator->(), n);
  }

  bool operator==(const Allocator&) const { return true; }
  bool operator!=(const Allocator&) const { return false; }
};

template class std::list<int, Allocator<int>>;

struct NonTrivial
{
  NonTrivial() { }
  NonTrivial(const NonTrivial&) { }
  ~NonTrivial() { }
  bool operator==(const NonTrivial&) const { return true; } // for remove(T)
  bool operator<(const NonTrivial&) const { return false; } // for sort()
};

template class std::list<NonTrivial, Allocator<NonTrivial>>;

#include <testsuite_iterators.h>

void
test_template_members(__gnu_test::input_container<short>& c)
{
  // Use member functions that are not included in explicit instantiations.
  std::list<int, Allocator<int>> l(c.begin(), c.end());
  l.assign(c.begin(), c.end());
  l.insert(l.begin(), c.begin(), c.end());
  l.emplace_front(1);
  l.emplace_back(1);
  l.emplace(l.begin(), 1);
  l.remove_if([](int) { return false; });
  l.unique([](int, int) { return false; });
  l.merge(l, [](int, int) { return false; });
  l.merge(std::move(l), [](int, int) { return false; });
  l.sort([](int, int) { return false; });

#ifdef __cpp_lib_ranges_to_container
  short arr[2];
  __gnu_test::test_input_range<short> r(arr);
  std::list<int, Allocator<int>> l2(std::from_range, r);
  l2.assign_range(r);
  l2.prepend_range(r);
  l2.append_range(r);
  l2.insert_range(l2.begin(), r);
#endif
}

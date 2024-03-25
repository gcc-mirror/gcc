// { dg-do compile { target c++20 } }

// PR libstdc++/112607
// _Normalize does not consider char_type for the basic_string_view case

#include <format>

template<typename T>
struct Alloc
{
  using value_type = T;
  Alloc() = default;
  template<typename U>
    Alloc(const Alloc<U>&) { }
  T* allocate(std::size_t);
  void deallocate(T*, std::size_t);
  bool operator==(const Alloc&) const;
};

template<typename C>
using String = std::basic_string<C, std::char_traits<C>, Alloc<C>>;

template<>
struct std::formatter<String<wchar_t>> : std::formatter<std::string> {
  auto format(const String<wchar_t>&, auto& ctx) const {
    return std::formatter<std::string>::format(" ", ctx);
  }
};

std::string str = std::format("{}", String<wchar_t>{});

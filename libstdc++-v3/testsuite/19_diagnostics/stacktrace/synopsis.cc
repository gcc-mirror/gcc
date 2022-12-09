// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }
// { dg-require-effective-target stacktrace }

#include <stacktrace>

#ifndef __cpp_lib_stacktrace
# error "Feature-test macro for stacktrace missing in <stacktrace>"
#elif __cpp_lib_stacktrace < 202011L
# error "Feature-test macro for stacktrace has wrong value in <stacktrace>"
#endif

namespace std
{
  class stacktrace_entry;

  template<class Allocator>
    class basic_stacktrace;

  using stacktrace = basic_stacktrace<allocator<stacktrace_entry>>;

  template<class Allocator>
    void swap(basic_stacktrace<Allocator>& a, basic_stacktrace<Allocator>& b)
    noexcept(noexcept(a.swap(b)));

  string to_string(const stacktrace_entry& f);

  template<class Allocator>
    string to_string(const basic_stacktrace<Allocator>& st);

  ostream&
  operator<<(ostream& os, const stacktrace_entry& f);

  template<class Allocator>
    ostream&
    operator<<(ostream& os, const basic_stacktrace<Allocator>& st);

  namespace pmr {
    using stacktrace = basic_stacktrace<polymorphic_allocator<stacktrace_entry>>;
  }

  template<class T> struct hash;
  template<> struct hash<stacktrace_entry>;
  template<class Allocator> struct hash<basic_stacktrace<Allocator>>;
}

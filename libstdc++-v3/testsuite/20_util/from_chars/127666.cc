// { dg-do run { target c++17 } }
// { dg-require-effective-target mmap }
// { dg-require-effective-target sysconf }
// { dg-add-options ieee }
// { dg-additional-options "-DSKIP_LONG_DOUBLE" { target aarch64-*-rtems* aarch64-*-vxworks* x86_64-*-vxworks* } }

// Bug 127666
// std::from_chars(first, last, long double&) reads past 'last' for "inf"/"nan"

#include <charconv>
#include <string_view>
#include <cstring>
#include <sys/mman.h>
#include <unistd.h>
#include <testsuite_hooks.h>

using namespace std::string_view_literals;

// One past the last writable byte; the guard page starts here, so reading
// at or beyond this address faults.
static char* buf_end;

template<typename T>
void
check(std::string_view input, std::errc ec, std::size_t len)
{
  // Position the input against the guard page so an over-read faults.
  char* const first = buf_end - input.size();
  std::memcpy(first, input.data(), input.size());
  T value = 1;
  auto res = std::from_chars(first, buf_end, value);
  VERIFY( res.ec == ec );
  VERIFY( res.ptr == first + len );
}

template<typename T>
void
test_type()
{
  // The inf/nan forms used to read past the end via strlen.
  check<T>("inf"sv, std::errc{}, 3);
  check<T>("infinity"sv, std::errc{}, 8);
  check<T>("nan"sv, std::errc{}, 3);
  check<T>("nan(1)"sv, std::errc{}, 6);
  check<T>("-nan(ab)"sv, std::errc{}, 8);

  // A lone sign used to read one byte past the end.
  check<T>("-"sv, std::errc::invalid_argument, 0);
  check<T>("+"sv, std::errc::invalid_argument, 0);
}

int
main()
{
  const long pg = ::sysconf(_SC_PAGESIZE);
  VERIFY( pg > 0 );
  char* const base = (char*) ::mmap(nullptr, 2 * pg, PROT_READ | PROT_WRITE,
				    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  VERIFY( base != MAP_FAILED );
  VERIFY( ::mprotect(base + pg, pg, PROT_NONE) == 0 );
  buf_end = base + pg;

  test_type<float>();
  test_type<double>();
#ifndef SKIP_LONG_DOUBLE
  test_type<long double>();
#endif

  ::munmap(base, 2 * pg);
}

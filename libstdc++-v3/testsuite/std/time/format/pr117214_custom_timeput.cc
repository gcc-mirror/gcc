// { dg-do run { target c++20 } }

#include <chrono>
#include <format>
#include <locale>
#include <testsuite_hooks.h>

struct custom_time_put : std::time_put<char>
{
  iter_type
  do_put(iter_type out, std::ios_base& io, char_type fill, const tm* t,
	 char format, char modifier) const override
  {
    using Base = std::time_put<char>;

    switch (format) {
	case 'a': case 'A': case 'b': case 'h': case 'B': case 'p':
	*out++ = '[';
	*out++ = format;
	*out++ = ']';
    }
    return Base::do_put(out, io, fill, t, format, modifier);
  }
};

int main()
{
  using namespace std::chrono;
  std::locale loc(std::locale::classic(), new custom_time_put);
#define test(t, fmt, exp) VERIFY( std::format(loc, fmt, t) == exp )
  test(Monday,  "{:L%a}", "[a]Mon");
  test(Monday,  "{:L%A}", "[A]Monday");
  test(January, "{:L%b}", "[b]Jan");
  test(January, "{:L%h}", "[h]Jan");
  test(January, "{:L%B}", "[B]January");
  test(1h,      "{:L%p}", "[p]AM");
}

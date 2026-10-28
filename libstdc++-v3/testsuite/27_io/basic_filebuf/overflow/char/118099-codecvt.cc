// { dg-do run }
// { dg-require-fileio "" }

// PR libstdc++/118099
// basic_filebuf::overflow leaves streambuf in inconsistent state on I/O error
//
// Companion test of 118099.cc testing the throwing failure path in overflow:
// a codecvt whose do_out always returns error makes _M_convert_to_external
// throw after the overflow character has been written to the end of put area.
// If the put pointer is advanced before the throw (and only rewound on the
// non-throwing error path), a caller that catches the exception and keeps
// writing will cause overflow(c) to write past the end of the managed buffer,
// just like the original PR libstdc++/118099 testcase.

#include <fstream>
#include <locale>
#include <testsuite_hooks.h>

typedef char C;

struct failing_codecvt : std::codecvt<C, char, std::mbstate_t>
{
protected:
  result
  do_out(state_type&, const intern_type* from, const intern_type*,
	 const intern_type*& from_next,
	 extern_type* to, extern_type*, extern_type*& to_next) const
  {
    from_next = from;
    to_next = to;
    return error;
  }

  bool do_always_noconv() const throw() { return false; }
};

static void
test_pr118099_codecvt()
{
  const C val = C('x');
  const C canary = ~val;
  const int N = 8;
  C buf[N * 2];
  for (int i = 0; i < N * 2; ++i)
    buf[i] = canary;

  std::basic_filebuf<C> fb;
  fb.pubimbue(std::locale(std::locale::classic(), new failing_codecvt));
  fb.pubsetbuf(buf, N); // filebuf can use buf[0..N]
  fb.open("tmp_118099_codecvt", std::ios_base::out | std::ios_base::trunc);
  VERIFY( fb.is_open() );

  // Libstdc++ filebuf throws ios::failure for the codecvt error,
  // but the standard allows overflow to return eof instead.
  // To make this test portable, we check for either kind of error.
  bool got_error = false;
  for (int i = 0; i < N * 2; ++i)
    try {
      std::char_traits<C>::int_type r = fb.sputc(val);
      if (std::char_traits<C>::eq_int_type(r, std::char_traits<C>::eof()))
	got_error = true;
    } catch (const std::exception&) {
      got_error = true;
    }
  VERIFY( got_error ); // a codecvt error actually happened

  for (int i = 0; i < N; ++i)
    VERIFY( buf[i] == val );
  for (int i = N; i < N * 2; ++i)
    VERIFY( buf[i] == canary ); // nothing was written past buf[0..N]
}

int main()
{
  test_pr118099_codecvt();
}

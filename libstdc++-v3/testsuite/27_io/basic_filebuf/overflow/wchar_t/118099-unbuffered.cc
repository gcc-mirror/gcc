// { dg-do run }
// { dg-require-fileio "" }

// PR libstdc++/118099
// basic_filebuf::overflow leaves streambuf in inconsistent state on I/O error
//
// Companion test of 118099.cc testing the unbuffered put path, which calls
// _M_convert_to_external(&__conv, 1).  When a codecvt forces the
// "partial conversion" retry, that retry must compute the amount of input
// left to convert from the buffer it was handed (__ibuf + __ilen), not from
// pptr().  In unbuffered mode pptr() is null, so the old code called
// codecvt::out with from_end == __iresume + (pptr() - __iend) == pptr(),
// i.e. a null `from_end` for a non-null `from`.

#include <fstream>
#include <locale>
#include <testsuite_hooks.h>

typedef wchar_t C;

// Emits one external char per do_out() call without consuming input, and
// under-reports max_length(), so that _M_convert_to_external is forced to
// take its partial-conversion retry branch.  The retry is where the bogus
// (null) from_end is passed.
// This is probably not a conforming codecvt facet and is intended only to
// test the libstdc++ basic_filebuf::_M_convert_to_external behaviour.
struct retry_forcing_codecvt : std::codecvt<C, char, std::mbstate_t>
{
protected:
  result
  do_out(state_type&, const intern_type* from, const intern_type* from_end,
	 const intern_type*& from_next, extern_type* to, extern_type* to_end,
	 extern_type*& to_next) const
  {
    // The _M_convert_to_external bug passed non-null from and null from_end.
    if (from != 0)
      VERIFY( from_end != 0 );

    from_next = from;
    to_next = to;

    if (from == from_end)
      return ok;

    if (to != to_end)
      *to_next++ = *from; // emit a character; do not advance from_next

    return partial;
  }

  bool do_always_noconv() const throw() { return false; }
  int  do_max_length()    const throw() { return 1; }
};

static void
test_pr118099_unbuffered()
{
  std::basic_filebuf<C> fb;
  fb.pubimbue(std::locale(std::locale::classic(), new retry_forcing_codecvt));
  fb.pubsetbuf(0, 0); // unbuffered: pptr() == epptr() == null
  fb.open("tmp_118099_unbuffered", std::ios_base::out | std::ios_base::trunc);
  VERIFY( fb.is_open() );

  // Each put goes straight to _M_convert_to_external(&__conv, 1); the codecvt
  // above asserts it is never called with the bogus null from_end.
  fb.sputc(L'a');
  fb.sputc(L'b');
  fb.sputc(L'c');
}

int main()
{
  test_pr118099_unbuffered();
}

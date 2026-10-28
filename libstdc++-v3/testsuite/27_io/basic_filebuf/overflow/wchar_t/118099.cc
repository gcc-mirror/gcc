// { dg-do run }
// { dg-require-fileio "" }

// PR libstdc++/118099
// basic_filebuf::overflow leaves streambuf in inconsistent state on I/O error

#include <fstream>
#include <testsuite_hooks.h>

static void
test_pr118099()
{
  typedef wchar_t C;

  // Only run if /dev/full already exists as a device: probe it with a
  // read-only open first, so we never accidentally create a regular file
  // named /dev/full on a hypothetical system with a writable /dev.
  {
    std::filebuf probe;
    if (!probe.open("/dev/full", std::ios_base::in))
      return; // no /dev/full here: nothing to test.
  }

  const C val = C('x');
  const C canary = ~val;
  const int N = 8;
  C buf[N * 2] = { };
  for (int i = 0; i < N * 2; ++i)
    buf[i] = canary;

  std::basic_filebuf<C> fb;
  fb.pubsetbuf(buf, N); // filebuf can use buf[0..N]
  fb.open("/dev/full", std::ios_base::out);
  VERIFY( fb.is_open() );

  int i = 0;
  // Keep writing until overflow happens (and fails):
  while (fb.sputc(val) != std::char_traits<C>::eof())
    ++i;
  // There should have been N-1 successful calls before overflow failed:
  VERIFY( i == (N - 1) );
  // Cause some more calls to overflow, which should also fail:
  VERIFY( fb.sputc(val) == std::char_traits<C>::eof() );
  VERIFY( fb.sputc(val) == std::char_traits<C>::eof() );
  VERIFY( fb.sputc(val) == std::char_traits<C>::eof() );

  // Verify that only the first N positions in the buffer were changed:
  for (i = 0; i < N; ++i)
    VERIFY( buf[i] == val );
  for (; i < N * 2; ++i)
    VERIFY( buf[i] == canary );
}

int main()
{
  test_pr118099();
}

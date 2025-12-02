// { dg-do run }

// Bug libstdc++/122907
// std::copy incorrectly uses memcpy when copying from signed or unsigned char
// buffer to bool buffer

#include <algorithm>
#include <testsuite_hooks.h>

template<typename T>
__attribute__((noinline,noipa))
void
test_pr122907(T (&buf)[4])
{
  unsigned char uc[4];
  bool bool_buf[4];
  std::copy(buf, buf+1, bool_buf);
  std::copy(bool_buf, bool_buf+1, uc);
  VERIFY(uc[0] == bool(buf[0]));
  std::copy(buf, buf+4, bool_buf);
  std::copy(bool_buf, bool_buf+4, uc);
  VERIFY(uc[0] == bool(buf[0]));
  VERIFY(uc[1] == bool(buf[1]));
  VERIFY(uc[2] == bool(buf[2]));
  VERIFY(uc[3] == bool(buf[3]));
}

template<typename T>
void
test_pr122907()
{
  T buf[4] = { (T)3,  (T)2, (T)1, (T)0 };
  test_pr122907(buf);
}

int main()
{
  test_pr122907<char>();
  test_pr122907<signed char>();
  test_pr122907<unsigned char>();
  test_pr122907<bool>();
  test_pr122907<int>();
}

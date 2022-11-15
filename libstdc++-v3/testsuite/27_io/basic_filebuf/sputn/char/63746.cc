// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

class testbuf : public std::filebuf {
public:
  char_type* pub_pprt() const
  {
    return this->pptr();
  }

  char_type* pub_pbase() const
  {
    return this->pbase();
  }
};

void test01()
{
  using namespace std;

  // Leave capacity to avoid flush.
  const streamsize chunk_size = BUFSIZ - 1 - 1;
  const char data[chunk_size] = {};

  testbuf a_f;
  VERIFY( a_f.open("tmp_63746_sputn", ios_base::out) );
  VERIFY( chunk_size == a_f.sputn(data, chunk_size) );
  VERIFY( (a_f.pub_pprt() - a_f.pub_pbase()) == chunk_size );
  VERIFY( a_f.close() );
}

int main()
{
  test01();
  return 0;
}

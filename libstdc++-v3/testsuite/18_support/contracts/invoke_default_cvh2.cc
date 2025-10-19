// check that default contract violation is not invoked if not explicitly invoked
// { dg-options "-fcontracts -fcontract-evaluation-semantic=observe" }
// { dg-do run { target c++26 } }

#include <contracts>
#include <testsuite_hooks.h>
#include <iostream>
#include <sstream>


struct checking_buf
  : public std::streambuf
{
  bool written = false;

  checking_buf() = default;

  virtual int_type
  overflow(int_type)
  {
    written = true;
    return int_type();
  }

  std::streamsize xsputn(const char* s, std::streamsize count)
  {
    written = true;
    return count;
  }

};


bool custom_called = false;


void handle_contract_violation(const std::contracts::contract_violation& v)
{
  custom_called = true;
}

void f(int i) pre (i>10) {};

int main()
{
  auto save_buf = std::cerr.rdbuf();
  checking_buf buf;
  std::cerr.rdbuf(&buf);

  f(0);
  std::cerr.rdbuf(save_buf);
  VERIFY(!buf.written);
  return 0;
}


// { dg-do run { target c++23 } }

#include <spanstream>
#include <testsuite_hooks.h>

using std::ispanstream;
using std::ospanstream;
using std::span;

void
test_move()
{
  char c;
  {
    const char str[] = "chars";
    std::ispanstream a(str);
    std::ispanstream b = std::move(a);
    VERIFY( b.span().data() == str && b.span().size() == 6 );
    VERIFY( b >> c );
    VERIFY( c == 'c' );

    a = std::move(b);
    VERIFY( a.span().data() == str && a.span().size() == 6 );
    VERIFY( a >> c >> c );
    VERIFY( c == 'a' );
  }

  {
    char buf[10] = {};
    std::ospanstream a(buf);
    std::ospanstream b = std::move(a);
    VERIFY( b << 'c' );
    VERIFY( buf[0] == 'c' );
    VERIFY( !std::char_traits<char>::compare(buf, "c", 2) );

    a = std::move(b);
    VERIFY( a << 'h' << 'a' << "rs" );
    VERIFY( !std::char_traits<char>::compare(buf, "chars", 6) );
  }

  {
    char buf[10] = {};
    std::spanstream a(buf);
    std::spanstream b = std::move(a);
    VERIFY( b.span().empty() );
    VERIFY( b << 'c' );
    VERIFY( buf[0] == 'c' );
    VERIFY( !std::char_traits<char>::compare(buf, "c", 2) );
    VERIFY( b.span().data() == buf && b.span().size() == 1 );
    VERIFY( b >> c );
    VERIFY( c == 'c' );

    a = std::move(b);
    VERIFY( a.span().data() == buf && a.span().size() == 1 );
    VERIFY( a << 'h' << 'a' << "rs" );
    VERIFY( !std::char_traits<char>::compare(buf, "chars", 6) );
    VERIFY( a.span().data() == buf && a.span().size() == 5 );
  }
}

void
test_swap()
{
  {
    const char str1[] = "chars";
    const char str2[] = "STRING";
    std::ispanstream a(str1);
    std::ispanstream b(str2);
    a.swap(b);
    VERIFY( a.span().data() == str2 && a.span().size() == 7 );
    VERIFY( b.span().data() == str1 && b.span().size() == 6 );
    char c;
    VERIFY( a >> c );
    VERIFY( c == 'S' );
    VERIFY( b >> c );
    VERIFY( c == 'c' );

    swap(a, b);
    VERIFY( a.span().data() == str1 && a.span().size() == 6 );
    VERIFY( b.span().data() == str2 && b.span().size() == 7 );
    VERIFY( a >> c >> c );
    VERIFY( c == 'a' );
    VERIFY( b >> c >> c );
    VERIFY( c == 'R' );
  }

  {
    char buf1[] = "xxxxxxxxxxxxxxx";
    char buf2[] = "xxxxxxxxxxxxxxx";
    std::ospanstream a(buf1);
    std::ospanstream b(buf2);
    a.swap(b);
    VERIFY( a << "STR" );
    VERIFY( !std::char_traits<char>::compare(buf2, "STRx", 4) );
    VERIFY( b << 'c' << 'h' );
    VERIFY( !std::char_traits<char>::compare(buf1, "chx", 3) );

    swap(a, b);
    VERIFY( a.span().size() == 2 );
    VERIFY( b.span().size() == 3 );
    VERIFY( a << 'a' << "rs" );
    VERIFY( !std::char_traits<char>::compare(buf1, "charsx", 6) );
    VERIFY( b << "IN" << 'G' );
    VERIFY( !std::char_traits<char>::compare(buf2, "STRINGx", 7) );
  }
}

int main()
{
  test_move();
  test_swap();
}

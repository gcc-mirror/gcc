#include <string>
#include <tuple>
#include <testsuite_hooks.h>

static std::string result;

struct X {
  int state; // this has to be here
  X() {
    result += "Def";
  }

  X(X const&) {
    result += "Copy";
  }

  X(X&&) {
    result += "Move";
  }

  ~X() {
    result += "Dtor";
  }
};

void f()
{
  X v;
  std::tuple<X> t1{v};
  std::tuple<std::tuple<X>&&> t2{std::move(t1)};
  std::tuple<std::tuple<X>> t3{std::move(t2)};
}

void f2()
{
  X v;
  std::tuple<X> t1{std::move(v)};
  std::tuple<std::tuple<X>&&> t2{std::move(t1)};
  std::tuple<std::tuple<X>> t3{std::move(t2)};
}

void f3()
{
  std::tuple<X> t1{X{}};
  std::tuple<std::tuple<X>&&> t2{std::move(t1)};
  std::tuple<std::tuple<X>> t3{std::move(t2)};
}

int main()
{
  f();
  VERIFY(result == "DefCopyMoveDtorDtorDtor");
  result = "";
  f2();
  VERIFY(result == "DefMoveMoveDtorDtorDtor");
  result = "";
  f3();
  VERIFY(result == "DefMoveDtorMoveDtorDtor");
  result = "";
}

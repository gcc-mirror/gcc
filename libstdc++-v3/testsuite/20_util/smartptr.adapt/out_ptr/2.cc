// { dg-do compile { target c++23 } }

#include <memory>
#include <testsuite_hooks.h>

int counter = 0;

void
test_unique_ptr()
{
  std::unique_ptr<int> up(new int(1));
  {
    auto op = std::out_ptr(up);
    VERIFY( up == nullptr );
    int** p = op;
    VERIFY( *p == nullptr );
    *p = new int(2);

    const auto& cop = op;
    VERIFY( static_cast<int**>(cop) == static_cast<int**>(op) );
  }
  VERIFY( *up == 2 );

  {
    std::default_delete<int> d;
    auto op = std::out_ptr(up, d);
    VERIFY( up == nullptr );
    int** p = op;
    VERIFY( *p == nullptr );
    *p = new int(3);

    const auto& cop = op;
    VERIFY( static_cast<int**>(cop) == static_cast<int**>(op) );
  }
  VERIFY( *up == 3 );

  struct D
  {
    explicit D(int id) : id(id) { }
    void operator()(long* p) const { ++counter; delete p; }
    int id;
  };
  counter = 0;
  std::unique_ptr<long, D> upd(new long(1), D(11));
  {
    auto op = std::out_ptr(upd);
    VERIFY( counter == 1 );
    VERIFY( upd == nullptr );
    long** p = op;
    VERIFY( *p == nullptr );
    *p = new long(4);

    const auto& cop = op;
    VERIFY( static_cast<long**>(cop) == static_cast<long**>(op) );
  }
  VERIFY( *upd == 4 );
  VERIFY( upd.get_deleter().id == 11 );
  VERIFY( counter == 1 );

  {
    D d(33);
    auto op = std::out_ptr(upd, d);
    VERIFY( counter == 2 );
    VERIFY( upd == nullptr );
    long** p = op;
    VERIFY( *p == nullptr );

    const auto& cop = op;
    VERIFY( static_cast<long**>(cop) == static_cast<long**>(op) );
  }
  VERIFY( upd == nullptr );
  VERIFY( upd.get_deleter().id == 11 ); // Deleter not replaced if p is null.
  VERIFY( counter == 2 );

  {
    D d(33);
    auto op = std::out_ptr(upd, d);
    VERIFY( counter == 2 );
    VERIFY( upd == nullptr );
    long** p = op;
    VERIFY( *p == nullptr );
    *p = new long(5);
  }
  VERIFY( *upd == 5 );
  VERIFY( upd.get_deleter().id == 33 );
  VERIFY( counter == 2 );

  struct Base { };
  struct Derived : Base
  {
    Derived(int id) : id(id) { }
    int id;
  };
  std::unique_ptr<Derived> upbd(new Derived(1));
  {
    auto op = std::out_ptr<Base*>(upbd);
    Base** bpp = op;
    VERIFY( *bpp == nullptr );
    *bpp = new Derived(2);

    const auto& cop = op;
    VERIFY( static_cast<Base**>(cop) == static_cast<Base**>(op) );
  }
  VERIFY( upbd->id == 2 );
}

void deleter_function(float* p) { delete p; }

void
test_shared_ptr()
{
  using DD = std::default_delete<int>;

  std::shared_ptr<int> sp(new int(1));
  {
    auto op = std::out_ptr(sp, DD{});
    VERIFY( sp == nullptr );
    int** p = op;
    VERIFY( *p == nullptr );
    *p = new int(2);

    const auto& cop = op;
    VERIFY( static_cast<int**>(cop) == static_cast<int**>(op) );
  }
  VERIFY( *sp == 2 );

  {
    auto op = std::out_ptr(sp, DD{});
    VERIFY( sp == nullptr );
    int** p = op;
    VERIFY( *p == nullptr );
    *p = new int(3);
  }
  VERIFY( *sp == 3 );

  struct D
  {
    explicit D(int id) : id(id) { }
    void operator()(long* p) const { ++counter; delete p; }
    int id;
  };
  counter = 0;
  std::shared_ptr<long> spd(new long(1), D(11));
  {
    auto op = std::out_ptr(spd, D(22));
    VERIFY( counter == 1 );
    VERIFY( spd == nullptr );
    long** p = op;
    VERIFY( *p == nullptr );
    *p = new long(5);
  }
  VERIFY( counter == 1 );
  VERIFY( *spd == 5 );
  VERIFY( std::get_deleter<D>(spd)->id == 22 );

  struct Base { };
  struct Derived : Base
  {
    Derived(int id) : id(id) { }
    int id;
  };
  std::shared_ptr<Derived> spbd(new Derived(1));
  {
    auto op = std::out_ptr<Base*>(spbd, std::default_delete<Derived>());
    Base** bpp = op;
    VERIFY( *bpp == nullptr );
    *bpp = new Derived(2);
  }
  VERIFY( spbd->id == 2 );

  std::shared_ptr<float> spf;
  {
    auto op = std::out_ptr(spf, deleter_function);
    float** fpp = op;
    *fpp = new float(0.5);
  }
  VERIFY( std::get_deleter<void(*)(float*)>(spf) != nullptr );
  VERIFY( *std::get_deleter<void(*)(float*)>(spf) == &deleter_function );
  VERIFY( *spf == 0.5 );
}

void
test_custom_ptr()
{
  struct UPtr : std::unique_ptr<int>
  {
    using std::unique_ptr<int>::unique_ptr;
  };

  UPtr up(new int(1));
  {
    auto op = std::out_ptr(up);
    VERIFY( up == nullptr );
    int** p = op;
    VERIFY( *p == nullptr );
    *p = new int(2);

    const auto& cop = op;
    VERIFY( static_cast<int**>(cop) == static_cast<int**>(op) );
  }
  VERIFY( *up == 2 );

  {
    auto op = std::out_ptr(up, std::default_delete<int>{});
    VERIFY( up == nullptr );
    int** p = op;
    VERIFY( *p == nullptr );
    *p = new int(3);
  }
  VERIFY( *up == 3 );

  struct D
  {
    explicit D(int id) : id(id) { }
    void operator()(long* p) const { ++counter; delete p; }
    int id;
  };
  counter = 0;
  struct UDPtr : std::unique_ptr<long, D>
  {
    using std::unique_ptr<long, D>::unique_ptr;
  };
  UDPtr upd(new long(1), D(11));
  {
    auto op = std::out_ptr(upd);
    VERIFY( counter == 1 );
    VERIFY( upd == nullptr );
    long** p = op;
    VERIFY( *p == nullptr );
    *p = new long(4);
  }
  VERIFY( counter == 1 );
  VERIFY( *upd == 4 );
  VERIFY( upd.get_deleter().id == 11 );

  {
    auto op = std::out_ptr(upd, D(22));
    VERIFY( upd == nullptr );
    VERIFY( counter == 2 );
    long** p = op;
    VERIFY( *p == nullptr );
    *p = new long(5);
  }
  VERIFY( counter == 2 );
  VERIFY( *upd == 5 );
  VERIFY( upd.get_deleter().id == 22 );
}

void
test_raw_ptr()
{
  long l = 5, l2 = 6;
  long* lp = &l;
  {
    auto op = std::out_ptr(lp);
    VERIFY( lp == nullptr );
    long** p = op;
    VERIFY( *p == nullptr );
    *p = &l2;

    const auto& cop = op;
    VERIFY( static_cast<long**>(cop) == static_cast<long**>(op) );
  }
  VERIFY( *lp == 6 );
}

int main()
{
  test_unique_ptr();
  test_shared_ptr();
  test_custom_ptr();
  test_raw_ptr();
}

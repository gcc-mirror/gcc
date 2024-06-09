// { dg-do run { target c++23 } }

#include <memory>
#include <testsuite_hooks.h>

// C++23 [inout.ptr.t] Class template inout_ptr_t

int counter = 0;

void
test_unique_ptr()
{
  int* ip = new int(1);
  std::unique_ptr<int> up(ip);
  {
    auto iop = std::inout_ptr(up);
    int** ipp = iop;
    VERIFY( *ipp == ip );
    delete *ipp;
    *ipp = new int(2);
  }
  VERIFY( *up == 2 );

  ip = up.get();
  {
    std::default_delete<int> d;
    auto iop = std::inout_ptr(up, d);
    int** ipp = iop;
    VERIFY( *ipp == ip );
    delete *ipp;
    *ipp = new int(3);
  }
  VERIFY( *up == 3 );

  struct D
  {
    explicit D(int id) : id(id) { }
    void operator()(long* p) const { ++counter; delete p; }
    int id;
  };
  counter = 0;
  std::unique_ptr<long, D> upd(new long(3), D(11));
  {
    auto iop = std::inout_ptr(upd);
    VERIFY( counter == 0 );
    long** lpp = iop;
    VERIFY( **lpp == 3 );
    delete *lpp;
    *lpp = new long(4);
  }
  VERIFY( *upd == 4 );
  VERIFY( upd.get_deleter().id == 11 );
  VERIFY( counter == 0 );

  {
    D d(22);
    auto iop = std::inout_ptr(upd, d);
    VERIFY( counter == 0 );
    long** lpp = iop;
    VERIFY( **lpp == 4 );
    delete *lpp;
    *lpp = nullptr;
  }
  VERIFY( upd == nullptr );
  VERIFY( upd.get_deleter().id == 11 ); // Deleter not replaced if p is null.
  VERIFY( counter == 0 );

  upd.reset(new long(5));
  {
    D d(33);
    auto iop = std::inout_ptr(upd, d);
    VERIFY( counter == 0 );
    long** lpp = iop;
    VERIFY( **lpp == 5 );
    delete *lpp;
    *lpp = new long(6);
  }
  VERIFY( *upd == 6 );
  VERIFY( upd.get_deleter().id == 33 );
  VERIFY( counter == 0 );

  struct Base { };
  struct Derived : Base
  {
    Derived(int id) : id(id) { }
    int id;
  };
  std::unique_ptr<Derived> upbd(new Derived(1));
  {
    auto iop = std::inout_ptr<Base*>(upbd);
    Base** bpp = iop;
    VERIFY( static_cast<Derived*>(*bpp)->id == 1 );
    delete (Derived*)*bpp;
    *bpp = new Derived(2);
  }
  VERIFY( upbd->id == 2 );
}

void
test_lwg3897()
{
  // Verify that implementation handles LWG Issue 3897
  auto nuller = [](int** p) {
    delete *p;
    *p = nullptr;
  };
  int* i = new int{5};
  nuller(std::inout_ptr(i));

  VERIFY( i == nullptr );
}

int main()
{
  test_unique_ptr();
  test_lwg3897();
}

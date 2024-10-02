// { dg-do run { target c++11 } }

#include <memory>
#include <testsuite_hooks.h>

struct BaseBase { virtual ~BaseBase() = default; };
struct Base : BaseBase { virtual ~Base() = default; };
struct Derived1 : Base { virtual ~Derived1() = default; };
struct Derived2 : virtual Base { virtual ~Derived2() = default; };
struct Derived3 : virtual Base { virtual ~Derived3() = default; };
struct Derived4 : Derived2, Derived3 { virtual ~Derived4() = default; };
struct Derived5 : Derived4 { virtual ~Derived5() = default; };

template<typename T>
void test01()
{
  std::shared_ptr<T> ptr(new T);
  VERIFY(ptr);

  std::weak_ptr<T> wptr1 = ptr;
  VERIFY(wptr1.lock());

  std::weak_ptr<Base> wptr2 = ptr;
  VERIFY(wptr2.lock());

  std::weak_ptr<Base> wptr3 = wptr1;
  VERIFY(wptr3.lock());

  std::weak_ptr<BaseBase> wptr4 = ptr;
  VERIFY(wptr4.lock());

  std::weak_ptr<BaseBase> wptr5 = std::move(wptr1);
  VERIFY(wptr5.lock());

  ptr.reset();

  VERIFY(!wptr1.lock());
  VERIFY(!wptr2.lock());
  VERIFY(!wptr3.lock());
  VERIFY(!wptr4.lock());
  VERIFY(!wptr5.lock());
}

template<typename T>
void test02()
{
  std::shared_ptr<T> ptr(new T);
  VERIFY(ptr);

  std::weak_ptr<T> wptr1 = ptr;
  VERIFY(wptr1.lock());

  std::weak_ptr<Base> wptr2 = ptr;
  VERIFY(wptr2.lock());

  ptr.reset();

  std::weak_ptr<Base> wptr3 = wptr1;
  std::weak_ptr<BaseBase> wptr4 = wptr1;
  std::weak_ptr<BaseBase> wptr5 = std::move(wptr1);

  VERIFY(!wptr1.lock());
  VERIFY(!wptr2.lock());
  VERIFY(!wptr3.lock());
  VERIFY(!wptr4.lock());
  VERIFY(!wptr5.lock());
}

int main()
{
  test01<Derived1>();
  test01<Derived2>();
  test01<Derived4>();
  test01<Derived5>();

  test02<Derived1>();
  test02<Derived2>();
  test02<Derived4>();
  test02<Derived5>();
}

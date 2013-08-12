#include <assert.h>

extern "C" int printf(const char *, ...);

class Subscriptor
{
  public:

  Subscriptor()
    { counter = 1;}

  virtual ~Subscriptor()
  {
    counter--;
    assert(counter == 0);
  }

  private:
    static int counter;
};

int Subscriptor::counter;

template <typename number>
class Polynomial : public Subscriptor
{
};

class LagrangeEquidistant: public Polynomial<double>
{
};

template <int value>
class A
{
 public:
  class Nested: public LagrangeEquidistant
  {
  };
  A() { n = new Nested; }
  ~A() { delete n; }
  Subscriptor * n;
};

template<typename _Tp>
inline void
_MyDestroy(_Tp* __pointer)
  { __pointer->~_Tp(); }

int main()
{
  Subscriptor * s1 =  new  LagrangeEquidistant;
  _MyDestroy(s1);
  A<1> * a1 = new A<1>;
  _MyDestroy(a1);
  A<2> * a2 = new A<2>;
  _MyDestroy(a2);

  return 0;
}

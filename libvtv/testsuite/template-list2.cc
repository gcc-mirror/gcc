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

template<typename _Tp>
inline void
_MyDestroy(_Tp* __pointer)
  { __pointer->~_Tp(); }

int main()
{
  LagrangeEquidistant * s1 =  new  LagrangeEquidistant;
  _MyDestroy(s1);

  return 0;
}

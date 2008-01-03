// PR c++/34619
// { dg-do compile }

template <typename> struct A
{
  typedef int X;
  static const int N = 1;
};

template <typename T> struct B
{
  typedef typename A <int [A <T>::N]>::X Y;
  template <typename U> B (Y, U) {}
};

int main ()
{
}

B <int>b (0, 0);

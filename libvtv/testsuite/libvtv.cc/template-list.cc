// { dg-do run }

#include <assert.h>

extern "C" int printf(const char *, ...);

class Subscriptor
{
  public:

  Subscriptor() : counter(1) {}

  virtual ~Subscriptor()
  {
    counter--;
    assert(counter == 0);
  }

  private:
    mutable int counter;
};

template <int dim> struct Function
{
  Function(int i): value(dim + i) {}
  int value;
};

template <int dim> struct Triangulation
{

};

template <int dim> struct Exercise_2_3
{
  enum { DIM = dim };
};

  template <int dim>
  struct SetUpBase : public Subscriptor
  {
      virtual
      const Function<dim> get_boundary_values () const = 0;

      virtual
      const Function<dim> get_right_hand_side () const = 0;

    //      virtual
    //      void create_coarse_grid (Triangulation<dim> &coarse_grid) const = 0;
  };

  template <class Traits, int dim>
  struct SetUp : public SetUpBase<dim>
  {
      SetUp () {};

      virtual
      const Function<dim>  get_boundary_values () const
    { return Function<dim>(Traits::DIM); }

      virtual
      const Function<dim>  get_right_hand_side () const
    { return Function<dim>(Traits::DIM); }

    //      virtual
    //      void create_coarse_grid (Triangulation<dim> &coarse_grid) const;

    //      static const typename Traits::BoundaryValues boundary_values;
    //      static const typename Traits::RightHandSide  right_hand_side;
  };


int main()
{
  /*

  SetUp<Exercise_2_3<1000>, 2> s1a;
  SetUp<Exercise_2_3<2000>, 1> s2;
  SetUp<Exercise_2_3<2000>, 2> s2a;
  return s1->get_boundary_values().value + s1a.get_boundary_values().value +
      s2.get_boundary_values().value + s2a.get_boundary_values().value +
      s1->get_right_hand_side().value + s1a.get_right_hand_side().value +
      s2.get_right_hand_side().value + s2a.get_right_hand_side().value;
  */
#ifndef NFAIL
  SetUp<Exercise_2_3<1000>, 1> * s1 =  new  SetUp<Exercise_2_3<1000>, 1>();
  printf("%d\n", s1->get_boundary_values().value);
  return 0;
#else
  SetUp<Exercise_2_3<1000>, 1> s1;
  printf("%d\n", s1.get_boundary_values().value);
  return 0;
#endif
}

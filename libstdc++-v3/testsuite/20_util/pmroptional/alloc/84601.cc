// { dg-options "-std=gnu++17" }
// { dg-do compile }

#include "../../include/std/pmroptional"

struct pmr_type
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  pmr_type(std::allocator_arg_t,allocator_type)
  : pmr_type(){}


  pmr_type(std::allocator_arg_t,allocator_type, int i)
  : pmr_type(i){}

  pmr_type(std::allocator_arg_t,allocator_type, pmr_type const& other)
    : pmr_type(other.i){}

  pmr_type(){};

  pmr_type(int _i) : i(_i){};

  pmr_type(pmr_type const& other): i(other.i)
      {};

  pmr_type& operator=(pmr_type const& other)
  {
    i = other.i;
    return *this;
  }
  int i = 0;
};



using pair_t = std::pair<pmr_type, int>;
using opt_t = std::pmr::optional<pair_t>;

static_assert(std::is_copy_constructible_v<opt_t::value_type>);
static_assert(std::is_copy_assignable_v<opt_t::value_type>);

static_assert(std::is_copy_assignable_v<opt_t>); // assertion fails.

class A
{
  void f(const opt_t& opt)
  {
    _opt = opt;
  }

  opt_t _opt;
};

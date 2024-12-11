// { dg-do compile { target c++23 } }

// LWG 3900. allocator_arg_t overloads of generator::promise_type::operator new
// should not be constrained

#include <generator>
#include <memory_resource>

std::pmr::generator<int>
bar(std::allocator_arg_t, std::pmr::memory_resource& mr) // { dg-error "here" }
{
  co_yield 3;
}

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
// { dg-error "no matching function .*memory_resource&" "" { target *-*-* } 0 }

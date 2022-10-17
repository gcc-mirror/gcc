// { dg-do compile { target c++11 } }

#include <queue>

// LWG 3522
// Missing requirement on InputIterator template parameter for priority_queue
// constructors
std::priority_queue<int> x = {1, 2}; // { dg-error "could not convert" }

using Q = std::priority_queue<int>;
using Compare = Q::value_compare;
using Sequence = Q::container_type;

static_assert( ! std::is_constructible<Q, int, int>(),
	       "priority_queue(InputIterator, InputIterator) is constrained" );

static_assert( ! std::is_constructible<Q, int, int, Compare>(),
	       "priority_queue(InputIterator, InputIterator, const Compare&) "
	       " is constrained" );

static_assert( ! std::is_constructible<Q, int, int, Compare, const Sequence&>(),
	       "and with const Sequence lvalue argument" );
static_assert( ! std::is_constructible<Q, int, int, Compare, Sequence>(),
	       "and with Sequence rvalue argument" );

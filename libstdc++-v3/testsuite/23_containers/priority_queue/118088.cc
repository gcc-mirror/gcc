// { dg-do run { target c++11 } }

#include <queue>
#include <vector>
#include <testsuite_hooks.h>

template<typename T, typename Seq>
bool
check(std::priority_queue<T, Seq>& p)
{
  if (!p.empty())
    {
      T prev = p.top();
      p.pop();
      while (!p.empty())
	{
	  if ( prev < p.top() )
	    return false;
	  prev = p.top();
	  p.pop();
	}
    }
  return true;
}

// A vector-like type that has a non-empty moved-from state.
struct Vector : std::vector<int>
{
  using Base = std::vector<int>;

  using Base::Base;

  Vector(const Vector&) = default;
  Vector& operator=(const Vector&) = default;

  Vector(Vector&& v) : Base(static_cast<const Base&>(v))
  {
    invalidate_heap(v);
  }

  Vector(Vector&& v, const std::allocator<int>&)
  : Base(static_cast<const Base&>(v))
  {
    invalidate_heap(v);
  }

  Vector&
  operator=(Vector&& v)
  {
    static_cast<Base&>(*this) = static_cast<const Base&>(v);
    invalidate_heap(v);
    return *this;
  }

  void invalidate_heap(Base& v) { v = {1,2,3}; }
};

void
test_moves()
{
  std::priority_queue<int, Vector> p;
  p.push(1);
  p.push(3);
  p.push(5);
  p.push(2);
  p.push(2);
  p.push(2);
  p.push(2);
  std::priority_queue<int, Vector> p2 = std::move(p);
  VERIFY( check(p) );

  // Allocator-extended move constructor:
  std::priority_queue<int, Vector> p3(std::move(p2), std::allocator<int>());
  VERIFY( check(p2) );

  p2 = std::move(p3);
  VERIFY( check(p3) );
}

int main()
{
  test_moves();
}

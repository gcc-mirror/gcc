// { dg-do run { target c++11 } }

#include <queue>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

void
test_lwg3506()
{
  // LWG 3506 Missing allocator-extended constructors for priority_queue

  using Alloc = __gnu_test::uneq_allocator<int>;

  using Container = std::vector<int, Alloc>;

  struct Queue : std::priority_queue<int, Container>
  {
    using priority_queue::priority_queue;

    Alloc get_allocator() const { return c.get_allocator(); }
  };

  using Compare = Queue::value_compare;

  const Alloc a1(1), a2(2), a3(3), a4(4);
  const int vals[] = { 5, 3, 9, 1, 7 };
  Container cont({ 20, 30, 40 }, Alloc(99));

  Queue q1(vals, vals+5, a1);
  VERIFY( q1.get_allocator() == a1 );
  VERIFY( q1.size() == 5 );
  VERIFY( q1.top() == 9 );

  Queue q2(vals, vals+5, Compare(), a2);
  VERIFY( q2.get_allocator() == a2 );
  VERIFY( q2.size() == 5 );
  VERIFY( q2.top() == 9 );

  Queue q3(vals, vals+5, Compare(), cont, a3);
  VERIFY( q3.get_allocator() == a3 );
  VERIFY( q3.size() == 8 );
  VERIFY( q3.top() == 40 );

  Queue q4(vals, vals+5, Compare(), std::move(cont), a4);
  VERIFY( q4.get_allocator() == a4 );
  VERIFY( q4.size() == 8 );
  VERIFY( q4.top() == 40 );
  VERIFY( cont.empty() );
}

int main()
{
  test_lwg3506();
}

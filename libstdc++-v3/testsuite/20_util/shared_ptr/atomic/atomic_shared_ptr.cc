// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-require-effective-target gthreads }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-add-options libatomic }

#include <memory>

#ifndef __cpp_lib_atomic_shared_ptr
# error "Feature-test macro for atomic<shared_ptr<T>> missing in <memory>"
#elif __cpp_lib_atomic_shared_ptr != 201711L
# error "Feature-test macro for atomic<shared_ptr<T>> has wrong value in <memory>"
#endif

#include <thread>

#include <testsuite_hooks.h>

// Check constexpr constructor.
constinit std::atomic<std::shared_ptr<int>> a;
// LWG 3661. constinit atomic<shared_ptr<T>> a(nullptr); should work
constinit std::atomic<std::shared_ptr<int>> a2 = nullptr;

void
test_is_lock_free()
{
  using test_type = std::atomic<std::shared_ptr<int>>;
  static_assert( test_type::is_always_lock_free == false );

  test_type p;
  VERIFY( p.is_lock_free() == false );
}

void
test_atomic_shared_ptr()
{
  struct A { int a; int b; };

  auto a = std::make_shared<A>( 0, 42 );
  using ptr_t = std::shared_ptr<A>;
  {
    std::atomic<ptr_t> p{ };
    VERIFY( p.load().get() == nullptr );
  }

  std::atomic<ptr_t> p{ a };
  VERIFY( p.load().get() == a.get() );
  auto b = std::make_shared<A>( 42, 0 );
  p.store(b);
  VERIFY( p.load().get() != a.get() );
  VERIFY( p.load().get() == b.get() );
  p.exchange(a);
  VERIFY( p.load().get() != b.get() );
  VERIFY( p.load().get() == a.get() );

  {
    ptr_t aa{ a };
    VERIFY( p.compare_exchange_strong(aa, b,
				      std::memory_order_seq_cst,
				      std::memory_order_seq_cst) == true );
    ptr_t bb{ a };
    VERIFY( p.compare_exchange_strong(bb, b,
				      std::memory_order_seq_cst,
				      std::memory_order_seq_cst) == false );
    VERIFY( bb.get() == b.get() );
  }

  {
    ptr_t bb{ b };
    VERIFY( p.compare_exchange_weak(bb, a,
				    std::memory_order_seq_cst,
				    std::memory_order_seq_cst) == true );
    ptr_t aa{ b };
    VERIFY( p.compare_exchange_weak(aa, a,
				      std::memory_order_seq_cst,
				      std::memory_order_seq_cst) == false );
    VERIFY( aa.get() == a.get() );
  }
}

void
test_wait_notify()
{
  std::atomic<std::shared_ptr<int>> p;
  std::shared_ptr<int> a = std::make_shared<int>();
  std::shared_ptr<int> b = std::make_shared<int>();

  p.store(a);
  p.wait(b);
  std::thread t([&]
      {
	p.store(b);
	p.notify_one();
      });
  p.wait(a);
  t.join();
}

int counter = 0;

void
test_counting()
{
  struct X
  {
    ~X() { ++counter; }
  };

  {
    std::atomic<std::shared_ptr<X>> p{ std::make_shared<X>() };
    std::shared_ptr<X> a = p.load();
    VERIFY( a.use_count() == 2 ); // p, a
    p.store({});
    VERIFY( a.use_count() == 1 ); // a
    p.store(a);
    VERIFY( a.use_count() == 2 ); // p, a
    std::shared_ptr<X> b = std::make_shared<X>();
    std::shared_ptr<X> c = p.exchange(b);
    VERIFY( a.use_count() == 2 ); // a, c
    VERIFY( c == a );
    VERIFY( b.use_count() == 2 ); // p, b
    std::atomic<std::shared_ptr<X>> p2{a};
    VERIFY( a.use_count() == 3 ); // p2, a, c
    VERIFY( p2.compare_exchange_strong(a, b) );
    VERIFY( a.use_count() == 2 ); // a, c
    VERIFY( b.use_count() == 3 ); // p, p2, b
    VERIFY ( ! p2.compare_exchange_strong(a, b) );
    VERIFY( a == b );
    VERIFY( a.use_count() == 4 ); // p, p2, a, b
    VERIFY( b.use_count() == 4 );
    VERIFY( c.use_count() == 1 ); // c
    VERIFY( p.compare_exchange_weak(b, c) );
    VERIFY( b.use_count() == 3 ); // p2, a, b
    VERIFY( c.use_count() == 2 ); // p, c
    VERIFY( ! p.compare_exchange_weak(a, b) );
    VERIFY( a == c );
    VERIFY( a.use_count() == 3 ); // p, a, c
    VERIFY( b.use_count() == 2 ); // p2, b
    VERIFY( c.use_count() == 3 ); // p, a, c
    a.reset();
    b.reset();
    c.reset();
    VERIFY( counter == 0 );
  }
  VERIFY( counter == 2 );
}

void
test_lwg3893()
{
  // LWG 3893. LWG 3661 broke atomic<shared_ptr<T>> a; a = nullptr;
  std::atomic<std::shared_ptr<int>> a;
  a = nullptr;
}

int
main()
{
  test_is_lock_free();
  test_atomic_shared_ptr();
  test_wait_notify();
  test_counting();
  test_lwg3893();
}

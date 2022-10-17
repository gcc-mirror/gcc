// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-require-effective-target gthreads }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-add-options libatomic }

#include <memory>
#include <thread>
#include <testsuite_hooks.h>

// Check constexpr constructor.
constinit std::atomic<std::weak_ptr<int>> a;

void
test_is_lock_free()
{
  using test_type = std::atomic<std::weak_ptr<int>>;
  static_assert( test_type::is_always_lock_free == false );

  test_type p;
  VERIFY( p.is_lock_free() == false );
}

void
test_atomic_weak_ptr()
{
  struct A { int a; int b; };

  auto a = std::make_shared<A>( 0, 42 );
  using ptr_t = std::weak_ptr<A>;
  ptr_t wa{ a };
  {
    std::atomic<ptr_t> p{ };
    VERIFY( p.load().lock().get() == nullptr );
  }

  std::atomic<ptr_t> p{ wa };
  VERIFY( p.load().lock().get() == a.get() );

  auto b = std::make_shared<A>( 42, 0 );
  ptr_t wb{ b };
  p.store(wb);
  VERIFY( p.load().lock().get() != a.get() );
  VERIFY( p.load().lock().get() == b.get() );
  p.exchange(wa);
  VERIFY( p.load().lock().get() != b.get() );
  VERIFY( p.load().lock().get() == a.get() );

  {
    ptr_t aa{ a };
    VERIFY( p.compare_exchange_strong(aa, b,
				      std::memory_order_seq_cst,
				      std::memory_order_seq_cst) == true );
    ptr_t bb{ a };
    VERIFY( p.compare_exchange_strong(bb, b,
					std::memory_order_seq_cst,
					std::memory_order_seq_cst) == false );
    VERIFY( bb.lock().get() == b.get() );
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
    VERIFY( aa.lock().get() == a.get() );
  }
}

void
test_wait_notify()
{
  std::atomic<std::weak_ptr<int>> p;
  std::weak_ptr<int> a = std::make_shared<int>();
  std::weak_ptr<int> b = std::make_shared<int>();

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

int
main()
{
  test_is_lock_free();
  test_atomic_weak_ptr();
  test_wait_notify();
}

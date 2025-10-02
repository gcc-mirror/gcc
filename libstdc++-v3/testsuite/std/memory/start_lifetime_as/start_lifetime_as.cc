// { dg-do run { target c++23 } }

#include <bit>
#include <memory>

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct S { int a; int b; };
struct T { long long c; };

template<typename S, typename T>
void
test01()
{
}

template<typename S, typename T>
requires (sizeof(S) == sizeof(T))
void
test01()
{
  union U { unsigned char a[sizeof(S)]; S b; T c; } u;
  u.a[0] = 1;
  T v = std::bit_cast<T> (S{1, 2});
  union V { unsigned char a[3 * sizeof(S)]; S b[3]; T c[3]; } w;
  T x = std::bit_cast<T> (S{3, 4});
  T y = std::bit_cast<T> (S{5, 6});
  S* d = std::start_lifetime_as<S>(reinterpret_cast<void*>(&u.a));
  d->a = 1;
  d->b = 2;
  T* e = std::start_lifetime_as<T>(reinterpret_cast<void*>(d));
  VERIFY( e->c == v.c );
  const T* f = std::start_lifetime_as<T>(reinterpret_cast<const void*>(d));
  VERIFY( f->c == v.c );
  volatile T* g
    = std::start_lifetime_as<T>(reinterpret_cast<volatile void*>(d));
  VERIFY( g->c == v.c );
  const volatile T* h
    = std::start_lifetime_as<T>(reinterpret_cast<const volatile void*>(d));
  VERIFY( h->c == v.c );
  S* i = std::start_lifetime_as_array<S>(reinterpret_cast<void*>(&w.a), 3);
  i[0].a = 1;
  i[0].b = 2;
  i[1].a = 3;
  i[1].b = 4;
  i[2].a = 5;
  i[2].b = 6;
  T* j = std::start_lifetime_as_array<T>(reinterpret_cast<void*>(i), 3);
  VERIFY( j[0].c == v.c && j[1].c == x.c && j[2].c == y.c );
  const T* k
    = std::start_lifetime_as_array<T>(reinterpret_cast<const void*>(i), 3);
  VERIFY( k[0].c == v.c && k[1].c == x.c && k[2].c == y.c );
  volatile T* l
    = std::start_lifetime_as_array<T>(reinterpret_cast<volatile void*>(i), 3);
  VERIFY( l[0].c == v.c && l[1].c == x.c && l[2].c == y.c );
  const volatile T* m
    = std::start_lifetime_as_array<T>(reinterpret_cast<const volatile void*>(i),
				      3);
  VERIFY( m[0].c == v.c && m[1].c == x.c && m[2].c == y.c );
  T* n = std::start_lifetime_as_array<T>(static_cast<void*>(nullptr), 0);
  VERIFY( n == nullptr );
  const T* o
    = std::start_lifetime_as_array<T>(static_cast<const void*>(nullptr), 0);
  VERIFY( o == nullptr );
  volatile T* p
    = std::start_lifetime_as_array<T>(static_cast<volatile void*>(nullptr), 0);
  VERIFY( p == nullptr );
  const volatile T* q
    = std::start_lifetime_as_array<T>(static_cast<const volatile void*>(nullptr),
				      0);
  VERIFY( q == nullptr );
  VERIFY( std::start_lifetime_as_array<T>(reinterpret_cast<void*>(&w.a), 0)
	  == &w.c[0] );
  VERIFY( std::start_lifetime_as_array<T>(reinterpret_cast<const void*>(&w.a), 0)
	  == static_cast<const T*>(&w.c[0]) );
  VERIFY( std::start_lifetime_as_array<T>(reinterpret_cast<volatile void*>(&w.a),
					  0)
	  == static_cast<volatile T*>(&w.c[0]) );
  VERIFY( std::start_lifetime_as_array<T>(reinterpret_cast<const volatile void*>(&w.a),
					  0)
	  == static_cast<const volatile T*>(&w.c[0]) );
  static const S r[] = { { 5, 6 }, { 3, 4 } };
  const T* s = std::start_lifetime_as<T>(&r[1]);
  VERIFY( s->c == x.c );
  const T* t = std::start_lifetime_as_array<T>(&r[0], 2);
  VERIFY( t[0].c == y.c && t[1].c == x.c );
}

int
main()
{
  test01<S, T>();
}

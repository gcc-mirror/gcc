// { dg-do compile { target c++23 } }

#include <expected>
#include <type_traits>

// Default construction

template<typename T, typename E>
  constexpr bool default_constructible
    = std::is_default_constructible_v<std::expected<T, E>>;

struct A { A(int); };

static_assert( default_constructible< int,  int > );
static_assert( default_constructible< A,    int > == false );
static_assert( default_constructible< int,  A   > );
static_assert( default_constructible< A,    A   > == false );
static_assert( default_constructible< int,  A   > );
static_assert( default_constructible< void, int > );

// Destruction

template<typename T, typename E>
  constexpr bool trivially_destructible
    = std::is_trivially_destructible_v<std::expected<T, E>>;

struct B { ~B(); };

static_assert( trivially_destructible< int,  int > );
static_assert( trivially_destructible< B,    int > == false );
static_assert( trivially_destructible< int,  B   > == false );
static_assert( trivially_destructible< B,    B   > == false );
static_assert( trivially_destructible< void, int > );
static_assert( trivially_destructible< void, B   > == false );

enum Result { No, Yes, NoThrow, Trivial };

// Copy construction

template<typename T, typename E>
  constexpr Result copy_constructible
    = std::is_trivially_copy_constructible_v<std::expected<T, E>> ? Trivial
    : std::is_copy_constructible_v<std::expected<T, E>> ? Yes
    : No;

struct C { C(const C&); };
struct D { D(D&&); };

static_assert( copy_constructible< int,  int > == Trivial );
static_assert( copy_constructible< C,    C   > == Yes );
static_assert( copy_constructible< C,    int > == Yes );
static_assert( copy_constructible< int,  C   > == Yes );
static_assert( copy_constructible< int,  D   > == No );
static_assert( copy_constructible< D,    int > == No );
static_assert( copy_constructible< D,    D   > == No );
static_assert( copy_constructible< void, int > == Trivial );
static_assert( copy_constructible< void, C   > == Yes );
static_assert( copy_constructible< void, D   > == No );

// Move construction

template<typename T, typename E>
  constexpr Result move_constructible
    = std::is_trivially_move_constructible_v<std::expected<T, E>> ? Trivial
    : std::is_nothrow_move_constructible_v<std::expected<T, E>> ? NoThrow
    : std::is_move_constructible_v<std::expected<T, E>> ? Yes
    : No;

struct E { E(E&&) noexcept; };

static_assert( move_constructible< int,  int > == Trivial );
static_assert( move_constructible< C,    C   > == Yes );
static_assert( move_constructible< C,    int > == Yes );
static_assert( move_constructible< int,  C   > == Yes );
static_assert( move_constructible< D,    D   > == Yes );
static_assert( move_constructible< D,    int > == Yes );
static_assert( move_constructible< int,  D   > == Yes );
static_assert( move_constructible< E,    E   > == NoThrow );
static_assert( move_constructible< E,    int > == NoThrow );
static_assert( move_constructible< int,  E   > == NoThrow );
static_assert( move_constructible< void, int > == Trivial );
static_assert( move_constructible< void, C   > == Yes );
static_assert( move_constructible< void, D   > == Yes );
static_assert( move_constructible< void, E   > == NoThrow );

// Copy assignment

template<typename T, typename E>
  constexpr Result copy_assignable
    = std::is_trivially_copy_assignable_v<std::expected<T, E>> ? Trivial
      : std::is_nothrow_copy_assignable_v<std::expected<T, E>> ? NoThrow
      : std::is_copy_assignable_v<std::expected<T, E>> ? Yes
      : No;

struct F { F(F&&); F& operator=(const F&); }; // not copy-constructible

template<bool CopyCtor, bool MoveCtor, bool CopyAssign, bool MoveAssign>
struct X {
    X(const X&) noexcept(CopyCtor);
    X(X&&) noexcept(MoveCtor);
    X& operator=(const X&) noexcept(CopyAssign);
    X& operator=(X&&) noexcept(MoveAssign);
};
using G = X<false, false, false, false>;
using H = X<false, true, true, true>;
using I = X<true, true, true, false>;

static_assert( copy_assignable< int,  int > == Trivial );
static_assert( copy_assignable< F,    int > == No );
static_assert( copy_assignable< int,  F   > == No );
static_assert( copy_assignable< F,    F   > == No );
static_assert( copy_assignable< G,    int > == Yes );
static_assert( copy_assignable< int,  G   > == Yes );
static_assert( copy_assignable< G,    G   > == No );
static_assert( copy_assignable< int,  H   > == Yes );
static_assert( copy_assignable< H,    H   > == Yes );
static_assert( copy_assignable< int,  I   > == NoThrow );
static_assert( copy_assignable< I,    I   > == NoThrow );
static_assert( copy_assignable< void, int > == Trivial );
static_assert( copy_assignable< void, F > == No );
static_assert( copy_assignable< void, G > == Yes );
static_assert( copy_assignable< void, H > == Yes );
static_assert( copy_assignable< void, I > == NoThrow );

// Move assignment

template<typename T, typename E>
  constexpr Result move_assignable
    = std::is_trivially_move_assignable_v<std::expected<T, E>> ? Trivial
      : std::is_nothrow_move_assignable_v<std::expected<T, E>> ? NoThrow
      : std::is_move_assignable_v<std::expected<T, E>> ? Yes
      : No;

static_assert( move_assignable< int,  int > == Trivial );
static_assert( move_assignable< F,    int > == Yes );
static_assert( move_assignable< int,  F   > == Yes );
static_assert( move_assignable< F,    F   > == No );
static_assert( move_assignable< G,    int > == Yes );
static_assert( move_assignable< int,  G   > == Yes );
static_assert( move_assignable< G,    G   > == No );
static_assert( move_assignable< int,  H   > == NoThrow );
static_assert( move_assignable< H,    H   > == NoThrow );
static_assert( move_assignable< I,    I   > == Yes );
static_assert( move_assignable< void, int > == Trivial );
static_assert( move_assignable< void, F > == Yes );
static_assert( move_assignable< void, G > == Yes );
static_assert( move_assignable< void, H > == NoThrow );
static_assert( move_assignable< void, I > == Yes );

// QoI properties
static_assert( sizeof(std::expected<char, unsigned char>) == 2 );
static_assert( sizeof(std::expected<void, char>) == 2 );
static_assert( sizeof(std::expected<void*, char>) == sizeof(void*) + __alignof(void*) );
static_assert( alignof(std::expected<void, char>) == 1 );
static_assert( alignof(std::expected<void*, char>) == alignof(void*) );

// For QoI we propagate noexcept(false) from trivial special members.
template<bool CopyCtor, bool MoveCtor, bool CopyAssign, bool MoveAssign>
struct Y {
    Y(const Y&) noexcept(CopyCtor) = default;
    Y(Y&&) noexcept(MoveCtor) = default;
    Y& operator=(const Y&) noexcept(CopyAssign) = default;
    Y& operator=(Y&&) noexcept(MoveAssign) = default;
};

template<int I> using Yi = Y<bool(I&8), bool(I&4), bool(I&2), bool(I&1)>;

template<typename> constexpr bool nothrow_copy = false;
template<typename> constexpr bool nothrow_move = false;

template<bool CC, bool MC, bool CA, bool MA>
constexpr bool nothrow_copy<Y<CC, MC, CA, MA>> = CC && CA;

template<bool CC, bool MC, bool CA, bool MA>
constexpr bool nothrow_move<Y<CC, MC, CA, MA>> = MC && MA;

template<> constexpr bool nothrow_copy<void> = true;
template<> constexpr bool nothrow_move<void> = true;

template<typename A, typename B>
consteval bool do_checks()
{
  if constexpr (std::is_void_v<A> || std::is_nothrow_move_constructible_v<A>
		|| std::is_nothrow_move_constructible_v<B>)
    {
      // All assignments should be trivial
      static_assert( copy_assignable<A, B> == Trivial );
      static_assert( move_assignable<A, B> == Trivial );
      // But whether they are nothrow depends on the noexcept-specifiers
      static_assert( std::is_nothrow_copy_assignable_v<std::expected<A, B>>
		    == (nothrow_copy<A> && nothrow_copy<B>) );
      static_assert( std::is_nothrow_move_assignable_v<std::expected<A, B>>
		    == (nothrow_move<A> && nothrow_move<B>) );
    }
  else
    {
      static_assert( copy_assignable<A, B> == No );
      static_assert( move_assignable<A, B> == No );
    }
  return true;
}

template<typename A, int... I>
consteval bool check(std::integer_sequence<int, I...>)
{
  return (do_checks<A, Yi<I>>() && ...);
}

template<int... I>
consteval bool
check_all(std::integer_sequence<int, I...> i)
{
  return (check<Yi<I>>(i) && ...) && check<void>(i);
}

static_assert(check_all(std::make_integer_sequence<int, 16>{}));

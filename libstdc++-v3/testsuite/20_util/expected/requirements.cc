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
  constexpr bool copy_assignable
    = std::is_copy_assignable_v<std::expected<T, E>>;

struct F { F(F&&); F& operator=(const F&); }; // not copy-constructible
struct G { G(const G&); G(G&&); G& operator=(const G&); }; // throwing move

static_assert( copy_assignable< int,  int > );
static_assert( copy_assignable< F,    int > == false );
static_assert( copy_assignable< int,  F   > == false );
static_assert( copy_assignable< F,    F   > == false );
static_assert( copy_assignable< G,    int > );
static_assert( copy_assignable< int,  G   > );
static_assert( copy_assignable< G,    G   > == false );
static_assert( copy_assignable< void, int > );
static_assert( copy_assignable< void, F > == false );
static_assert( copy_assignable< void, G > );

// Move assignment

template<typename T, typename E>
  constexpr bool move_assignable
    = std::is_move_assignable_v<std::expected<T, E>>;

static_assert( move_assignable< int,  int > );
static_assert( move_assignable< F,    int > );
static_assert( move_assignable< int,  F   > );
static_assert( move_assignable< F,    F   > == false );
static_assert( move_assignable< G,    int > );
static_assert( move_assignable< int,  G   > );
static_assert( move_assignable< G,    G   > == false );
static_assert( move_assignable< void, int > );
static_assert( move_assignable< void, F > );
static_assert( move_assignable< void, G > );

// QoI properties
static_assert( sizeof(std::expected<char, unsigned char>) == 2 );
static_assert( sizeof(std::expected<void, char>) == 2 );
static_assert( sizeof(std::expected<void*, char>) == sizeof(void*) + __alignof(void*) );
static_assert( alignof(std::expected<void, char>) == 1 );
static_assert( alignof(std::expected<void*, char>) == alignof(void*) );

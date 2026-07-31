// { dg-do compile { target c++20 } }

#include <coroutine>

struct Promise
{};

template<>
struct std::coroutine_handle<Promise> // { dg-error "cannot be specialized" }
{};

template<typename>
struct PromiseTempl
{};

template<typename T>
struct std::coroutine_handle<PromiseTempl<T>> // { dg-error "cannot be specialized" }
{};

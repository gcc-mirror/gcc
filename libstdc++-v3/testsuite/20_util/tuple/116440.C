// PR libstdc++/116440 - std::tuple<std::tuple<std::any>> does not compile
// { dg-do compile { target c++17 } }

#include <any>
#include <tuple>
#include <type_traits>

template <typename T>
using TupleTuple = std::tuple<std::tuple<T>>;

struct EmbedAny {
    std::any content;
};

static_assert(std::is_copy_constructible<TupleTuple<EmbedAny>>::value);
static_assert(std::is_move_constructible<TupleTuple<EmbedAny>>::value);

static_assert(std::is_copy_constructible<TupleTuple<std::any>>::value);
static_assert(std::is_move_constructible<TupleTuple<std::any>>::value);

static_assert(std::is_constructible_v<std::any, TupleTuple<std::any>>);

struct EmbedAnyWithZeroSizeArray {
    void* pad[0];
    std::any content;
};

static_assert(std::is_copy_constructible<TupleTuple<EmbedAnyWithZeroSizeArray>>::value);
static_assert(std::is_move_constructible<TupleTuple<EmbedAnyWithZeroSizeArray>>::value);

// { dg-do compile { target c++11 } }

#include <memory>


// Check for presence/absence of nested types.

template<typename T> using res_type = typename std::hash<T>::result_type;
template<typename T> using arg_type = typename std::hash<T>::argument_type;

template<typename UniqPtr, typename = void>
constexpr bool
has_res_type(...)
{ return false; }

template<typename UniqPtr>
constexpr typename std::is_void<res_type<UniqPtr>>::value_type // i.e. bool
has_res_type()
{ return true; }

template<typename UniqPtr, typename = void>
constexpr bool
has_arg_type(...)
{ return false; }

template<typename UniqPtr>
constexpr typename std::is_void<arg_type<UniqPtr>>::value_type // i.e. bool
has_arg_type()
{ return true; }

template<typename UniqPtr>
constexpr bool
has_no_types()
{ return ! has_res_type<UniqPtr>() && ! has_arg_type<UniqPtr>(); }

#if __cplusplus >= 202002L
// Nested types result_type and argument_type are not present in C++20
static_assert( has_no_types<std::unique_ptr<int>>() );
static_assert( has_no_types<std::unique_ptr<double>>() );
#else
// Nested types result_type and argument_type are deprecated in C++17.
using R1 = std::hash<std::unique_ptr<int>>::result_type; // { dg-warning "deprecated" "" { target c++17_only } }
using A1 = std::hash<std::unique_ptr<int>>::argument_type; // { dg-warning "deprecated" "" { target c++17_only } }
#endif

struct S { };
template<> struct std::hash<S*> // disabled specialization
{
  hash(hash&&) = delete;
  ~hash() = delete;
};
// Disabled specializations do not have the nested types.
static_assert( has_no_types<std::unique_ptr<S>>(), "disabled specialization" );

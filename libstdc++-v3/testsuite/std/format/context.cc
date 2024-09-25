// { dg-do compile { target c++20 } }

#include <format>

template<typename Context>
concept format_context_reqs = std::is_destructible_v<Context>
  && (!std::is_default_constructible_v<Context>)
  && (!std::is_copy_constructible_v<Context>)
  && (!std::is_move_constructible_v<Context>)
  && (!std::is_copy_assignable_v<Context>)
  && (!std::is_move_assignable_v<Context>)
  && requires (Context& ctx, const Context& cctx) {
    typename Context::iterator;
    typename Context::char_type;
    requires std::same_as<typename Context::template formatter_type<int>,
			  std::formatter<int, typename Context::char_type>>;
    { ctx.locale() } -> std::same_as<std::locale>;
    { ctx.out() } -> std::same_as<typename Context::iterator>;
    { ctx.advance_to(ctx.out()) } -> std::same_as<void>;
    { cctx.arg(1) } -> std::same_as<std::basic_format_arg<Context>>;
  };

template<typename Out, typename charT>
constexpr bool
check(std::basic_format_context<Out, charT>*)
{
  using context = std::basic_format_context<Out, charT>;
  static_assert( format_context_reqs<context> );
  static_assert( std::is_same_v<typename context::iterator, Out> );
  static_assert( std::is_same_v<typename context::char_type, charT> );
  return true;
}

static_assert( check( (std::format_context*)nullptr) );
static_assert( check( (std::wformat_context*)nullptr) );
static_assert( check( (std::basic_format_context<char*, char>*)nullptr) );

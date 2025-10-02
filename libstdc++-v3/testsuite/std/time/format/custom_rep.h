#include <chrono>
#include <ostream>

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(CharT, S)

template<typename Ret = void, typename Under = long>
struct Rep
{
  using Return
    = std::conditional_t<std::is_void_v<Ret>, Rep, Ret>;

  Rep(Under v = 0) : val(v) {}

  template<typename ORet, typename OUnder>
  Rep(Rep<ORet, OUnder> o) : val(o.val) {}

  operator Under() const
  { return val; }

  Return
  operator+() const
  { return val; }

  Rep
  operator-() const
  { return -val; }

  friend Rep
  operator+(Rep lhs, Rep rhs)
  { return lhs.val + rhs.val; }

  friend Rep
  operator-(Rep lhs, Rep rhs)
  { return lhs.val - rhs.val; }

  friend Rep
  operator*(Rep lhs, Rep rhs)
  { return lhs.val * rhs.val; }

  friend Rep
  operator/(Rep lhs, Rep rhs)
  { return lhs.val / rhs.val; }

  friend auto operator<=>(Rep, Rep) = default;

  template<typename CharT>
  friend std::basic_ostream<CharT>&
  operator<<(std::basic_ostream<CharT>& os, const Rep& t)
  { return os << t.val << WIDEN("[via <<]"); }

  Under val;
};

template<typename Ret, typename Under1, typename Under2>
struct std::common_type<Rep<Ret, Under1>, Rep<Ret, Under2>>
{
  using type = Rep<Ret, std::common_type_t<Under1, Under2>>;
};

template<typename Ret, typename Under, typename Other>
  requires std::is_integral_v<Other>
struct std::common_type<Rep<Ret, Under>, Other>
{
  using type = Rep<Ret, std::common_type_t<Under, Other>>;
};

template<typename Ret, typename Under, typename Other>
  requires std::is_integral_v<Other>
struct std::common_type<Other, Rep<Ret, Under>>
  : std::common_type<Rep<Ret, Under>, Other>
{ };

template<typename Ret, typename Under>
struct std::numeric_limits<Rep<Ret, Under>>
  : std::numeric_limits<Under>
{ };

template<typename Ret, typename Under, typename CharT>
struct std::formatter<Rep<Ret, Under>, CharT>
  : std::formatter<Under, CharT>
{
  template<typename Out>
  typename std::basic_format_context<Out, CharT>::iterator
  format(const Rep<Ret>& t, std::basic_format_context<Out, CharT>& ctx) const
  {
    constexpr std::basic_string_view<CharT> suffix = WIDEN("[via format]");
    auto out = std::formatter<Under, CharT>::format(t.val, ctx);
    return std::ranges::copy(suffix, out).out;
  }
};


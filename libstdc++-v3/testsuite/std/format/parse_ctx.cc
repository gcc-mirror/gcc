// { dg-do run { target c++20 } }

#include <format>
#include <testsuite_hooks.h>

static_assert(std::is_constructible_v<std::format_parse_context,
				      std::string_view>);
static_assert(std::is_constructible_v<std::wformat_parse_context,
				      std::wstring_view>);

#if __cpp_lib_format < 202305
constexpr bool construct_with_num_args = true;
#else
constexpr bool construct_with_num_args = false;
#endif

static_assert(std::is_constructible_v<std::format_parse_context,
				      std::string_view, std::size_t>
				      == construct_with_num_args);
static_assert(std::is_constructible_v<std::wformat_parse_context,
				      std::wstring_view, std::size_t>
				      == construct_with_num_args);

static_assert( ! std::is_constructible_v<std::format_parse_context,
					 std::wstring_view>);
static_assert( ! std::is_constructible_v<std::wformat_parse_context,
					 std::string_view>);

static_assert( ! std::is_convertible_v<std::string_view,
				       std::format_parse_context> );
static_assert( ! std::is_convertible_v<std::wstring_view,
				       std::wformat_parse_context> );

static_assert( ! std::is_default_constructible_v<std::format_parse_context> );
static_assert( ! std::is_copy_constructible_v<std::format_parse_context> );
static_assert( ! std::is_move_constructible_v<std::format_parse_context> );
static_assert( ! std::is_copy_assignable_v<std::format_parse_context> );
static_assert( ! std::is_move_assignable_v<std::format_parse_context> );

// This concept is satisfied if the next_arg_id() call is a constant expression
template<typename Ch, typename PC = std::basic_format_parse_context<Ch>>
concept arg_id_available = requires {
  typename std::integral_constant<std::size_t,
				  PC({}).next_arg_id()>::type;
};

void
test_members()
{
  std::string_view s = "spec string";

  std::format_parse_context pc(s);

  VERIFY( pc.begin() == s.begin() );
  VERIFY( pc.end() == s.end() );
  pc.advance_to(s.begin() + 5);
  VERIFY( pc.begin() == s.begin() + 5 );

  // Runtime calls to these do not check for the correct number of args.
  VERIFY( pc.next_arg_id() == 0 );
  VERIFY( pc.next_arg_id() == 1 );
  VERIFY( pc.next_arg_id() == 2 );
  try
  {
    // Cannot mix manual and automatic indexing.
    pc.check_arg_id(0);
    VERIFY( false );
  }
  catch (const std::format_error&)
  {
  }
  // But they do check during constant evaluation:
  VERIFY( ! arg_id_available<char> );
  VERIFY( ! arg_id_available<wchar_t> );

  std::format_parse_context pc2("");
  pc2.check_arg_id(2);
  pc2.check_arg_id(1);
  pc2.check_arg_id(3);
  try
  {
    // Cannot mix manual and automatic indexing.
    (void) pc2.next_arg_id();
    VERIFY( false );
  }
  catch (const std::format_error&)
  {
  }
}

template<typename T, bool auto_indexing = true>
bool
is_std_format_spec_for(std::string_view spec)
{
  std::format_parse_context pc(spec);
  if (auto_indexing)
    (void) pc.next_arg_id();
  else
    pc.check_arg_id(0);

  std::formatter<T> f;
  try {
    auto end = f.parse(pc);
    VERIFY( end == spec.end() || *end == '}' );
    return true;
  } catch (const std::format_error&) {
    return false;
  }
}

#if __cpp_lib_format_ranges
constexpr bool escaped_strings_supported = true;
#else
constexpr bool escaped_strings_supported = false;
#endif

void
test_char()
{
  VERIFY( is_std_format_spec_for<char>("") );
  VERIFY( is_std_format_spec_for<char>("<") );
  VERIFY( is_std_format_spec_for<char>(">") );
  VERIFY( is_std_format_spec_for<char>("^") );
  VERIFY( is_std_format_spec_for<char>("0<") );
  VERIFY( is_std_format_spec_for<char>("0>") );
  VERIFY( is_std_format_spec_for<char>("0^") );
  VERIFY( ! is_std_format_spec_for<char>("{^") );
  VERIFY( ! is_std_format_spec_for<char>("+") );
  VERIFY( ! is_std_format_spec_for<char>("-") );
  VERIFY( ! is_std_format_spec_for<char>(" ") );
  VERIFY( ! is_std_format_spec_for<char>("#") );
  VERIFY( is_std_format_spec_for<char>("0d") );
  VERIFY( ! is_std_format_spec_for<char>("0") );
  VERIFY( ! is_std_format_spec_for<char>("00d") );
  VERIFY( is_std_format_spec_for<char>("01d") );
  VERIFY( is_std_format_spec_for<char>("0{}d") );
  VERIFY( ! is_std_format_spec_for<char>("0{1}d") );
  VERIFY(( is_std_format_spec_for<char, false>("0{1}d") ));
  VERIFY( is_std_format_spec_for<char>("1") );
  VERIFY( ! is_std_format_spec_for<char>("-1") );
  VERIFY( is_std_format_spec_for<char>("-1d") ); // sign and width
  VERIFY( ! is_std_format_spec_for<char>(".") );
  VERIFY( ! is_std_format_spec_for<char>(".1") );
  VERIFY( is_std_format_spec_for<char>("c") );
  VERIFY( is_std_format_spec_for<char>("b") );
  VERIFY( is_std_format_spec_for<char>("B") );
  VERIFY( is_std_format_spec_for<char>("d") );
  VERIFY( is_std_format_spec_for<char>("o") );
  VERIFY( is_std_format_spec_for<char>("x") );
  VERIFY( is_std_format_spec_for<char>("X") );
  VERIFY( ! is_std_format_spec_for<char>("s") );
  VERIFY( is_std_format_spec_for<char>("?") == escaped_strings_supported );
  VERIFY( ! is_std_format_spec_for<char>("a") );
  VERIFY( ! is_std_format_spec_for<char>("A") );
  VERIFY( ! is_std_format_spec_for<char>("f") );
  VERIFY( ! is_std_format_spec_for<char>("F") );
  VERIFY( ! is_std_format_spec_for<char>("g") );
  VERIFY( ! is_std_format_spec_for<char>("G") );
  VERIFY( ! is_std_format_spec_for<char>("+c") );
  VERIFY( ! is_std_format_spec_for<char>("+?") );
  VERIFY( is_std_format_spec_for<char>("+d") );
}

void
test_int()
{
  VERIFY( is_std_format_spec_for<int>("") );
  VERIFY( is_std_format_spec_for<int>("<") );
  VERIFY( is_std_format_spec_for<int>(">") );
  VERIFY( is_std_format_spec_for<int>("^") );
  VERIFY( is_std_format_spec_for<int>("0<") );
  VERIFY( is_std_format_spec_for<int>("0>") );
  VERIFY( is_std_format_spec_for<int>("0^") );
  VERIFY( ! is_std_format_spec_for<int>("{^") );
  VERIFY( is_std_format_spec_for<int>("+") );
  VERIFY( is_std_format_spec_for<int>("-") );
  VERIFY( is_std_format_spec_for<int>(" ") );
  VERIFY( is_std_format_spec_for<int>("#") );
  VERIFY( is_std_format_spec_for<int>("0d") );
  VERIFY( is_std_format_spec_for<int>("0") );
  VERIFY( ! is_std_format_spec_for<int>("00d") );
  VERIFY( is_std_format_spec_for<int>("01d") );
  VERIFY( ! is_std_format_spec_for<int>("0{1}d") );
  VERIFY(( is_std_format_spec_for<int>("0{}d") ));
  VERIFY(( ! is_std_format_spec_for<int>("0{1}d") ));
  VERIFY(( is_std_format_spec_for<int, false>("0{1}d") ));
  VERIFY( is_std_format_spec_for<int>("1") );
  VERIFY( is_std_format_spec_for<int>("-1") ); // sign and width
  VERIFY( ! is_std_format_spec_for<int>(".") );
  VERIFY( ! is_std_format_spec_for<int>(".1") );
  VERIFY( is_std_format_spec_for<int>("c") );
  VERIFY( is_std_format_spec_for<int>("b") );
  VERIFY( is_std_format_spec_for<int>("B") );
  VERIFY( is_std_format_spec_for<int>("d") );
  VERIFY( is_std_format_spec_for<int>("o") );
  VERIFY( is_std_format_spec_for<int>("x") );
  VERIFY( is_std_format_spec_for<int>("X") );
  VERIFY( ! is_std_format_spec_for<int>("s") );
  VERIFY( ! is_std_format_spec_for<int>("?") );
  VERIFY( ! is_std_format_spec_for<int>("a") );
  VERIFY( ! is_std_format_spec_for<int>("A") );
  VERIFY( ! is_std_format_spec_for<int>("f") );
  VERIFY( ! is_std_format_spec_for<int>("F") );
  VERIFY( ! is_std_format_spec_for<int>("g") );
  VERIFY( ! is_std_format_spec_for<int>("G") );
  VERIFY( ! is_std_format_spec_for<int>("p") );
  VERIFY( ! is_std_format_spec_for<int>("P") );
  VERIFY( is_std_format_spec_for<int>("+c") ); // But LWG 3644 would change it.
  VERIFY( ! is_std_format_spec_for<int>("+?") );
  VERIFY( is_std_format_spec_for<int>("+d") );
}

void
test_bool()
{
  VERIFY( is_std_format_spec_for<bool>("") );
  VERIFY( is_std_format_spec_for<bool>("<") );
  VERIFY( is_std_format_spec_for<bool>(">") );
  VERIFY( is_std_format_spec_for<bool>("^") );
  VERIFY( is_std_format_spec_for<bool>("0<") );
  VERIFY( is_std_format_spec_for<bool>("0>") );
  VERIFY( is_std_format_spec_for<bool>("0^") );
  VERIFY( ! is_std_format_spec_for<bool>("{^") );
  VERIFY( ! is_std_format_spec_for<bool>("+") );
  VERIFY( ! is_std_format_spec_for<bool>("-") );
  VERIFY( ! is_std_format_spec_for<bool>(" ") );
  VERIFY( ! is_std_format_spec_for<bool>("#") );
  VERIFY( is_std_format_spec_for<bool>("0d") );
  VERIFY( ! is_std_format_spec_for<bool>("0") );
  VERIFY( ! is_std_format_spec_for<bool>("00d") );
  VERIFY( is_std_format_spec_for<bool>("01d") );
  VERIFY( is_std_format_spec_for<bool>("1") );
  VERIFY( ! is_std_format_spec_for<bool>("-1") );
  VERIFY( is_std_format_spec_for<bool>("-1d") ); // sign and width
  VERIFY( ! is_std_format_spec_for<bool>(".") );
  VERIFY( ! is_std_format_spec_for<bool>(".1") );
  VERIFY( ! is_std_format_spec_for<bool>("c") );
  VERIFY( is_std_format_spec_for<bool>("b") );
  VERIFY( is_std_format_spec_for<bool>("B") );
  VERIFY( is_std_format_spec_for<bool>("d") );
  VERIFY( is_std_format_spec_for<bool>("o") );
  VERIFY( is_std_format_spec_for<bool>("x") );
  VERIFY( is_std_format_spec_for<bool>("X") );
  VERIFY( is_std_format_spec_for<bool>("s") );
  VERIFY( ! is_std_format_spec_for<bool>("?") );
  VERIFY( ! is_std_format_spec_for<bool>("a") );
  VERIFY( ! is_std_format_spec_for<bool>("A") );
  VERIFY( ! is_std_format_spec_for<bool>("f") );
  VERIFY( ! is_std_format_spec_for<bool>("F") );
  VERIFY( ! is_std_format_spec_for<bool>("g") );
  VERIFY( ! is_std_format_spec_for<bool>("G") );
  VERIFY( ! is_std_format_spec_for<bool>("p") );
  VERIFY( ! is_std_format_spec_for<bool>("P") );
  VERIFY( ! is_std_format_spec_for<bool>("+s") );
  VERIFY( is_std_format_spec_for<bool>("+d") );
}

void
test_float()
{
  VERIFY( is_std_format_spec_for<float>("") );
  VERIFY( is_std_format_spec_for<float>("<") );
  VERIFY( is_std_format_spec_for<float>(">") );
  VERIFY( is_std_format_spec_for<float>("^") );
  VERIFY( is_std_format_spec_for<float>("0<") );
  VERIFY( is_std_format_spec_for<float>("0>") );
  VERIFY( is_std_format_spec_for<float>("0^") );
  VERIFY( ! is_std_format_spec_for<float>("{^") );
  VERIFY( is_std_format_spec_for<float>("+") );
  VERIFY( is_std_format_spec_for<float>("-") );
  VERIFY( is_std_format_spec_for<float>(" ") );
  VERIFY( is_std_format_spec_for<float>("#") );
  VERIFY( is_std_format_spec_for<float>("0f") );
  VERIFY( is_std_format_spec_for<float>("0") );
  VERIFY( ! is_std_format_spec_for<float>("00f") );
  VERIFY( is_std_format_spec_for<float>("01f") );
  VERIFY( is_std_format_spec_for<float>("0{}f") );
  VERIFY( ! is_std_format_spec_for<float>("0{1}f") );
  VERIFY( ! is_std_format_spec_for<float>("0{1}f") );
  VERIFY(( is_std_format_spec_for<float, false>("0{1}f") ));
  VERIFY( is_std_format_spec_for<float>("1") );
  VERIFY( is_std_format_spec_for<float>("-1") ); // sign and width
  VERIFY( ! is_std_format_spec_for<float>(".") );
  VERIFY( is_std_format_spec_for<float>(".1") );
  VERIFY( is_std_format_spec_for<float>(".{}") );
  VERIFY( ! is_std_format_spec_for<float>(".{1}") );
  VERIFY(( is_std_format_spec_for<float, false>(".{1}") ));
  VERIFY( is_std_format_spec_for<float>("{}.{}") );
  VERIFY(( is_std_format_spec_for<float, false>("{1}.{1}") ));
  VERIFY(( is_std_format_spec_for<float, false>("{2}.{1}") ));
  VERIFY( ! is_std_format_spec_for<float>("c") );
  VERIFY( ! is_std_format_spec_for<float>("b") );
  VERIFY( ! is_std_format_spec_for<float>("B") );
  VERIFY( ! is_std_format_spec_for<float>("d") );
  VERIFY( ! is_std_format_spec_for<float>("o") );
  VERIFY( ! is_std_format_spec_for<float>("x") );
  VERIFY( ! is_std_format_spec_for<float>("X") );
  VERIFY( ! is_std_format_spec_for<float>("s") );
  VERIFY( ! is_std_format_spec_for<float>("?") );
  VERIFY( is_std_format_spec_for<float>("a") );
  VERIFY( is_std_format_spec_for<float>("A") );
  VERIFY( is_std_format_spec_for<float>("f") );
  VERIFY( is_std_format_spec_for<float>("F") );
  VERIFY( is_std_format_spec_for<float>("g") );
  VERIFY( is_std_format_spec_for<float>("G") );
  VERIFY( ! is_std_format_spec_for<float>("p") );
  VERIFY( ! is_std_format_spec_for<float>("P") );
  VERIFY( is_std_format_spec_for<float>("+f") );

  VERIFY( is_std_format_spec_for<float>("_<+#09.6Lf") );
  VERIFY( is_std_format_spec_for<float>("<+#09.6Lf") );
  VERIFY( is_std_format_spec_for<float>("<+#9.6Lf") );
  VERIFY( is_std_format_spec_for<float>(".0006f") );
}

void
test_pointer()
{
  VERIFY( is_std_format_spec_for<void*>("") );
  VERIFY( is_std_format_spec_for<void*>("<") );
  VERIFY( is_std_format_spec_for<void*>(">") );
  VERIFY( is_std_format_spec_for<void*>("^") );
  VERIFY( is_std_format_spec_for<void*>("0<") );
  VERIFY( is_std_format_spec_for<void*>("0>") );
  VERIFY( is_std_format_spec_for<void*>("0^") );
  VERIFY( ! is_std_format_spec_for<void*>("{^") );
  VERIFY( ! is_std_format_spec_for<void*>("+") );
  VERIFY( ! is_std_format_spec_for<void*>("-") );
  VERIFY( ! is_std_format_spec_for<void*>(" ") );
  VERIFY( ! is_std_format_spec_for<void*>("#") );
  VERIFY( is_std_format_spec_for<void*>("1") );
  VERIFY( ! is_std_format_spec_for<void*>("-1") );
  VERIFY( ! is_std_format_spec_for<void*>("-1p") );
  VERIFY( ! is_std_format_spec_for<void*>(".") );
  VERIFY( ! is_std_format_spec_for<void*>(".1") );
  VERIFY( ! is_std_format_spec_for<void*>("c") );
  VERIFY( ! is_std_format_spec_for<void*>("b") );
  VERIFY( ! is_std_format_spec_for<void*>("B") );
  VERIFY( ! is_std_format_spec_for<void*>("d") );
  VERIFY( ! is_std_format_spec_for<void*>("o") );
  VERIFY( ! is_std_format_spec_for<void*>("x") );
  VERIFY( ! is_std_format_spec_for<void*>("X") );
  VERIFY( ! is_std_format_spec_for<void*>("s") );
  VERIFY( ! is_std_format_spec_for<void*>("?") );
  VERIFY( is_std_format_spec_for<void*>("p") );
  VERIFY( ! is_std_format_spec_for<void*>("a") );
  VERIFY( ! is_std_format_spec_for<void*>("A") );
  VERIFY( ! is_std_format_spec_for<void*>("f") );
  VERIFY( ! is_std_format_spec_for<void*>("F") );
  VERIFY( ! is_std_format_spec_for<void*>("g") );
  VERIFY( ! is_std_format_spec_for<void*>("G") );
  VERIFY( ! is_std_format_spec_for<void*>("+p") );

#if __cpp_lib_format >= 202304L
  // P2510R3 Formatting pointers
  VERIFY( is_std_format_spec_for<void*>("P") );
  VERIFY( is_std_format_spec_for<void*>("0p") );
  VERIFY( is_std_format_spec_for<void*>("0P") );
  VERIFY( is_std_format_spec_for<void*>("0") );
  VERIFY( is_std_format_spec_for<void*>("01p") );
  VERIFY( ! is_std_format_spec_for<void*>("00p") );
#endif
}

void
test_string()
{
  VERIFY( is_std_format_spec_for<const char*>("") );
  VERIFY( is_std_format_spec_for<const char*>("<") );
  VERIFY( is_std_format_spec_for<const char*>(">") );
  VERIFY( is_std_format_spec_for<const char*>("^") );
  VERIFY( is_std_format_spec_for<const char*>("0<") );
  VERIFY( is_std_format_spec_for<const char*>("0>") );
  VERIFY( is_std_format_spec_for<const char*>("0^") );
  VERIFY( ! is_std_format_spec_for<const char*>("{^") );
  VERIFY( ! is_std_format_spec_for<const char*>("+") );
  VERIFY( ! is_std_format_spec_for<const char*>("-") );
  VERIFY( ! is_std_format_spec_for<const char*>(" ") );
  VERIFY( ! is_std_format_spec_for<const char*>("#") );
  VERIFY( ! is_std_format_spec_for<const char*>("0") );
  VERIFY( ! is_std_format_spec_for<const char*>("01s") );
  VERIFY( is_std_format_spec_for<const char*>("1") );
  VERIFY( ! is_std_format_spec_for<const char*>("-1") );
  VERIFY( ! is_std_format_spec_for<const char*>("-1s") );
  VERIFY( ! is_std_format_spec_for<const char*>(".") );
  VERIFY( is_std_format_spec_for<const char*>(".1") );
  VERIFY( is_std_format_spec_for<const char*>(".{}") );
  VERIFY(( is_std_format_spec_for<const char*, false>(".{0}") ));
  VERIFY(( is_std_format_spec_for<const char*, false>(".{1}") ));
  VERIFY( ! is_std_format_spec_for<const char*>("c") );
  VERIFY( ! is_std_format_spec_for<const char*>("b") );
  VERIFY( ! is_std_format_spec_for<const char*>("B") );
  VERIFY( ! is_std_format_spec_for<const char*>("d") );
  VERIFY( ! is_std_format_spec_for<const char*>("o") );
  VERIFY( ! is_std_format_spec_for<const char*>("x") );
  VERIFY( ! is_std_format_spec_for<const char*>("X") );
  VERIFY( is_std_format_spec_for<const char*>("s") );
  VERIFY( is_std_format_spec_for<const char*>("?") == escaped_strings_supported );
  VERIFY( ! is_std_format_spec_for<const char*>("p") );
  VERIFY( ! is_std_format_spec_for<const char*>("P") );
  VERIFY( ! is_std_format_spec_for<const char*>("a") );
  VERIFY( ! is_std_format_spec_for<const char*>("A") );
  VERIFY( ! is_std_format_spec_for<const char*>("f") );
  VERIFY( ! is_std_format_spec_for<const char*>("F") );
  VERIFY( ! is_std_format_spec_for<const char*>("g") );
  VERIFY( ! is_std_format_spec_for<const char*>("G") );

  VERIFY( is_std_format_spec_for<const char*>("*^6s") );
  VERIFY( is_std_format_spec_for<const char*>(">6s") );
  VERIFY( is_std_format_spec_for<const char*>("_<6.4?") == escaped_strings_supported );
}

struct S { };

template<>
struct std::formatter<S, char>
{
  constexpr std::format_parse_context::iterator
  parse(std::format_parse_context& pc)
  {
    std::string_view spec(pc.begin(), pc.end());
    auto p = spec.find('}');
    if (p == std::string_view::npos)
      p = spec.size();
    if (p == 0)
      throw std::format_error("empty format-spec");
    if (spec != "custom")
      throw std::format_error("invalid format-spec");
    return pc.begin() + p;
  }

  std::format_context::iterator
  format(const S&, std::format_context&) const;
};

void
test_custom()
{
  VERIFY( is_std_format_spec_for<S>("custom") );
  VERIFY( ! is_std_format_spec_for<S>("customer") );
  VERIFY( ! is_std_format_spec_for<S>("custard") );
  VERIFY( ! is_std_format_spec_for<S>("") );
}

#if __cpp_lib_format >= 202305
struct X { };

template<>
struct std::formatter<X, char>
{
  constexpr std::format_parse_context::iterator
  parse(std::format_parse_context& pc)
  {
    std::string_view spec(pc.begin(), pc.end());
    auto p = spec.find('}');
    if (p != std::string_view::npos)
      spec = spec.substr(0, p); // truncate to closing brace
    if (spec == "int")
    {
      pc.check_dynamic_spec_integral(pc.next_arg_id());
      integer = true;
    }
    else if (spec == "str")
    {
      pc.check_dynamic_spec_string(pc.next_arg_id());
      integer = false;
    }
    else
      throw std::format_error("invalid format-spec");
    return pc.begin() + spec.size();
  }

  std::format_context::iterator
  format(X, std::format_context& c) const
  {
    std::visit_format_arg([this]<typename T>(T) { // { dg-warning "deprecated" "" { target c++26 } }
      if (is_integral_v<T> != this->integer)
	throw std::format_error("invalid argument type");
    }, c.arg(1));
    return c.out();
  }
private:
  bool integer = false;
};
#endif

void
test_dynamic_type_check()
{
#if __cpp_lib_format >= 202305
  std::format_parse_context pc("{1}.{2}");

  // None of these calls should do anything at runtime, only during consteval:
  pc.check_dynamic_spec<int, const char*>(0);
  pc.check_dynamic_spec_integral(0);
  pc.check_dynamic_spec_string(0);

  (void) std::format("{:int}", X{}, 42L);
  (void) std::format("{:str}", X{}, "H2G2");
#endif
}

int main()
{
  test_char();
  test_int();
  test_bool();
  test_float();
  test_string();
  test_pointer();
  test_custom();
  test_dynamic_type_check();
}

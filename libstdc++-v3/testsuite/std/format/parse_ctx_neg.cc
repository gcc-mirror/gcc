// { dg-do compile { target c++26 } }
// { dg-skip-if "" { *-*-* } { "-fno-char8_t" } }

#include <format>

void
test_invalid()
{
  std::format_parse_context pc("");

  // These types are all valid:
  pc.check_dynamic_spec<bool, char, int, unsigned, long long,
			unsigned long long, float, double, long double,
			const char*, std::string_view, const void*>(0);
  // For some reason, an empty pack of types is valid:
  pc.check_dynamic_spec<>(0);

  pc.check_dynamic_spec<void>(0); // { dg-error "here" }
  // const void* is allowed, but void* is not
  pc.check_dynamic_spec<void*>(0); // { dg-error "here" }
  // int and long long are allowed, but long is not
  pc.check_dynamic_spec<long>(0); // { dg-error "here" }
  // char_type is allowed, but other character types are not
  pc.check_dynamic_spec<wchar_t>(0); // { dg-error "here" }
  pc.check_dynamic_spec<char8_t>(0); // { dg-error "here" }
  // std::string_view is allowed, but std::string is not
  pc.check_dynamic_spec<std::string>(0); // { dg-error "here" }
  pc.check_dynamic_spec<int, bool, int>(0); // { dg-error "here" }

  std::wformat_parse_context wpc(L"");
  wpc.check_dynamic_spec<bool, wchar_t, int, unsigned, long long,
			 unsigned long long, float, double, long double,
			 const wchar_t*, std::wstring_view, const void*>(0);
  wpc.check_dynamic_spec<char>(0); // { dg-error "here" }
  wpc.check_dynamic_spec<char16_t>(0); // { dg-error "here" }
  wpc.check_dynamic_spec<char32_t>(0); // { dg-error "here" }
}

// Each failure above will point to a call to this non-constexpr function:
// { dg-error "__invalid_dynamic_spec" "" { target *-*-* } 0 }

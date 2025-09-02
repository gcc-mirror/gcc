// { dg-do compile { target c++20 } }

#include <chrono>
#include <format>

using namespace std::chrono;

auto d1 = std::format("{:%w}", 10d); // { dg-error "call to consteval function" }
auto d2 = std::format("{:%m}", 10d); // { dg-error "call to consteval function" }
auto d3 = std::format("{:%y}", 10d); // { dg-error "call to consteval function" }
auto d4 = std::format("{:%F}", 10d); // { dg-error "call to consteval function" }
auto d5 = std::format("{:%T}", 10d); // { dg-error "call to consteval function" }
auto d6 = std::format("{:%Q}", 10d); // { dg-error "call to consteval function" }
auto d7 = std::format("{:%Z}", 10d); // { dg-error "call to consteval function" }

auto w1 = std::format("{:%d}", Thursday); // { dg-error "call to consteval function" }
auto w2 = std::format("{:%m}", Thursday); // { dg-error "call to consteval function" }
auto w3 = std::format("{:%y}", Thursday); // { dg-error "call to consteval function" }
auto w4 = std::format("{:%F}", Thursday); // { dg-error "call to consteval function" }
auto w5 = std::format("{:%T}", Thursday); // { dg-error "call to consteval function" }
auto w6 = std::format("{:%Q}", Thursday); // { dg-error "call to consteval function" }
auto w7 = std::format("{:%Z}", Thursday); // { dg-error "call to consteval function" }

auto wi1 = std::format("{:%d}", Thursday[2]); // { dg-error "call to consteval function" }
auto wi2 = std::format("{:%m}", Thursday[2]); // { dg-error "call to consteval function" }
auto wi3 = std::format("{:%y}", Thursday[2]); // { dg-error "call to consteval function" }
auto wi4 = std::format("{:%F}", Thursday[2]); // { dg-error "call to consteval function" }
auto wi5 = std::format("{:%T}", Thursday[2]); // { dg-error "call to consteval function" }
auto wi6 = std::format("{:%Q}", Thursday[2]); // { dg-error "call to consteval function" }
auto wi7 = std::format("{:%Z}", Thursday[2]); // { dg-error "call to consteval function" }

auto wl1 = std::format("{:%d}", Thursday[last]); // { dg-error "call to consteval function" }
auto wl2 = std::format("{:%m}", Thursday[last]); // { dg-error "call to consteval function" }
auto wl3 = std::format("{:%y}", Thursday[last]); // { dg-error "call to consteval function" }
auto wl4 = std::format("{:%F}", Thursday[last]); // { dg-error "call to consteval function" }
auto wl5 = std::format("{:%T}", Thursday[last]); // { dg-error "call to consteval function" }
auto wl6 = std::format("{:%Q}", Thursday[last]); // { dg-error "call to consteval function" }
auto wl7 = std::format("{:%Z}", Thursday[last]); // { dg-error "call to consteval function" }

auto m1 = std::format("{:%d}", January); // { dg-error "call to consteval function" }
auto m2 = std::format("{:%w}", January); // { dg-error "call to consteval function" }
auto m3 = std::format("{:%y}", January); // { dg-error "call to consteval function" }
auto m4 = std::format("{:%F}", January); // { dg-error "call to consteval function" }
auto m5 = std::format("{:%T}", January); // { dg-error "call to consteval function" }
auto m6 = std::format("{:%Q}", January); // { dg-error "call to consteval function" }
auto m7 = std::format("{:%Z}", January); // { dg-error "call to consteval function" }

auto yr1 = std::format("{:%d}", 2025y); // { dg-error "call to consteval function" }
auto yr2 = std::format("{:%w}", 2025y); // { dg-error "call to consteval function" }
auto yr3 = std::format("{:%m}", 2025y); // { dg-error "call to consteval function" }
auto yr4 = std::format("{:%F}", 2025y); // { dg-error "call to consteval function" }
auto yr5 = std::format("{:%T}", 2025y); // { dg-error "call to consteval function" }
auto yr6 = std::format("{:%Q}", 2025y); // { dg-error "call to consteval function" }
auto yr7 = std::format("{:%Z}", 2025y); // { dg-error "call to consteval function" }

auto md1 = std::format("{:%w}", January/10d); // { dg-error "call to consteval function" }
auto md2 = std::format("{:%y}", January/10d); // { dg-error "call to consteval function" }
auto md3 = std::format("{:%F}", January/10d); // { dg-error "call to consteval function" }
auto md4 = std::format("{:%T}", January/10d); // { dg-error "call to consteval function" }
auto md5 = std::format("{:%Q}", January/10d); // { dg-error "call to consteval function" }
auto md6 = std::format("{:%Z}", January/10d); // { dg-error "call to consteval function" }

auto mwi1 = std::format("{:%d}", January/Thursday[2]); // { dg-error "call to consteval function" }
auto mwi2 = std::format("{:%y}", January/Thursday[2]); // { dg-error "call to consteval function" }
auto mwi3 = std::format("{:%F}", January/Thursday[2]); // { dg-error "call to consteval function" }
auto mwi4 = std::format("{:%T}", January/Thursday[2]); // { dg-error "call to consteval function" }
auto mwi5 = std::format("{:%Q}", January/Thursday[2]); // { dg-error "call to consteval function" }
auto mwi6 = std::format("{:%Z}", January/Thursday[2]); // { dg-error "call to consteval function" }

auto mwl1 = std::format("{:%d}", January/Thursday[last]); // { dg-error "call to consteval function" }
auto mwl2 = std::format("{:%y}", January/Thursday[last]); // { dg-error "call to consteval function" }
auto mwl3 = std::format("{:%F}", January/Thursday[last]); // { dg-error "call to consteval function" }
auto mwl4 = std::format("{:%T}", January/Thursday[last]); // { dg-error "call to consteval function" }
auto mwl5 = std::format("{:%Q}", January/Thursday[last]); // { dg-error "call to consteval function" }
auto mwl6 = std::format("{:%Z}", January/Thursday[last]); // { dg-error "call to consteval function" }

auto ml1 = std::format("{:%d}", January/last); // { dg-error "call to consteval function" }
auto ml2 = std::format("{:%w}", January/last); // { dg-error "call to consteval function" }
auto ml3 = std::format("{:%y}", January/last); // { dg-error "call to consteval function" }
auto ml4 = std::format("{:%F}", January/last); // { dg-error "call to consteval function" }
auto ml5 = std::format("{:%T}", January/last); // { dg-error "call to consteval function" }
auto ml6 = std::format("{:%Q}", January/last); // { dg-error "call to consteval function" }
auto ml7 = std::format("{:%Z}", January/last); // { dg-error "call to consteval function" }

auto ym1 = std::format("{:%d}", 2024y/March); // { dg-error "call to consteval function" }
auto ym2 = std::format("{:%w}", 2024y/March); // { dg-error "call to consteval function" }
auto ym3 = std::format("{:%F}", 2024y/March); // { dg-error "call to consteval function" }
auto ym4 = std::format("{:%T}", 2024y/March); // { dg-error "call to consteval function" }
auto ym5 = std::format("{:%Q}", 2024y/March); // { dg-error "call to consteval function" }
auto ym6 = std::format("{:%Z}", 2024y/March); // { dg-error "call to consteval function" }

auto ymd1 = std::format("{:%T}", 2021y/January/10d); // { dg-error "call to consteval function" }
auto ymd2 = std::format("{:%Q}", 2021y/January/10d); // { dg-error "call to consteval function" }
auto ymd3 = std::format("{:%Z}", 2021y/January/10d); // { dg-error "call to consteval function" }

auto ymwi1 = std::format("{:%T}", 2021y/January/Thursday[2]); // { dg-error "call to consteval function" }
auto ymwi2 = std::format("{:%Q}", 2021y/January/Thursday[2]); // { dg-error "call to consteval function" }
auto ymwi3 = std::format("{:%Z}", 2021y/January/Thursday[2]); // { dg-error "call to consteval function" }

auto ymwl1 = std::format("{:%T}", 2021y/January/Thursday[last]); // { dg-error "call to consteval function" }
auto ymwl2 = std::format("{:%Q}", 2021y/January/Thursday[last]); // { dg-error "call to consteval function" }
auto ymwl3 = std::format("{:%Z}", 2021y/January/Thursday[last]); // { dg-error "call to consteval function" }

auto yml1 = std::format("{:%T}", 2021y/January/last); // { dg-error "call to consteval function" }
auto yml2 = std::format("{:%Q}", 2021y/January/last); // { dg-error "call to consteval function" }
auto yml3 = std::format("{:%Z}", 2021y/January/last); // { dg-error "call to consteval function" }

auto ls1 = std::format("{:%Q}", local_seconds(20s)); // { dg-error "call to consteval function" }
auto ls2 = std::format("{:%Z}", local_seconds(10s)); // { dg-error "call to consteval function" }
auto ld1 = std::format("{:%Q}", local_days(days(20))); // { dg-error "call to consteval function" }
auto ld2 = std::format("{:%Z}", local_days(days(10))); // { dg-error "call to consteval function" }

auto ss1 = std::format("{:%Q}", sys_seconds(20s)); // { dg-error "call to consteval function" }
auto sd1 = std::format("{:%Q}", sys_days(days(20))); // { dg-error "call to consteval function" }

auto utc = std::format("{:%Q}", utc_clock::now()); // { dg-error "call to consteval function" }
auto gps = std::format("{:%Q}", gps_clock::now()); // { dg-error "call to consteval function" }
auto tai = std::format("{:%Q}", tai_clock::now()); // { dg-error "call to consteval function" }
auto file = std::format("{:%Q}", file_clock::now()); // { dg-error "call to consteval function" }

const auto ltc = local_seconds(10s);
#if _GLIBCXX_USE_CXX11_ABI
const auto zt = zoned_time<seconds>("Europe/Sofia", local_seconds(10s));
auto zt1 = std::format("{:%Q}", zt); // { dg-error "call to consteval function" "" { target cxx11_abi } }
#endif
auto lf1 = std::format("{:%Q}", local_time_format(ltc)); // { dg-error "call to consteval function" }

auto dur1 = std::format("{:%d}", 123s); // { dg-error "call to consteval function" }
auto dur2 = std::format("{:%w}", 123s); // { dg-error "call to consteval function" }
auto dur3 = std::format("{:%m}", 123s); // { dg-error "call to consteval function" }
auto dur4 = std::format("{:%y}", 123s); // { dg-error "call to consteval function" }
auto dur5 = std::format("{:%F}", 123s); // { dg-error "call to consteval function" }
auto dur6 = std::format("{:%Z}", 123s); // { dg-error "call to consteval function" }

using HMS = hh_mm_ss<seconds>;
auto hms1 = std::format("{:%d}", HMS(1255s)); // { dg-error "call to consteval function" }
auto hms2 = std::format("{:%w}", HMS(1255s)); // { dg-error "call to consteval function" }
auto hms3 = std::format("{:%m}", HMS(1255s)); // { dg-error "call to consteval function" }
auto hms4 = std::format("{:%y}", HMS(1255s)); // { dg-error "call to consteval function" }
auto hms5 = std::format("{:%F}", HMS(1255s)); // { dg-error "call to consteval function" }
auto hms6 = std::format("{:%Q}", HMS(1255s)); // { dg-error "call to consteval function" }
auto hms7 = std::format("{:%Z}", HMS(1255s)); // { dg-error "call to consteval function" }

#if _GLIBCXX_USE_CXX11_ABI
auto li1 = std::format("{:%d}", local_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto li2 = std::format("{:%w}", local_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto li3 = std::format("{:%m}", local_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto li4 = std::format("{:%y}", local_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto li5 = std::format("{:%F}", local_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto li6 = std::format("{:%T}", local_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto li7 = std::format("{:%Q}", local_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto li8 = std::format("{:%Z}", local_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }

auto si1 = std::format("{:%d}", sys_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto si2 = std::format("{:%w}", sys_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto si3 = std::format("{:%m}", sys_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto si4 = std::format("{:%y}", sys_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto si5 = std::format("{:%F}", sys_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto si6 = std::format("{:%T}", sys_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto si7 = std::format("{:%Q}", sys_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
auto si8 = std::format("{:%Z}", sys_info()); // { dg-error "call to consteval function" "" { target cxx11_abi } }
#endif

// { dg-error "call to non-'constexpr' function" "" { target *-*-* } 0 }

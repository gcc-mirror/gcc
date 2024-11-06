// { dg-do compile { target c++17 } }

#include <filesystem>
#include <string>

int main()
{
  std::filesystem::path p = "/var/log/";
  std::string s = "file";
  // Concatenation of strings and string views (P2591R5)
  // should not make this possible:
  p + s; // { dg-error "no match for" "operator+(string,string_view) should not make this possible" }
}

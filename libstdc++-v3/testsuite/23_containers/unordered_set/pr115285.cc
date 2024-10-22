// { dg-do run { target c++11 } }

// libstdc++/115285

#include <string>
#include <unordered_set>

#include <testsuite_hooks.h>

class TrimmedStr : public std::string
{
  static std::string trim_str(std::string const &str)
  {
    auto start = str.find_first_not_of(" \r\n\t");

    return start == std::string::npos
      ? str
      : str.substr(start, str.find_last_not_of(" \r\n\t") - start + 1);
  }

public:
  TrimmedStr(std::string const &arg)
    : std::string{trim_str(arg)} {}
  TrimmedStr(char const *arg)
    : TrimmedStr{std::string{arg}} {}
};

int main()
{
  std::unordered_set<TrimmedStr, std::hash<std::string>, std::equal_to<std::string>>
    set_from_initializer_list{ "foo", "bar", " foo ", " bar " };

  VERIFY( set_from_initializer_list.size() == 2 );

  std::vector<std::string> args{ "foo", "bar", " foo ", " bar " };
  std::unordered_set<TrimmedStr, std::hash<std::string>, std::equal_to<std::string>>
    set_from_iterators;
  set_from_iterators.insert(args.begin(), args.end());
  VERIFY( set_from_iterators.size() == 2 );
}

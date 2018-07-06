// { dg-options "-D_GLIBCXX_CONCEPT_CHECKS" }
// { dg-do run { target c++11 } }

#include <iterator>
#include <utility>
#include <sstream>
#include <string>
#include <testsuite_hooks.h>

template<typename Distance, typename InputRange>
std::pair<std::istream_iterator<char>, std::istream_iterator<char>>
drop(Distance n, InputRange& rng)
{
  return std::make_pair(std::next(std::istream_iterator<char>(rng), n),
			std::istream_iterator<char>()
			);
}

int main()
{
    std::stringstream x("let let there be rock");
    x << std::noskipws;
    auto y = drop(4, x);
    std::string z(y.first, y.second);
    VERIFY(z == "let there be rock");
}

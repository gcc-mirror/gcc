// { dg-do run { target c++11 } }

#include <testsuite_performance.h>

#include <sstream>
#include <string>
#include <vector>
#include <unordered_set>

namespace
{
  const int sz = 2000000;
  const std::string pattern = "long enough test string #";
  const int nb_copies = 2;

  // Perfect std::string hasher knowing how string instances have been
  // generated. It is not tag as slow so that hash code is not cached.
  // It is easier to show impact of hint in this context.
  struct hasher
  {
    static int nb_calls;

    std::size_t
    operator()(const std::string& str) const noexcept
    {
      ++nb_calls;
      std::hash<std::string> std_hasher;
      auto hash = std_hasher(pattern);
      std::istringstream isstr(str.substr(pattern.length()));
      int idx = -1;
      isstr >> idx;
      if (idx != -1)
	return (std::size_t)(hash / sz) * sz + idx;

      return hash;
    }
  };

  // Like previous hasher but tagged as slow.
  struct slow_hasher
  {
    std::size_t
    operator()(const std::string& str) const noexcept
    { return hasher{}(str); }
  };

  int hasher::nb_calls = 0;

  template<typename _Hash>
    using us_t = std::unordered_set<std::string, _Hash>;

  template<typename _Hash>
    void
    insert_once_individually(const std::vector<std::string>& strs,
			     us_t<_Hash>& s)
    {
      for (std::size_t i = 0; i != strs.size(); ++i)
	s.insert(strs[i]);
    }

  template<typename _Hash>
    void
    insert_once_range(const std::vector<std::string>& strs,
		      us_t<_Hash>& s)
    {
      s.insert("initial string to not leave s empty");
      s.insert(strs.begin(), strs.end());
    }

  template<typename _Hash>
    void
    insert_individually(const std::vector<std::string>& strs,
			us_t<_Hash>& s)
    {
      for (int j = 1; j != nb_copies; ++j)
	for (std::size_t i = 0; i != strs.size(); ++i)
	  s.insert(strs[i]);
    }

  template<typename _Hash>
    void
    insert_range(const std::vector<std::string>& strs,
		 us_t<_Hash>& s)
    {
      s.insert("initial string to not leave s empty");
      for (int j = 1; j != nb_copies; ++j)
	s.insert(strs.begin(), strs.end());
    }
}

template<typename _Hash>
  void bench(const char* ctx)
  {
    using namespace __gnu_test;

    const int nb_iter = 10;

    std::vector<std::string> strs;
    strs.reserve(sz / nb_copies);

    for (int i = 0; i != sz / nb_copies; ++i)
      {
	std::ostringstream osstr;
	osstr << pattern << i;
	strs.push_back(osstr.str());
      }

    us_t<_Hash> s;
    s.reserve(sz / nb_copies);

    // Warm up.
    {
      for (auto str : strs)
	for (int j = 0; j != nb_copies; ++j)
	  s.insert(str);
    }

    int nb_calls_once_individually = 0;
    int nb_calls_once_range = 0;
    int nb_calls_individually = 0;
    int nb_calls_range = 0;
    time_counter time_once_individually, time_once_range;
    time_counter time_individually, time_range;
    resource_counter resource_once_individually, resource_once_range;
    resource_counter resource_individually, resource_range;

    for (int i = 0; i != nb_iter; ++i)
      {
	{
	  hasher::nb_calls = 0;
	  s.clear();
	  start_counters(time_once_individually, resource_once_individually);
	  insert_once_individually(strs, s);
	  stop_counters(time_once_individually, resource_once_individually);
	  nb_calls_once_individually += hasher::nb_calls;
	}

	{
	  hasher::nb_calls = 0;
	  s.clear();
	  start_counters(time_once_range, resource_once_range);
	  insert_once_range(strs, s);
	  stop_counters(time_once_range, resource_once_range);
	  nb_calls_once_range += hasher::nb_calls;
	}

	{
	  hasher::nb_calls = 0;
	  s.clear();
	  start_counters(time_individually, resource_individually);
	  insert_individually(strs, s);
	  stop_counters(time_individually, resource_individually);
	  nb_calls_individually += hasher::nb_calls;
	}

	{
	  hasher::nb_calls = 0;
	  s.clear();
	  start_counters(time_range, resource_range);
	  insert_range(strs, s);
	  stop_counters(time_range, resource_range);
	  nb_calls_range += hasher::nb_calls;
	}
      }

    std::ostringstream ostr;
    ostr << ctx << ' '
	 << nb_copies << " X " << sz / nb_copies << " inserts individually";
    if (nb_calls_individually != 0)
      ostr << ' ' << nb_calls_individually << " calls";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_individually, resource_individually);

    ostr.str("");
    ostr << ctx << ' '
	 << nb_copies << " X " << sz / nb_copies << " inserts in range";
    if (nb_calls_range)
      ostr << ' ' << nb_calls_range << " calls";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_range, resource_range);

    ostr.str("");
    ostr << ctx << ' '
	 << sz / nb_copies << " X inserts individually";
    if (nb_calls_once_individually != 0)
      ostr << ' ' << nb_calls_once_individually << " calls";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_once_individually, resource_once_individually);

    ostr.str("");
    ostr << ctx << ' '
	 << sz / nb_copies << " X inserts in range";
    if (nb_calls_once_range != 0)
      ostr << ' ' << nb_calls_once_range << " calls";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_once_range, resource_once_range);
  }

namespace std
{
  template<>
    struct __is_fast_hash<slow_hasher> : std::false_type
    { };
}

int main()
{
  bench<hasher>("hash code NOT cached");
  bench<slow_hasher>("hash code cached");
  return 0;
}

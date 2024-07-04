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

  // Perfect std::string hasher knowing how string instances have been
  // generated. It is not tag as slow so that hash code is not cached.
  // It is easier to show impact of hint in this context.
  struct hasher
  {
    std::size_t
    operator()(const std::string& str) const noexcept
    {
      std::hash<std::string> std_hasher;
      auto hash = std_hasher(pattern);
      std::istringstream isstr(str.substr(pattern.length()));
      int idx;
      isstr >> idx;
      return (std::size_t)(hash / sz) * sz + idx;
    }
  };

  // Like previous hasher but tagged as slow.
  struct slow_hasher
  {
    std::size_t
    operator()(const std::string& str) const noexcept
    { return hasher{}(str); }
  };

  template<typename _Hash>
    using us_t = std::unordered_set<std::string, _Hash>;

  template<typename _Hash>
    void
    insert_with_perfect_hint(const std::vector<std::string>& strs,
			     us_t<_Hash>& s)
    {
      auto hint = s.end();
      for (auto str : strs)
	{
	  auto insert_pos = s.insert(hint, str);
	  if (std::next(insert_pos) == s.end())
	    hint = insert_pos;
	}
    }

  template<typename _Hash>
    void
    insert_with_bad_hint(const std::vector<std::string>& strs,
			 us_t<_Hash>& s)
    {
      auto hint = s.begin();
      for (auto str : strs)
	{
	  auto insert_pos = s.insert(hint, str);
	  if (std::next(insert_pos) == hint)
	    hint = s.begin();
	}
    }

  template<typename _Hash>
    void
    insert_without_hint(const std::vector<std::string>& strs,
			us_t<_Hash>& s)
    {
      for (auto str : strs)
	s.insert(str);
    }

  template<typename _Hash>
    void
    insert_range(const std::vector<std::string>& strs,
		 us_t<_Hash>& s)
    { s.insert(strs.begin(), strs.end()); }
}

template<typename _Hash>
  void bench(const char* ctx)
  {
    using namespace __gnu_test;

    const int nb_iter = 10;

    std::vector<std::string> strs;
    strs.reserve(sz);

    for (int i = 0; i != sz; ++i)
      {
	std::ostringstream osstr;
	osstr << pattern << i;
	strs.push_back(osstr.str());
      }

    us_t<_Hash> s;
    s.reserve(sz);

    // Warm up.
    {
      for (auto str : strs)
	s.insert(str);
    }

    time_counter time_no_hint, time_bad_hint, time_perfect_hint,
      time_range;
    resource_counter resource_no_hint, resource_bad_hint, resource_perfect_hint,
      resource_range;

    for (int i = 0; i != nb_iter; ++i)
      {
	// Bad hint
	{
	  s.clear();
	  start_counters(time_bad_hint, resource_bad_hint);
	  insert_with_bad_hint(strs, s);
	  stop_counters(time_bad_hint, resource_bad_hint);
	}

	// No hint
	{
	  s.clear();
	  start_counters(time_no_hint, resource_no_hint);
	  insert_without_hint(strs, s);
	  stop_counters(time_no_hint, resource_no_hint);
	}

	// Perfect hint
	{
	  s.clear();
	  start_counters(time_perfect_hint, resource_perfect_hint);
	  insert_with_perfect_hint(strs, s);
	  stop_counters(time_perfect_hint, resource_perfect_hint);
	}

	// Range insert
	{
	  s.clear();
	  start_counters(time_range, resource_range);
	  insert_range(strs, s);
	  stop_counters(time_range, resource_range);
	}
      }

    std::ostringstream ostr;
    ostr << ctx << ' ' << sz << " insertions w/o hint";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_no_hint, resource_no_hint);

    ostr.str("");
    ostr << ctx << ' ' << sz << " insertions with perfect hint";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_perfect_hint, resource_perfect_hint);

    ostr.str("");
    ostr << ctx << ' ' << sz << " insertions with bad hint";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_bad_hint, resource_bad_hint);

    ostr.str("");
    ostr << ctx << ' ' << sz << " range insertions";
    report_performance(__FILE__, ostr.str().c_str(),
		       time_range, resource_range);
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

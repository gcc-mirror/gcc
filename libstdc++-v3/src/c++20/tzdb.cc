// chrono::tzdb -*- C++ -*-

// Copyright The GNU Toolchain Authors.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

// The -Wabi warnings in this file are all for non-exported symbols.
#pragma GCC diagnostic ignored "-Wabi"

// In the usual dual-abi build, std::chrono::tzdb is only defined for cxx11.
#define _GLIBCXX_USE_CXX11_ABI 1

#include <chrono>
#include <fstream>    // ifstream
#include <sstream>    // istringstream
#include <algorithm>  // ranges::upper_bound, ranges::lower_bound, ranges::sort
#include <atomic>     // atomic<T*>, atomic<int>
#include <memory>     // atomic<shared_ptr<T>>
#include <mutex>      // mutex
#include <iomanip>    // quoted
#include <span>
#if defined __GTHREADS && ! defined _GLIBCXX_HAS_GTHREADS
# include <ext/concurrence.h> // __gnu_cxx::__mutex
#endif

#ifdef _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h> // _XOPEN_VERSION
#endif
#if defined _GLIBCXX_USE_REALPATH && _XOPEN_VERSION >= 700
# include <stdlib.h>   // malloc, free, realpath
#else
# include <filesystem> // filesystem::canonicalize
#endif

#ifdef _AIX
# include <cstdlib>   // getenv
#endif

#if _GLIBCXX_HAVE_WINDOWS_H
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
# include <psapi.h>

# include <array>
#endif

#if defined __GTHREADS && ATOMIC_POINTER_LOCK_FREE == 2
# define USE_ATOMIC_LIST_HEAD 1
// TODO benchmark atomic<shared_ptr<>> vs mutex.
# define USE_ATOMIC_SHARED_PTR 1
#else
# define USE_ATOMIC_LIST_HEAD 0
# define USE_ATOMIC_SHARED_PTR 0
#endif

#if USE_ATOMIC_SHARED_PTR && ! USE_ATOMIC_LIST_HEAD
// Cannot use atomic<shared_ptr<T>> without lock-free atomic<T*>.
# error Unsupported combination
#endif

#if ! __cpp_constinit
# if __has_cpp_attribute(clang::require_constant_initialization)
#  define constinit [[clang::require_constant_initialization]]
# else // YOLO
#  define constinit
# endif
#endif

namespace __gnu_cxx
{
#ifdef _AIX
  // Cannot override weak symbols on AIX.
  const char* (*zoneinfo_dir_override)() = nullptr;
#else
  [[gnu::weak]] const char* zoneinfo_dir_override();

#if defined(__APPLE__) || defined(__hpux__) \
  || (defined(__VXWORKS__) && !defined(__RTP__))
  // Need a weak definition for Mach-O et al.
  [[gnu::weak]] const char* zoneinfo_dir_override()
  {
#ifdef _GLIBCXX_ZONEINFO_DIR
    return _GLIBCXX_ZONEINFO_DIR;
#else
    return nullptr;
#endif
  }
#endif
#endif
}

#if ! defined _GLIBCXX_ZONEINFO_DIR && ! defined _GLIBCXX_STATIC_TZDATA
# define TZDB_DISABLED
  [[noreturn]] void __throw_disabled()
  {
    std::__throw_runtime_error("tzdb: support for loading tzdata is disabled");
  }
#endif

namespace std::chrono
{
  namespace
  {
#ifndef __GTHREADS
    // Dummy no-op mutex type for single-threaded targets.
    struct mutex { void lock() { } void unlock() { } };
#elif ! defined _GLIBCXX_HAS_GTHREADS
    // Use __gnu_cxx::__mutex if std::mutex isn't available.
    using mutex = __gnu_cxx::__mutex;
# if ! USE_ATOMIC_SHARED_PTR && defined __GTHREAD_MUTEX_INIT
#  error "TODO: __gnu_cxx::__mutex can't be initialized with 'constinit'"
# endif
#endif

#if ! USE_ATOMIC_SHARED_PTR
    inline mutex& list_mutex()
    {
#ifdef __GTHREAD_MUTEX_INIT
      constinit static mutex m;
#else
      // Cannot use a constinit mutex, so use a local static.
      alignas(mutex) constinit static char buf[sizeof(mutex)];
      static mutex& m = *::new(buf) mutex();
#endif
      return m;
    }
#endif // ! USE_ATOMIC_SHARED_PTR

    struct Rule;
  }

  // The tzdb list is a singly-linked list of _Node objects, using shared_ptr
  // for the links. Iterators into the list share ownership of the nodes.
  // Each _Node contains a tzdb and a vector<Rule> with the rule set.
  struct tzdb_list::_Node
  {
    shared_ptr<_Node> next;
    tzdb db;
#ifndef TZDB_DISABLED
    vector<Rule> rules;
#endif

    // The following static members are here because making them members
    // of this type gives them access to the private members of time_zone
    // and tzdb, without needing them declared in the <chrono> header.

    // The tzdb_list singleton. This doesn't contain the actual linked list,
    // but it has member functions that give access to it.
    static tzdb_list _S_the_list;

#if USE_ATOMIC_SHARED_PTR
    using head_ptr = atomic<shared_ptr<_Node>>;
#else
    // Non-atomic, list_mutex must be locked to access it.
    using head_ptr = shared_ptr<_Node>;
#endif
    // This is the owning reference to the first tzdb in the list.
    static head_ptr _S_head_owner;

#if USE_ATOMIC_LIST_HEAD
    // Lock-free access to the head of the list.
    static atomic<_Node*> _S_head_cache;

    static _Node*
    _S_list_head(memory_order ord) noexcept
    { return _S_head_cache.load(ord); }

    static void
    _S_cache_list_head(_Node* new_head) noexcept
    { _S_head_cache = new_head; }
#else
    static _Node*
    _S_list_head(memory_order)
    {
      lock_guard<mutex> l(list_mutex());
      return _S_head_owner.get();
    }

    static void
    _S_cache_list_head(_Node*) noexcept
    { }
#endif

    static const tzdb& _S_init_tzdb();
    static const tzdb& _S_replace_head(shared_ptr<_Node>, shared_ptr<_Node>);

    static pair<vector<leap_second>, bool> _S_read_leap_seconds();

    // This is here because _Node is a friend so can call private constructor.
    static const leap_second fixed_leaps[];

    // This is a member so that it can access fixed_leaps.
    struct NumLeapSeconds;
    static NumLeapSeconds num_leap_seconds;
  };

  // Implementation of the private constructor used for the singleton object.
  constexpr tzdb_list::tzdb_list(nullptr_t) { }

#pragma GCC diagnostic ignored "-Wprio-ctor-dtor"

  [[gnu::init_priority(98)]]
  constinit tzdb_list tzdb_list::_Node::_S_the_list(nullptr);

  [[gnu::init_priority(98)]]
  constinit tzdb_list::_Node::head_ptr tzdb_list::_Node::_S_head_owner{nullptr};

#if USE_ATOMIC_LIST_HEAD
  [[gnu::init_priority(98)]]
  constinit atomic<tzdb_list::_Node*> tzdb_list::_Node::_S_head_cache{nullptr};
#endif

  // The data structures defined in this file (Rule, on_month_day, at_time etc.)
  // are used to represent the information parsed from the tzdata.zi file
  // described at https://man7.org/linux/man-pages/man8/zic.8.html#FILES

  // N.B. Most stream extraction operations for time zones, rules etc.
  // assume that setting failbit will throw an exception, so individual
  // input operations are not always checked for success.

#ifndef TZDB_DISABLED
  namespace
  {
    // Used for reading a possibly-quoted string from a stream.
    struct quoted
    {
      string& str;

      friend istream& operator>>(istream& in, quoted&& q)
      {
	if (ws(in).peek() == '"')
	  in >> std::quoted(q.str);
	else
	  in >> q.str;
	return in;
      }
    };

    // 32-bit version of chrono::seconds for offsets in the range [-24h,24h].
    // Care must be taken to avoid overflow when using this in arithmetic.
    // For example, if sys_days::rep is also 32-bit then the result of
    // sys_days(1850y/January/1) + sec32_t(0) will be incorrect.
    using sec32_t = duration<int_least32_t>;

    // A time relative to midnight, as measured by the indicator clock.
    struct at_time
    {
      sec32_t time{};
      enum Indicator : unsigned char { Wall, Universal, Standard, Daylight };
      Indicator indicator = Wall;

      static pair<Indicator, bool> is_indicator(int c) noexcept
      {
	switch (c)
	{
	  case 's':
	  case 'S':
	    return {Standard, true};
	  case 'u':
	  case 'U':
	  case 'g':
	  case 'G':
	  case 'z':
	  case 'Z':
	    return {Universal, true};
	  case 'w':
	  case 'W':
	    return {Wall, true};
	  case 'd':
	  case 'D':
	    return {Daylight, true};
	  default:
	    return {Wall, false};
	}
      }

      // Checks if the next character in the stream is an indicator.
      // If not, indic is unchanged. Callers should set a default first.
      friend istream& operator>>(istream& in, Indicator& indic)
      {
	if (!in.eof())
	  {
	    auto [val, yes] = at_time::is_indicator(in.peek());
	    if (yes)
	      {
		in.ignore(1);
		indic = val;
	      }
	  }
	return in;
      }

      friend istream& operator>>(istream& in, at_time& at);
    };

    // Wrapper for chrono::month that can be extracted from an istream
    // as an abbreviated month name.
    // The month name can be any unambiguous portion of a month name,
    // e.g. "S" (September) or "Ja" (January), but not "Ju" (June/July).
    struct abbrev_month
    {
      month m;

      friend istream& operator>>(istream& in, abbrev_month& am);
    };

    // The IN and ON fields of a RULE record, e.g. "March lastSunday".
    struct on_month_day
    {
      using rep = uint_least16_t;
      // Equivalent to Kind, chrono::month, chrono::day, chrono::weekday,
      // but half the size.
      enum Kind : rep { DayOfMonth=0, LastWeekday, LessEq, GreaterEq };
      Kind kind : 2 = DayOfMonth;
      rep month : 4 = 0;        // [1,12]
      rep day_of_month : 5 = 0; // [1,31]
      rep day_of_week : 3 = 0;  // [0,6]

      chrono::month
      get_month() const noexcept
      { return chrono::month{month}; }

      chrono::day
      get_day() const noexcept
      { return chrono::day{day_of_month}; }

      chrono::month_day
      get_month_day() const noexcept
      { return chrono::month_day{get_month(), get_day()}; }

      bool ok() const noexcept
      {
	switch (kind)
	{
	case DayOfMonth:
	  return day_of_month != 0;
	case LastWeekday:
	  return day_of_week != 7 && day_of_month == 0;
	case LessEq:
	case GreaterEq:
	  return day_of_week != 7 && day_of_month != 0;
	}
      }

      // Convert date like "last Sunday in June" or "Sunday <= June 30"
      // to a specific date in the given year.
      year_month_day pin(year y) const
      {
	year_month_day ymd;
	if (kind == LastWeekday) // Last Sunday in June
	  {
	    month_weekday_last mwdl{get_month(),
				    weekday_last{weekday{day_of_week}}};
	    ymd = year_month_day{sys_days{y/mwdl}};
	  }
	else if (kind != DayOfMonth) // Sunday <= June 30 or Sunday >= June 30
	  {
	    sys_days date{y/get_month_day()};
	    days diff;
	    if (kind == LessEq)
	      diff = -(weekday{date} - weekday{day_of_week});
	    else
	      diff = weekday{day_of_week} - weekday{date};
	    // XXX need to handle underflow/overflow to another year?
	    ymd = year_month_day{date + diff};
	  }
	else
	  ymd = year_month_day{y, get_month(), get_day()};
	return ymd;
      }


      friend istream& operator>>(istream&, on_month_day&);
    };

    // Wrapper for two chrono::year values, which reads the FROM and TO
    // fields of a Rule line. The FROM field is a year and TO is a year or
    // one of the keywords "maximum" or "only" (or an abbreviation of those).
    // For backwards compatibility, the keyword "minimum" is recognized
    // for FROM and interpreted as 1900.
    struct years_from_to
    {
      year& from;
      year& to;

      friend istream& operator>>(istream& in, years_from_to&& yy)
      {
	string s;
	auto c = ws(in).peek();
	if (c == 'm' || c == 'M') [[unlikely]] // keyword "minimum"
	  {
	    in >> s; // extract the rest of the word
	    yy.from = year(1900);
	  }
	else if (int num = 0; in >> num) [[likely]]
	  yy.from = year{num};

	c = ws(in).peek();
	if (c == 'm' || c == 'M') // keyword "maximum"
	  {
	    in >> s; // extract the rest of the word
	    yy.to = year::max();
	  }
	else if (c == 'o' || c == 'O') // keyword "only"
	  {
	    in >> s; // extract the rest of the word
	    yy.to = yy.from;
	  }
	else if (int num = 0; in >> num)
	  yy.to = year{num};

	return in;
      }
    };

    bool
    select_std_or_dst_abbrev(string& abbrev, minutes save)
    {
      if (size_t pos = abbrev.find('/'); pos != string::npos)
	{
	  // Select one of "STD/DST" for standard or daylight.
	  if (save == 0min)
	    abbrev.erase(pos);
	  else
	    abbrev.erase(0, pos + 1);
	  return true;
	}
      return false;
    }

    // Set the sys_info::abbrev string by expanding any placeholders.
    void
    format_abbrev_str(sys_info& info, string_view letters = {})
    {
      if (size_t pos = info.abbrev.find('%'); pos != string::npos)
	{
	  if (info.abbrev[pos + 1] == 's')
	    {
	      // Expand "%s" to the variable part, given by Rule::letters.
	      if (letters == "-")
		info.abbrev.erase(pos, 2);
	      else
		info.abbrev.replace(pos, 2, letters);
	    }
	  else if (info.abbrev[pos + 1] == 'z')
	    {
	      // Expand "%z" to the UT offset as +/-hh, +/-hhmm, or +/-hhmmss.
	      hh_mm_ss<seconds> t(info.offset);
	      string z(1, "+-"[t.is_negative()]);
	      long val = t.hours().count();
	      int digits = 2;
	      if (int m = t.minutes().count())
		{
		  digits = 4;
		  val *= 100;
		  val += m;
		  if (int s = t.seconds().count())
		    {
		      digits = 6;
		      val *= 100;
		      val += s;
		    }
		}
	      auto sval = std::to_string(val);
	      z += string(digits - sval.size(), '0');
	      z += sval;
	      info.abbrev.replace(pos, 2, z);
	    }
	}
      else
	select_std_or_dst_abbrev(info.abbrev, info.save);
    }

    struct Rule;

    // A time zone information record.
    // Zone  NAME        STDOFF  RULES   FORMAT  [UNTIL]
    // Zone  Asia/Amman  2:00    Jordan  EE%sT   2017 Oct 27 01:00
    // Will be lazily expanded into sys_info objects as needed.
    struct ZoneInfo
    {
      ZoneInfo() = default;

      ZoneInfo(sys_info&& info)
      : m_buf(std::move(info.abbrev)), m_state(Expanded), m_save(info.save),
	m_offset(info.offset - seconds(info.save)), m_until(info.end)
      { }

      ZoneInfo(const pair<sys_info, string_view>& info)
      : m_state(Expanded), m_save(info.first.save),
	m_offset(info.first.offset - seconds(info.first.save)),
       	m_until(info.first.end)
      {
	if (info.second.size())
	  {
	    m_buf = info.second; // LETTERS field
	    m_buf += ' ';
	    m_pos = m_buf.size();
	  }
	m_buf += info.first.abbrev;
      }

      // STDOFF: Seconds from UTC during standard time (without any save).
      seconds
      offset() const noexcept { return m_offset; }

      // RULES: The name of the rules that apply for this period.
      string_view
      rules() const noexcept
      {
	string_view r;
	if (m_pos != 0)
	  r = {m_buf.data(), m_pos - 1u};
	return r;
      }

      // FORMAT: The name of the time zone (might contain %s or %z).
      string_view
      format() const noexcept
      { return {m_buf.data() + m_pos, m_buf.size() - m_pos}; }

      // UNTIL: The time when this info no longer applies.
      sys_seconds
      until() const noexcept { return m_until; }

      // For non-expanded zone (using rules) computes the value
      // of 'save' at the time of transitions, caches it in m_save.
      // Returns true if until value was changed in the process.
      bool
      calc_save(span<const Rule> all_rules) noexcept;

      // save value at the transition boundary, usable if expanded()
      // is true or after calc_save() was called.
      seconds
      save() const noexcept { return m_save; }

      friend istream& operator>>(istream&, ZoneInfo&);

      bool
      expanded() const noexcept
      { return m_state == Expanded; }

      // For an expanded ZoneInfo this returns the LETTERS that apply to the
      // **next** sys_info after this one.
      string_view
      next_letters() const noexcept
      { return (m_state == Expanded) ? rules() : string_view{}; }


      bool
      to(sys_info& info) const
      {
	// If this object references a named Rule then we can't populate
	// a sys_info yet.
	if (m_state != Expanded)
	  return false;

	info.end = until();
	info.offset = offset() + seconds(m_save);
	info.save = minutes(m_save);
	info.abbrev = format();
	format_abbrev_str(info); // expand %z
	return true;
      }

    private:
      friend class time_zone;

      enum class State : uint_least16_t
      { Expanded, SaveKnown, SavePending, UntilPending };
      using enum State;

      void
      set_abbrev(string abbrev)
      {
	m_buf = std::move(abbrev);
	m_pos = 0;
	m_state = Expanded;
      }

      void
      set_rules_and_format(const string& rls, const string& fmt)
      {
	// Both strings are typically short enough to fit in one SSO string.
	// As of tzdata 2022f the maximum is 14 chars, e.g. "AU +0845/+0945".
	m_buf.reserve(rls.size() + 1 + fmt.size());
	m_buf = rls;
	m_buf += ' ';
	m_buf += fmt;
	m_pos = rls.size() + 1;
      }

      string m_buf;     // rules() + ' ' + format() OR letters + ' ' + format()
      uint_least16_t m_pos : 14 = 0; // offset of format() in m_buf
      State m_state : 2 = SavePending;
      duration<int_least16_t, ratio<60>> m_save{};
      sec32_t m_offset{};
      sys_seconds m_until{};

#if 0
      // Consider making this class more compact, e.g.
      int_least64_t offset_seconds : 18;
      int_least64_t until_sys_seconds : 46;
      uint_least32_t save_minutes : 12;
      uint_least32_t pos : 20;
      string buf; // abbrev OR "rules format"
#endif
    };

    // A RULE record from the tzdata.zi timezone info file.
    struct Rule
    {
      // This allows on_month_day to reuse padding of at_time.
      // This keeps the size to 8 bytes and the alignment to 4 bytes.
      struct datetime : at_time { on_month_day day; };

      // TODO combining name+letters into a single string (like in ZoneInfo)
      // would save sizeof(string) and make Rule fit in a single cacheline.
      // Or don't store name in the Rule, and replace vector<Rule> with
      // vector<pair<string,vector<Rule>>> i.e. map-like structure.

      string name;    // the name of the rule set
      year from{};    // first year in which the rule applies
      year to{};      // final year in which the rule applies
      datetime when;  // the day and time on which the rule takes effect
      sec32_t save{}; // amount of time to be added when the rule is in effect
      string letters; // variable part of TZ abbreviations when rule in effect

      // Combine y + this->when + offset to give a UTC timestamp.
      sys_seconds
      start_time(year y, seconds offset) const
      {
	auto time = when.time;
	if (when.indicator == at_time::Wall
	      || when.indicator == at_time::Standard)
	  time -= offset; // Convert local time to sys time.
	return sys_days(when.day.pin(y)) + time;
      }

      friend istream& operator>>(istream& in, Rule& rule)
      {
	string str;

	// Rule  NAME  FROM  TO  TYPE  IN  ON  AT  SAVE  LETTER/S

	in >> quoted(rule.name) >> years_from_to{rule.from, rule.to};

	if (char type; in >> type && type != '-')
	  in.setstate(ios::failbit);
	in >> rule.when.day >> static_cast<at_time&>(rule.when);
	at_time save_time;
	save_time.indicator = at_time::Wall; // not valid for this field
	in >> save_time;
	if (save_time.indicator != at_time::Wall)
	  {
	    // We don't actually store the save_time.indicator, because we
	    // assume that it's always deducible from the offset value.
	    auto expected = save_time.time == 0s
			      ? at_time::Standard
			      : at_time::Daylight;
	    __glibcxx_assert(save_time.indicator == expected);
	  }

	rule.save = save_time.time;

	in >> rule.letters;
	return in;
      }

#ifdef _GLIBCXX_ASSERTIONS
      friend ostream& operator<<(ostream& out, const Rule& r)
      {
	out << "Rule " << r.name << ' ' << (int)r.from << ' ' << (int)r.to
	    << ' ' << r.when.day.get_month() << ' ';
	switch (r.when.day.kind)
	{
	case on_month_day::DayOfMonth:
	  out << (unsigned)r.when.day.get_day();
	  break;
	case on_month_day::LastWeekday:
	  out << "last" << weekday(r.when.day.day_of_week);
	  break;
	case on_month_day::LessEq:
	  out << weekday(r.when.day.day_of_week) << " <= "
	    << r.when.day.day_of_month;
	  break;
	case on_month_day::GreaterEq:
	  out << weekday(r.when.day.day_of_week) << " >= "
	    << r.when.day.day_of_month;
	  break;
	}
	out << ' ' << hh_mm_ss(r.when.time) << "wusd"[r.when.indicator];
	out << ' ' << r.save.count();
	if (!r.letters.empty())
	  out << ' ' << r.letters;
	else
	  out << " -";
	return out;
      }
#endif
    };

    struct Transitions
    {
      struct Entry
      {
	const Rule* rule;
	sys_seconds when;
      };

      Entry prev{nullptr, sys_seconds::min()};
      Entry curr{nullptr, sys_seconds::min()};
      Entry next{nullptr, sys_seconds::max()};
    };

    // Collect transitions of rules surrounding specified time t:
    // * .curr - rule active directly before or at t,
    // * .prev - rule transition before trans.curr
    // * .next - rule transition directly after t
    Transitions
    find_surrounding_transitions(span<const Rule> rules, sys_seconds t,
				 seconds std_offset)
    {
      const year_month_day date(chrono::floor<days>(t));
      // Expand the search window for a time near the end
      // of the year which can be pushed to next/previous
      // year by 'offset'. This assumes that 'offset' is
      // never greater than 24h.
      year first_year = date.year(), last_year = date.year();
      if (std_offset.count() > 0)
	if (date.month() == December && date.day() == day(31))
	  ++last_year;
      if (std_offset.count() < 0)
	if (date.month() == January && date.day() == day(1))
	  --first_year;

      Transitions trans;
      for (const auto& rule : rules)
	{
	  if (last_year < rule.from) // Rule doesn't apply yet at time t.
	    break; // Rules are ordered by 'from' in ascending order.

	  seconds offset{}; // appropriate for at_time::Universal
	  if (rule.when.indicator == at_time::Wall
	      || rule.when.indicator == at_time::Standard)
	    offset = std_offset;

	  // Times at which rule takes affect before (or equal) t,
	  // and after t, respectively.
	  sys_seconds start_before = sys_seconds::min();
	  sys_seconds start_after = sys_seconds::max();
	  auto for_year = [&](year y)
	  {
	    // Time the rule takes effect on year y:
	    const sys_seconds rule_start = rule.start_time(y, offset);
	    if (rule_start <= t)
	      {
		start_before = rule_start;
		if (y == last_year && rule.to > y)
		  start_after = rule.start_time(++y, offset);
	      }
	    // Do not override start_after set for first_year
	    else if (rule_start < start_after)
	      {
		start_after = rule_start;
		if (y == first_year && rule.from < y)
		  start_before = rule.start_time(--y, offset);
	      }
	  };

	  if (first_year > rule.to)
	    // Rule no longer applies at time t, record last transition
	    start_before = rule.start_time(rule.to, offset);
	  else if (first_year == last_year)
	    for_year(first_year);
	  else
	   {
	     // Local time may of t may be in prev/next year.
	     if (first_year >= rule.from)
	       for_year(first_year);
	     if (last_year <= rule.to)
	       for_year(last_year);
	   }

	  if (trans.curr.when < start_before)
	    {
	      trans.prev = trans.curr;
	      trans.curr = {&rule, start_before};
	    }
	  else if (trans.prev.when < start_before)
	    trans.prev = {&rule, start_before};

	  if (start_after < trans.next.when)
	    trans.next = {&rule, start_after};
	}
      return trans;
    }

    const Rule*
    find_active_rule(span<const Rule> rules, sys_seconds t, seconds std_offset)
    {
      // Rule specifying start time as Wall time, should apply
      // running 'save' accumulated by earlier rules. To handle
      // that we firstly collect transitions surrounding specified
      // time t:
      auto trans = find_surrounding_transitions(rules, t, std_offset);

      // No rule was active at the time of t, running 'save'
      // cannot change this output, as we have no save to apply.
      if (!trans.curr.rule)
	return nullptr;

      auto cascade_save = [](const Rule* from, Transitions::Entry& to)
      {
	if (!from || from->save == seconds(0))
	  return false;
	if (!to.rule || to.rule->when.indicator != at_time::Wall)
	   return false;
	to.when -= from->save;
	return true;
      };

      if (cascade_save(trans.curr.rule, trans.next))
	// Running save moved what we considered trans.next to time
	// before or at t, in that case trans.next is active rule.
	if (trans.next.when <= t)
	  return trans.next.rule;

      if (cascade_save(trans.prev.rule, trans.curr))
	// Running save moved what we consider trans.curr to
	// time after t, in that case trans.prev is active rule.
	if (trans.curr.when > t)
	  return trans.prev.rule;

      return trans.curr.rule;
    }

    const Rule*
    find_active_rule(span<const Rule> rules, local_seconds t, seconds std_offset)
    {
      // UNTIL or START time in local time should take a 'save' from
      // active rule. To handle tapproximate system time at, and
      // collect transitions surrounding it:
      const sys_seconds at(t.time_since_epoch() - std_offset);
      const auto trans = find_surrounding_transitions(rules, at, std_offset);

      auto to_local = [&](const Transitions::Entry& tran, const Rule* before)
      {
	local_seconds ls(tran.when.time_since_epoch());
	if (!tran.rule)
	  // 'when' is either min() or max(), and applying std_offset overflows.
	  return ls;

	ls += std_offset;
	if (!before || tran.rule->when.indicator == at_time::Wall)
	  // no active rule or 'when' was converted from local time without
	  // considering running 'save' in first place
	  return ls;

	return ls + before->save;
      };

      // Translate the transitions time to local time, taking 'save'
      // into consideration, and recheck active rule.
      if (to_local(trans.next, trans.curr.rule) <= t)
	return trans.next.rule;
      if (to_local(trans.curr, trans.prev.rule) > t)
	return trans.prev.rule;
      return trans.curr.rule;
    }

    const Rule*
    find_first_std(span<const Rule> rules)
    {
      auto is_std = [](const Rule& rule) { return !rule.save.count(); };
      // Rules with same name are sorted by 'from' and then 'save' in ascending order.
      auto it = ranges::find_if(rules, is_std);
      if (it == rules.end())
	return nullptr;

      const Rule* first = &*it;
      const year y = first->from;
      for (const Rule& next : span<const Rule>(++it, rules.end()))
	{
	  if (!is_std(next) || next.from > y)
	    break;
	  if (next.start_time(y, {}) < first->start_time(y, {}))
	    first = &next;
	}
      return first;
    }

    bool
    ZoneInfo::calc_save(span<const Rule> all_rules) noexcept
    {
      // Expanded rule, or the save value was already computed.
      if (m_state < SavePending)
	return false;

      // Find the rules named by rules()
      span<const Rule> sel_rules
	= ranges::equal_range(all_rules, rules(), ranges::less{}, &Rule::name);
      if (sel_rules.empty())
	__throw_runtime_error("std::chrono::time_zone::get_info: invalid data");

      if (m_state == UntilPending)
	{
	  // UNTIL was specified in Wall time, so it is affected by 'save'.
	  // Convert it back to local time, and find active rule using that.
	  const local_seconds lu(m_until.time_since_epoch() + m_offset);
	  if (const Rule* rule = find_active_rule(sel_rules, lu - seconds(1), m_offset))
	    if (rule->save.count() != 0)
	      {
		m_state = SaveKnown;
		m_save = duration_cast<minutes>(rule->save);
		m_until -= rule->save;
		return true;
	      }
	 }
      else
	// UNTIL was either specified in Universal time, or Standard
	// time (known total offset). m_until is corresponding sys_time point.
	if (const Rule* rule = find_active_rule(sel_rules, m_until - seconds(1), m_offset))
	  m_save = duration_cast<minutes>(rule->save);

      m_state = SaveKnown;
      return false;
    }
  } // namespace
#endif // TZDB_DISABLED

  // Private constructor used by reload_tzdb() to create time_zone objects.
  time_zone::time_zone(unique_ptr<_Impl> __p) : _M_impl(std::move(__p)) { }

  time_zone::~time_zone() = default;

  // The opaque pimpl class stored in a time_zone object.
  struct time_zone::_Impl
  {
#ifndef TZDB_DISABLED
    explicit
    _Impl(weak_ptr<tzdb_list::_Node> node) : node(std::move(node)) { }

    vector<ZoneInfo> infos; // Definitions of the time zone's transitions.

    // Non-owning reference back to the tzdb that owns this time_zone.
    // Needed to access the list of rules for the time zones.
    weak_ptr<tzdb_list::_Node> node;

    // In the simple case, we don't actual keep count. No concurrent access
    // to the infos vector is possible, even if all infos are expanded.
    template<typename _Tp>
      struct RulesCounter
      {
	// Called for each rule-based ZoneInfo added to the infos vector.
	// Called when the time_zone::_Impl is created, so no concurrent calls.
	void increment() { }
	// Called when a rule-based ZoneInfo is expanded.
	// The caller must have called lock() for exclusive access to infos.
	void decrement() { }

	// Use a mutex to synchronize all access to the infos vector.
	mutex infos_mutex;

	void lock() { infos_mutex.lock(); }
	void unlock() { infos_mutex.unlock(); }
      };

#if defined __GTHREADS && __cpp_lib_atomic_wait
    // Atomic count of unexpanded ZoneInfo objects in the infos vector.
    // Concurrent access is allowed when all objects have been expanded.
    // Only use an atomic counter if it would not require libatomic,
    // because we don't want libstdc++.so to depend on libatomic.
    template<typename _Tp> requires _Tp::is_always_lock_free
      struct RulesCounter<_Tp>
      {
	_Tp counter{0};

	void
	increment()
	{ counter.fetch_add(1, memory_order::relaxed); }

	void
	decrement()
	{
	  // The current thread holds the lock, so the counter is negative
	  // and so we need to increment it to decrement it!
	  // If the count reaches zero then there are no more unexpanded infos,
	  // so notify all waiting threads that they can access the infos.
	  // We must do this here, because unlock() is a no-op if counter==0.
	  if (++counter == 0)
	    counter.notify_all();
	}

	void
	lock()
	{
	  // If counter is zero then concurrent access is allowed, so lock()
	  // and unlock() are no-ops and multiple threads can "lock" at once.
	  // If counter is non-zero then the contents of the infos vector might
	  // need to be changed, so only one thread is allowed to access it.
	  for (auto c = counter.load(memory_order::relaxed); c != 0;)
	    {
	      // Setting counter to negative means this thread has the lock.
	      if (c > 0 && counter.compare_exchange_strong(c, -c))
		return;

	      if (c < 0)
		{
		  // Counter is negative, another thread already has the lock.
		  counter.wait(c);
		  c = counter.load();
		}
	    }
	}

	void
	unlock()
	{
	  if (auto c = counter.load(memory_order::relaxed); c < 0)
	  {
	    counter.store(-c, memory_order::release);
	    counter.notify_one();
	  }
	}
      };
#endif // __GTHREADS && __cpp_lib_atomic_wait

#if __cpp_lib_atomic_lock_free_type_aliases
    RulesCounter<atomic_signed_lock_free> rules_counter;
#else
    RulesCounter<void> rules_counter;
#endif

#else // TZDB_DISABLED
    _Impl(weak_ptr<tzdb_list::_Node>) { }
    struct {
      sys_info info;
      void push_back(sys_info i) { info = std::move(i); }
    } infos;
#endif // TZDB_DISABLED
  };

  // Implementation of std::chrono::time_zone::get_info(const sys_time<D>&)
  sys_info
  time_zone::_M_get_sys_info(sys_seconds tp) const
  {
#ifndef TZDB_DISABLED
    // This gives us access to the node->rules vector, but also ensures
    // that the tzdb node won't get erased while we're still using it.
    const auto node = _M_impl->node.lock();
    auto& infos = _M_impl->infos;

    // Prevent concurrent access to _M_impl->infos if it might need to change.
    lock_guard lock(_M_impl->rules_counter);

    // Find the transition info for the time point.
    auto i = ranges::upper_bound(infos, tp, ranges::less{}, &ZoneInfo::until);

    if (i == infos.end())
      {
	if (infos.empty())
	  __throw_runtime_error("std::chrono::time_zone::get_info: invalid data");
	tp = (--i)->until();
      }

    sys_info info;

    if (i == infos.begin())
      info.begin = sys_days(year::min()/January/1);
    else
      info.begin = i[-1].until();

    if (i->to(info)) // We already know a sys_info for this time.
      return info;

    // Otherwise, we have a ZoneInfo object that describes the applicable
    // transitions in terms of a set of named rules that vary by year.
    // Replace that rules-based ZoneInfo object with a sequence of one or more
    // objects that map directly to a sys_info value.
    const ZoneInfo& ri = *i;

    // Find the rules named by ri.rules()
    span<const Rule> rules = ranges::equal_range(node->rules, ri.rules(),
						 ranges::less{}, &Rule::name);

    if (rules.empty())
      __throw_runtime_error("std::chrono::time_zone::get_info: invalid data");

    vector<pair<sys_info, string_view>> new_infos;
    if (int n = ceil<years>(tp - info.begin).count())
      new_infos.reserve(std::min(100, n * 2));
    long result_index = -1;
    int num_after = 4; // number of sys_info to generate past tp.

    // The following initial values for info.offset, info.save, and letters
    // are only valid if the first sys_info we are generating uses the time
    // zone's standard time, because daylight time would need non-zero offset.
    // This is true by construction, because this function always tries to
    // finish so that the last ZoneInfo object expanded is for daylight time.
    // This means that i[-1] is either an expanded ZoneInfo for a DST sys_info
    // or is an unexpanded (rule-based) ZoneInfo for a different rule.
    info.offset = ri.offset();
    info.save = 0min;
    info.end = ri.until();
    info.abbrev = ri.format();

    string_view letters;
    if (i != infos.begin() && i[-1].expanded())
      letters = i[-1].next_letters();

    if (letters.empty())
      {
	// Try to find a Rule active before this time, to get initial
	// SAVE and LETTERS values. There may not be a Rule for the period
	// before the first DST transition, so find the earliest DST->STD
	// transition and use the LETTERS from that.
	if (const Rule* active_rule = find_active_rule(rules, info.begin, ri.offset()))
	  {
	    info.offset = ri.offset() + active_rule->save;
	    info.save = chrono::duration_cast<minutes>(active_rule->save);
	    letters = active_rule->letters;
	  }
	else if (const Rule* first_std = find_first_std(rules))
	  letters = first_std->letters;
      }

    // For transitions, that leads to backward jump in the local time,
    // and window of duplicated local time, the rule transition occurring
    // during that window are considered to apply immediately at the boundary.
    // This window is [info.begin, info.begin + merge_window].
    seconds merge_window(0);
    if (i != infos.begin())
      {
	const auto prev_offset = i[-1].offset() + i[-1].save();
	if (prev_offset > info.offset)
	  merge_window = prev_offset - info.offset;
      }

    const Rule* curr_rule = nullptr;

    while (info.begin < info.end && num_after > 0)
      {
	sys_seconds t = info.begin;
	const year_month_day date(chrono::floor<days>(t));
	const Rule* next_rule = nullptr;

	// Check every rule to find the next transition after t.
	for (const auto& rule : rules)
	  {
	    if (&rule == curr_rule) // don't bother checking this one again
	      continue;

	    if (date.year() > rule.to) // rule no longer applies at time t
	      continue;

	    sys_seconds rule_start;

	    seconds offset{}; // appropriate for at_time::Universal
	    if (rule.when.indicator == at_time::Wall)
	      offset = info.offset;
	    else if (rule.when.indicator == at_time::Standard)
	      offset = ri.offset();

	    if (date.year() < rule.from) // rule doesn't apply yet at time t
	      {
		// Find first transition for this rule:
		rule_start = rule.start_time(rule.from, offset);
	      }
	    else // rule applies in the year that contains time t
	      {
		year y = date.year();
		// Time the rule takes effect this year:
		rule_start = rule.start_time(y, offset);

		if (rule_start < t && rule.to > y)
		  {
		    // Try this rule in the following year.
		    rule_start = rule.start_time(++y, offset);
		  }
	      }

	    if (t < rule_start && rule_start < info.end)
	      {
		// Found a closer transition than the previous info.end.
		info.end = rule_start;
		next_rule = &rule;
	      }
	  }

	format_abbrev_str(info, letters);

	if (next_rule)
	  letters = next_rule->letters;
	else
	  letters = {};

	// Transitions occuring in the backward jump time window occuring
	// on zone transitions should be folded into zone change.
	if (info.end - t <= merge_window)
	  info.begin = t;
	else
	  {
	    new_infos.emplace_back(info, letters);

	    if (info.begin <= tp && tp < info.end) // Found the result.
	      result_index = new_infos.size() - 1;
	    else if (result_index >= 0)
	      {
		// Finish before a STD sys_info if possible, so that if we resume
		// generating sys_info objects after this time point, save=0
		// should be correct for the next sys_info.
		if (num_after > 1 || !next_rule || next_rule->save == 0s)
		  --num_after;
	      }

	    info.begin = info.end;
	  }
	merge_window = seconds(0);

	if (next_rule)
	  {
	    info.end = ri.until();
	    info.offset = ri.offset() + next_rule->save;
	    info.save = duration_cast<minutes>(next_rule->save);
	    info.abbrev = ri.format();
	    letters = next_rule->letters;
	    curr_rule = next_rule;
	  }
      }

    if (new_infos.empty() || result_index < 0)
      __throw_runtime_error("time_zone::get_info");

    info = new_infos[result_index].first;

    if (new_infos.back().first.end < ri.until())
      {
	// Insert the new sys_info objects but don't remove the rules_info.
	infos.insert(i, new_infos.begin(), new_infos.end());
      }
    else
      {
	// Replace the rules_info at *i with the sys_info objects in new_infos.

	// First note the index of *i because we will invalidate i.
	result_index = i - infos.begin();
	// Insert everything except new_infos.front() at the end of infos:
	i = infos.insert(infos.end(), new_infos.begin() + 1, new_infos.end());
	// Then rotate those new elements into place:
	std::rotate(infos.begin() + result_index + 1, i, infos.end());
	// Then replace the original rules_info object with new_infos.front():
	infos[result_index] = ZoneInfo(new_infos.front());
	// Decrement count of rule-based infos (might also release lock).
	_M_impl->rules_counter.decrement();
      }
    return info;
#else
    return _M_impl->infos.info;
#endif // TZDB_DISABLED
  }

  // Implementation of std::chrono::time_zone::get_info(const local_time<D>&)
  local_info
  time_zone::_M_get_local_info(local_seconds tp) const
  {
    local_info info{};
#ifndef TZDB_DISABLED
    const auto node = _M_impl->node.lock();

    // Get sys_info assuming no offset between local time and UTC:
    info.first = _M_get_sys_info(sys_seconds(tp.time_since_epoch()));

    // Convert to UTC using the real offset:
    sys_seconds st(tp.time_since_epoch() - info.first.offset);

    if ((st - info.first.begin) <= days(1))
      {
	sys_info prev = _M_get_sys_info(info.first.begin - 1s);
	sys_seconds prevst(tp.time_since_epoch() - prev.offset);
	if (st < info.first.begin)
	  {
	    if (prevst < info.first.begin)
	      {
		// tp is a unique local time, prev is the correct sys_info.
		info.first = prev;
	      }
	    else
	      {
		// The local time is nonexistent, falling within a clock change
		// e.g. there is no 1:30am if DST moves clock from 1am to 2am.
		__glibcxx_assert(prev.offset < info.first.offset); // start DST
		info.result = local_info::nonexistent;
		info.second = info.first;
		info.first = prev;
	      }
	  }
	else if (prevst < info.first.begin)
	  {
	    // The local time is ambiguous, referring to two possible UTC times
	    // e.g. 1:30am happens twice if clock moves back from 2am to 1am.
	    __glibcxx_assert(prev.offset > info.first.offset); // DST ended
	    info.result = local_info::ambiguous;
	    info.second = info.first;
	    info.first = prev;
	  }
	// else tp is a unique local time, info.first is the correct sys_info.
      }
    else if ((info.first.end - st) <= days(1))
      {
	sys_info next = _M_get_sys_info(info.first.end);
	sys_seconds nextst(tp.time_since_epoch() - next.offset);
	if (st >= info.first.end)
	  {
	    if (nextst >= info.first.end)
	      {
		// tp is a unique local time, next is the correct sys_info.
		info.first = next;
	      }
	    else
	      {
		info.result = local_info::nonexistent;
		info.second = next;
	      }
	  }
	else if (nextst >= info.first.end)
	  {
	    info.result = local_info::ambiguous;
	    info.second = next;
	  }
	// else tp is a unique local time, info.first is the correct sys_info.
      }
#else
    info.first = _M_impl->infos.info;
#endif // TZDB_DISABLED
    return info;
  }

#ifndef TZDB_DISABLED
  namespace
  {
    // If a zoneinfo directory is defined (either when the library was built,
    // or via the zoneinfo_dir_override function) then append filename to it.
    // The filename should have a leading '/' as one is not added explicitly.
    string
    zoneinfo_file(string_view filename)
    {
      string path;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Waddress"
      if (__gnu_cxx::zoneinfo_dir_override)
      {
	if (auto override_dir = __gnu_cxx::zoneinfo_dir_override())
	  path = override_dir;
#pragma GCC diagnostic pop
      }
#ifdef _GLIBCXX_ZONEINFO_DIR
      else
	path = _GLIBCXX_ZONEINFO_DIR;
#endif
#ifdef _GLIBCXX_HAVE_WINDOWS_H
      if (path.empty())
	{
	  HMODULE dll_module;
	  if (GetModuleHandleExA(
		  GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
		      | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
		  reinterpret_cast<const char *>(&zoneinfo_file), &dll_module))
	    {
	      char dll_path[MAX_PATH];
	      if (GetModuleFileNameA(dll_module, dll_path, MAX_PATH) != 0)
		{
		  string_view dll_path_view = dll_path;
		  auto pos = dll_path_view.find_last_of('\\');
		  dll_path_view = dll_path_view.substr(0, pos);
		  if (dll_path_view.ends_with("\\bin"))
		    {
		      constexpr string_view remaining_path = "share\\zoneinfo";
		      dll_path_view.remove_suffix(3); // Remove bin
		      path.reserve(dll_path_view.size()
				   + remaining_path.size());
		      path = dll_path_view;
		      path += remaining_path;
		    }
		}
	    }
	}
#endif
      if (!path.empty())
	path.append(filename);
      return path;
    }

    // N.B. Leading slash as required by zoneinfo_file function.
    const string_view tzdata_file = "/tzdata.zi";
    const string_view leaps_file = "/leapseconds";

#ifdef _GLIBCXX_STATIC_TZDATA
// Static copy of tzdata.zi embedded in the library.
static constexpr char tzdata_chars[] = {
#embed "tzdata.zi"
};
#endif

    // An istream type that can read from a file or from a string.
    struct tzdata_stream : istream
    {
      // std::spanbuf not available until C++23
      struct ispanbuf : streambuf
      {
	ispanbuf() : streambuf()
	{
#ifdef _GLIBCXX_STATIC_TZDATA
	  char* p = const_cast<char*>(tzdata_chars);
	  this->setg(p, p, p + std::size(tzdata_chars));
#endif
	}

	// N.B. seekoff and seekpos not overridden, not currently needed.
      };

      union {
	filebuf fb;
	ispanbuf sb;
      };

      tzdata_stream() : istream(nullptr)
      {
	if (string path = zoneinfo_file(tzdata_file); !path.empty())
	{
	  filebuf fbuf;
	  if (fbuf.open(path, std::ios::in))
	    {
	      std::construct_at(&fb, std::move(fbuf));
	      this->init(&fb);
	      return;
	    }
	}
	std::construct_at(&sb);
	this->init(&sb);
      }

      ~tzdata_stream() { std::destroy_at(this->rdbuf()); } // use virtual dtor

      bool using_static_data() const { return this->rdbuf() == &sb; }
    };
  }
#endif // TZDB_DISABLED

// These are the same values as the array in the <chrono> header, but might
// contain additional leap seconds if the libstdc++.so used at runtime is
// newer than the <chrono> header used to compile parts of the application.
constexpr leap_second tzdb_list::_Node::fixed_leaps[] {
#define LS leap_second
  LS(  78796800), // 1 Jul 1972
  LS(  94694400), // 1 Jan 1973
  LS( 126230400), // 1 Jan 1974
  LS( 157766400), // 1 Jan 1975
  LS( 189302400), // 1 Jan 1976
  LS( 220924800), // 1 Jan 1977
  LS( 252460800), // 1 Jan 1978
  LS( 283996800), // 1 Jan 1979
  LS( 315532800), // 1 Jan 1980
  LS( 362793600), // 1 Jul 1981
  LS( 394329600), // 1 Jul 1982
  LS( 425865600), // 1 Jul 1983
  LS( 489024000), // 1 Jul 1985
  LS( 567993600), // 1 Jan 1988
  LS( 631152000), // 1 Jan 1990
  LS( 662688000), // 1 Jan 1991
  LS( 709948800), // 1 Jul 1992
  LS( 741484800), // 1 Jul 1993
  LS( 773020800), // 1 Jul 1994
  LS( 820454400), // 1 Jan 1996
  LS( 867715200), // 1 Jul 1997
  LS( 915148800), // 1 Jan 1999
  LS(1136073600), // 1 Jan 2006
  LS(1230768000), // 1 Jan 2009
  LS(1341100800), // 1 Jul 2012
  LS(1435708800), // 1 Jul 2015
  LS(1483228800), // 1 Jan 2017
  // If new leap seconds get defined they should be added here.
  // Negative leap seconds are represented as -1 * timestamp.
#undef LS
};

namespace
{
  // The expiry date corresponding to the list above.
  // tzdata 2026a leapseconds list expires at 2026-12-28 00:00:00 UTC
  constexpr seconds fixed_expiry{1798416000u};
}

// This holds the most up-to-date number of leap seconds known at runtime.
// Initially zero, updated when _S_read_leap_seconds() is called.
struct tzdb_list::_Node::NumLeapSeconds
{
  // Called by __recent_leap_second_info to read num_leap_seconds.
  unsigned
  get()
  {
#if ATOMIC_INT_LOCK_FREE == 2
    atomic_ref<unsigned> ref(count);
    auto num = ref.load(memory_order::relaxed);

    if (num == std::size(_Node::fixed_leaps))
      // A leapseconds file has been read and has no new leap seconds.
      return num;

    if (num == 0)
      // No leapseconds file has been read yet.
      return 0;

    // The tzdb_list has been initialized and contains a tzdb object with
    // new leap seconds, which the caller is going to use.
    // The relaxed load above does not synchronize with anything, so to
    // ensure that the get_tzdb_list() in the caller will see a tzdb object
    // set by _S_replace_head, we load num_leap_seconds again with acquire
    // ordering:
    return ref.load(memory_order::acquire);
#else
    lock_guard<mutex> l(list_mutex()); // This ensures acquire ordering.
    return count;
#endif
  }

  // Called by __recent_leap_second_info to set num_leap_seconds when
  // we have determined there are no new leap seconds in a leapseconds file.
  void
  set_to_fixed_size()
  {
#if ATOMIC_INT_LOCK_FREE == 2
    atomic_ref<unsigned> ref(count);
    unsigned expected = 0;
    ref.compare_exchange_strong(expected, std::size(_Node::fixed_leaps),
				memory_order::relaxed);
#else
    lock_guard<mutex> l(list_mutex());
    if (count == 0)
      count = std::size(_Node::fixed_leaps);
#endif
  }

  // Called by _Node::_S_replace_head
  // The two versions are named differently so that caller has to be explicit
  // about which version it calls, based on whether the mutex is held.
  void
  set(unsigned val)
  {
#if ATOMIC_INT_LOCK_FREE == 2
    atomic_ref<unsigned> ref(count);
    // The release op here synchronizes with the acquire op in get().
    ref.store(val, memory_order::release);
#else
    lock_guard<mutex> l(list_mutex());
    count = val;
#endif
  }

  void
  set_locked(unsigned val, const lock_guard<mutex>&)
  {
#if ATOMIC_INT_LOCK_FREE == 2
    // Even though the caller locked the mutex, we still need to use an
    // atomic store in this case, because there could be concurrent loads.
    set(val);
#else
    // The only caller of this function locks list_mutex() so we would
    // deadlock if we locked it again here.
    count = val;
#endif
  }

private:
  unsigned count = 0;
};

constinit tzdb_list::_Node::NumLeapSeconds tzdb_list::_Node::num_leap_seconds;

  namespace __detail
  {
    // Called by chrono::__detail::__get_leap_second_info in <chrono>
    // to get leap_second_info for times after the expiry date in the header.
    // The caller provides the time being queried in `info.elapsed` and
    // whether that is a UTC time in `info.is_leap_second`.
    // If it returns true, this function did the lookup and updated `info`.
    // If this returns false, it means the hardcoded list of leap seconds
    // in the header should be used for the lookup.
    bool
    __recent_leap_second_info(leap_second_info& info,
			      unsigned num_positive_leaps)
    {
      // Extract the input args from info:
      const auto [is_utc, secs] = info;
      // And then reuse info for the output result:
      info.is_leap_second = false;
      info.elapsed = seconds(num_positive_leaps);

      auto update_info = [&](span<const leap_second> leaps)
      {
	if (leaps.size() == num_positive_leaps)
	  return false; // No new leap seconds, use the array in the header.

	// info.elapsed already contains the first N leap seconds,
	// so we only search the end of the span.
	auto first = leaps.begin() + num_positive_leaps;
	auto pos = std::upper_bound(first, leaps.end(), sys_seconds(secs));
	for (auto i = first; i != pos; ++i)
	  info.elapsed += i->value();

	if (is_utc)
	  {
	    // This should never happen, but check it so that pos[-1] is valid:
	    if (num_positive_leaps == 0) [[unlikely]]
	      return false;

	    // Convert utc_time to sys_time:
	    sys_seconds ss(secs - info.elapsed);
	    // See if that sys_time is before (or during) previous leap sec:
	    if (ss < pos[-1])
	      {
		if ((ss + 1s) >= pos[-1])
		  info.is_leap_second = true;
		else
		  info.elapsed -= pos[-1].value();
	      }
	  }
	return true;
      };

      using _Node = tzdb_list::_Node;

      // If the caller was compiled using an older GCC with an older expiry
      // time in the header than the `fixed_expiry` defined above, we might
      // be able to answer the query easily using the static `fixed_leaps`.
      if (secs <= fixed_expiry)
	return update_info(_Node::fixed_leaps);

      constexpr auto num_fixed_leaps = std::size(_Node::fixed_leaps);

      auto num_leaps = _Node::num_leap_seconds.get();
      if (num_leaps == num_fixed_leaps)
	// A leapseconds file has been read and has no new leap seconds:
	return update_info(_Node::fixed_leaps);
      else if (num_leaps == 0)
	{
	  // The tzdb_list has not been initialized yet, so we don't know
	  // the correct number of leap seconds.
	  // We use _S_read_leap_seconds() to read the leapseconds file.
	  // If that tells us there are no new leapseconds, we don't need
	  // to parse all of tzdata.zi and initialize a whole tzdb object.
	  if (_Node::_S_read_leap_seconds().first.size() == num_fixed_leaps)
	    {
	      // There are no new leap seconds. Remember that so that the next
	      // call to this function can just use fixed_leaps.
	      _Node::num_leap_seconds.set_to_fixed_size();
	      return update_info(_Node::fixed_leaps);
	    }
	  // else there are new leap seconds. We init tzdb_list so that the
	  // new leap seconds are persisted in a tzdb object.
	}

      // Use updated leap_seconds from tzdb.
      return update_info(get_tzdb_list().begin()->leap_seconds);
    }
  }

  // Return leap_second values, and a bool indicating whether the values are
  // current (true), or potentially out of date (false).
  pair<vector<leap_second>, bool>
  tzdb_list::_Node::_S_read_leap_seconds()
  {
    // Populate the vector with the leap seconds we already know about:
    vector<leap_second> leaps(fixed_leaps, std::end(fixed_leaps));

    bool read_leaps_file = false;

#ifndef TZDB_DISABLED
    if (ifstream ls{zoneinfo_file(leaps_file)})
      {
	constexpr year exp_year
	  = year_month_day(sys_days(duration_cast<days>(fixed_expiry))).year();

	std::string s, w;
	s.reserve(80); // Avoid later reallocations.
	while (std::getline(ls, s))
	  {
	    // Leap  YEAR  MONTH  DAY  HH:MM:SS  CORR  R/S

	    if (!s.starts_with("Leap"))
	      continue;

	    istringstream li(std::move(s));
	    li.exceptions(ios::failbit);
	    li.ignore(4);
	    unsigned yval;
	    if (li >> yval)
	      {
		if (year y(yval); y >= exp_year) // Only process new entries.
		  {
		    li >> w;
		    char m = w[0];
		    if (m != 'J' && m != 'D')
		      return {std::move(leaps), false};

		    const int is_december = m == 'D';
		    year_month_day ymd{y, month(6 + 6 * is_december),
				       day(30 + is_december)};
		    sys_seconds secs(sys_days(ymd) + days(1));
		    li >> w >> w >> m;

		    if (m != '+' && m != '-')
		      return {std::move(leaps), false};

		    seconds::rep val = secs.time_since_epoch().count();
		    if (m == '-') [[unlikely]]
		      val = -(val - 1); // -ve leap second happens at 23:59:59

		    if (leap_second ls{val}; ls > leaps.back())
		      leaps.push_back(ls);
		  }
	      }
	    s = std::move(li).str(); // give allocated storage back to s
	  }

	read_leaps_file = true;
      }
#endif

    return {std::move(leaps), read_leaps_file};
  }

#ifndef TZDB_DISABLED
  namespace
  {
    // Read the version number from a tzdata.zi file.
    string
    remote_version(istream& zif)
    {
      char hash;
      string label;
      string version;
      if (zif >> hash >> label >> version)
	if (hash == '#' && label == "version")
	  return version;
#if 0 // Ignore these files, because we're not using them anyway.
#if defined __NetBSD__
      if (string ver; ifstream(zoneinfo_file("/TZDATA_VERSION")) >> ver)
	return ver;
#elif defined __APPLE__
      // The standard install on macOS has no tzdata.zi, but we can find the
      // version from +VERSION.
      if (string ver; ifstream(zoneinfo_file("/+VERSION")) >> ver)
	return ver;
#endif
#endif
      __throw_runtime_error("tzdb: no version found in tzdata.zi");
    }
  }
#endif

  // Definition of std::chrono::remote_version()
  string remote_version()
  {
#ifndef TZDB_DISABLED
    tzdata_stream zif;
    return remote_version(zif);
#else
    __throw_disabled();
#endif
  }

  // Used by chrono::reload_tzdb() to add a new node to the front of the list.
  const tzdb&
  tzdb_list::_Node::_S_replace_head(shared_ptr<_Node> curr [[maybe_unused]],
				    shared_ptr<_Node> new_head)
  {
    _Node* new_head_ptr = new_head.get();
#if USE_ATOMIC_SHARED_PTR
    new_head_ptr->next = curr;
    while (!_S_head_owner.compare_exchange_strong(curr, new_head))
      {
	if (curr->db.version == new_head_ptr->db.version)
	  return curr->db;
	new_head_ptr->next = curr;
      }
    // XXX small window here where _S_head_cache still points to previous tzdb.
    _S_cache_list_head(new_head_ptr);

    // This allows __recent_leap_second_info() to know that it can use
    // get_tzdb_list()->begin()->leap_seconds to get new leap seconds.
    num_leap_seconds.set(new_head_ptr->db.leap_seconds.size());
#else
    lock_guard<mutex> lock(list_mutex());
    if (const _Node* h = _S_head_owner.get())
      {
	if (h->db.version == new_head_ptr->db.version)
	  return h->db;
	new_head_ptr->next = _S_head_owner;
      }
    _S_head_owner = std::move(new_head);
    _S_cache_list_head(new_head_ptr);

    num_leap_seconds.set_locked(new_head_ptr->db.leap_seconds.size(), lock);
#endif

    return new_head_ptr->db;
  }

  // Called to populate the list for the first time. If reload_tzdb() fails,
  // it creates a tzdb that only contains the UTC and GMT time zones.
  const tzdb&
  tzdb_list::_Node::_S_init_tzdb()
  {
#ifndef TZDB_DISABLED
    __try
      {
	return reload_tzdb();
      }
    __catch (const std::exception&)
#endif
      {
	auto [leaps, ok] = _S_read_leap_seconds();

	using Node = tzdb_list::_Node;
	auto node = std::make_shared<tzdb_list::_Node>();
	node->db.version = "ersatz";
	node->db.leap_seconds = std::move(leaps);
	node->db.zones.reserve(2);
	node->db.links.reserve(7);

	time_zone zone(nullptr);
	time_zone_link link(nullptr);
	sys_info info{sys_seconds::min(), sys_seconds::max(), 0s, 0min, ""};

	zone._M_impl = std::make_unique<time_zone::_Impl>(node);
	zone._M_name = "Etc/UTC";
	info.abbrev = "UTC";
	zone._M_impl->infos.push_back(std::move(info));

	link._M_target = zone._M_name;
	link._M_name = "UTC";
	node->db.links.push_back(std::move(link));
	for (auto name : {"Etc/UCT", "Etc/Universal", "Etc/Zulu"})
	  {
	    link._M_target = zone._M_name;
	    link._M_name = name;
	    node->db.links.push_back(std::move(link));
	    link._M_target = zone._M_name;
	    link._M_name = name + 4;
	    node->db.links.push_back(std::move(link));
	  }
	node->db.zones.emplace_back(std::move(zone));

	zone._M_impl = std::make_unique<time_zone::_Impl>(node);
	zone._M_name = "Etc/GMT";
	info.abbrev = "GMT";
	zone._M_impl->infos.push_back(std::move(info));

	link._M_target = zone._M_name;
	link._M_name = "GMT";
	node->db.links.push_back(std::move(link));
	for (auto name : {"Etc/GMT+0", "Etc/GMT-0", "Etc/GMT0", "Etc/Greenwich"})
	  {
	    link._M_target = zone._M_name;
	    link._M_name = name;
	    node->db.links.push_back(std::move(link));
	    link._M_target = zone._M_name;
	    link._M_name = name + 4;
	    node->db.links.push_back(std::move(link));
	  }
	node->db.zones.emplace_back(std::move(zone));

	ranges::sort(node->db.zones);
	ranges::sort(node->db.links);
	return Node::_S_replace_head(nullptr, std::move(node));
      }
  }

  // There are only three ways for users to access the tzdb list.
  // get_tzdb_list() returns a reference to the list itself.
  // get_tzdb() returns a reference to the front of the list.
  // reload_tzdb() returns a reference to the (possibly new) front of the list.
  // Those are the only functions that need to check whether the list has
  // been populated already.

  // Implementation of std::chrono::get_tzdb_list()
  tzdb_list&
  get_tzdb_list()
  {
    using Node = tzdb_list::_Node;
    if (Node::_S_list_head(memory_order::acquire) == nullptr) [[unlikely]]
      Node::_S_init_tzdb(); // populates list
    return Node::_S_the_list;
  }

  // Implementation of std::chrono::get_tzdb()
  const tzdb&
  get_tzdb()
  {
    using Node = tzdb_list::_Node;
    if (auto* __p = Node::_S_list_head(memory_order::acquire)) [[likely]]
      return __p->db;
    return Node::_S_init_tzdb(); // populates list
  }

  // Implementation of std::chrono::reload_tzdb()
  const tzdb&
  reload_tzdb()
  {
#ifndef TZDB_DISABLED
    using Node = tzdb_list::_Node;

    tzdata_stream zif;
    const string version = remote_version(zif);

#if USE_ATOMIC_SHARED_PTR
    auto head = Node::_S_head_owner.load(memory_order::acquire);
    if (head != nullptr && head->db.version == version)
      return head->db;
#else
    if (Node::_S_list_head(memory_order::relaxed) != nullptr) [[likely]]
    {
      lock_guard<mutex> l(list_mutex());
      const tzdb& current = Node::_S_head_owner->db;
      if (current.version == version)
	return current;
    }
    shared_ptr<Node> head; // Passed as unused arg to _S_replace_head.
#endif

    auto [leaps, leaps_ok] = Node::_S_read_leap_seconds();
    if (!leaps_ok && !zif.using_static_data())
      __throw_runtime_error("tzdb: cannot parse leapseconds file");

    auto node = std::make_shared<Node>();
    node->db.version = std::move(version);
    node->db.leap_seconds = std::move(leaps);

    string line, type;
    line.reserve(80); // Maximum allowed line is 511 but much less in practice.
    istringstream is;
    is.exceptions(ios::failbit);
    int lineno = 0;
    while (std::getline(zif, line))
      {
	++lineno;
	if (line.empty())
	  continue;
	is.str(std::move(line));
	is.clear();
	ws(is);
	int c = is.eof() ? '#' : is.peek();
	__try
	  {
	    switch (c)
	    {
	      case '#':
		break;
	      case 'r':
	      case 'R':
	      {
		// Rule  NAME  FROM  TO  TYPE  IN  ON  AT  SAVE  LETTER/S
		is >> type; // extract the "Rule" or "R" marker
		type[0] = 'R';

		Rule rule;
		is >> rule;
		node->rules.push_back(std::move(rule));
		break;
	      }
	      case 'l':
	      case 'L':
	      {
		// Link  TARGET           LINK-NAME
		is >> type; // extract the "Link" or "L" marker
		type[0] = 'L';

		time_zone_link link(nullptr);
		is >> quoted(link._M_target) >> quoted(link._M_name);
		node->db.links.push_back(std::move(link));
		break;
	      }
	      case 'z':
	      case 'Z':
	      {
		// Zone  NAME        STDOFF  RULES   FORMAT  [UNTIL]
		is >> type; // extract the "Zone" or "Z" marker
		type[0] = 'Z';

		time_zone tz(std::make_unique<time_zone::_Impl>(node));
		is >> quoted(tz._M_name);
		node->db.zones.push_back(time_zone(std::move(tz)));
		[[fallthrough]]; // Use default case to parse rest of line ...
	      }
	      default: // Continuation of the previous Zone line.
	      {
		// STDOFF  RULES   FORMAT  [UNTIL]
		if (type[0] != 'Z')
		  is.setstate(ios::failbit);

		auto& impl = *node->db.zones.back()._M_impl;
		ZoneInfo& info = impl.infos.emplace_back();
		is >> info;

		// Keep count of ZoneInfo objects that refer to named Rules.
		if (!info.rules().empty())
		  impl.rules_counter.increment();
	      }
	    }
	  }
	__catch (const ios::failure&)
	  {
	    ostringstream ss;
	    ss << "std::chrono::reload_tzdb: parse error at line " << lineno
	       << ": " << std::move(is).str();
	    __throw_runtime_error(std::move(ss).str().c_str());
	  }

	line = std::move(is).str(); // return storage to line
      }

    ranges::sort(node->db.zones, {}, &time_zone::name);
    ranges::sort(node->db.links, {}, &time_zone_link::name);
    ranges::sort(node->rules, [](const Rule& lhs, const Rule& rhs)
    {
      if (auto result = lhs.name <=> rhs.name; result != 0)
	return result < 0;
      if (auto result = lhs.from <=> rhs.from; result != 0)
	return result < 0;
      return lhs.save < rhs.save;
    });

    // Calculate the SAVE value at UNTIL, and adjust it if necessary.
    for (time_zone& tz : node->db.zones)
      for (ZoneInfo& info : tz._M_impl->infos)
	info.calc_save(node->rules);

    return Node::_S_replace_head(std::move(head), std::move(node));
#else
    __throw_disabled();
#endif // TZDB_DISABLED
  }

  // Any call to tzdb_list::front() or tzdb_list::begin() must follow
  // a call to get_tzdb_list() so the list has already been populated.

  // Implementation of std::chrono::tzdb_list::front().
  const tzdb&
  tzdb_list::front() const noexcept
  {
    return _Node::_S_list_head(memory_order::seq_cst)->db;
  }

  // Implementation of std::chrono::tzdb_list::begin().
  auto
  tzdb_list::begin() const noexcept
  -> const_iterator
  {
#if USE_ATOMIC_SHARED_PTR
    return const_iterator{_Node::_S_head_owner.load()};
#else
    lock_guard<mutex> l(list_mutex());
    return const_iterator{_Node::_S_head_owner};
#endif
  }

  // Implementation of std::chrono::tzdb_list::erase_after(const_iterator).
  auto
  tzdb_list::erase_after(const_iterator p)
  -> const_iterator
  {
    if (p._M_node) [[likely]]
    {
#if ! USE_ATOMIC_SHARED_PTR
      lock_guard<mutex> l(list_mutex());
#endif
      if (auto next = p._M_node->next) [[likely]]
	return const_iterator{p._M_node->next = std::move(next->next)};
    }

    // This is undefined, but let's be kind:
    std::__throw_logic_error("std::tzdb_list::erase_after: iterator is not "
			     "dereferenceable");
  }

  // Private constructor for tzdb_list::const_iterator.
  // Only used within this file, so can be inline.
  inline
  tzdb_list::
  const_iterator::const_iterator(const shared_ptr<_Node>& __p) noexcept
  : _M_node(__p)
  { }

  // Implementation of std::chrono::tzdb_list::const_iterator::operator*().
  auto
  tzdb_list::const_iterator::operator*() const noexcept
  -> reference
  {
    return _M_node->db;
  }

  // Implementation of std::chrono::tzdb_list::const_iterator::operator++().
  auto
  tzdb_list::const_iterator::operator++()
  -> const_iterator&
  {
    auto cur = std::move(_M_node);
    _M_node = cur->next;
    return *this;
  }

  // Implementation of std::chrono::tzdb_list::const_iterator::operator++(int).
  auto
  tzdb_list::const_iterator::operator++(int)
  -> const_iterator
  {
    auto tmp = std::move(*this);
    _M_node = tmp._M_node->next;
    return tmp;
  }

  namespace
  {
    const time_zone*
    do_locate_zone(const vector<time_zone>& zones,
		   const vector<time_zone_link>& links,
		   string_view tz_name)
    {
      // Lambda mangling changed between -fabi-version=2 and -fabi-version=18
      auto search = []<class Vec>(const Vec& v, string_view name) {
	auto pos = ranges::lower_bound(v, name, {}, &Vec::value_type::name);
	auto ptr = pos.base();
	if (pos == v.end() || pos->name() != name)
	  ptr = nullptr;
	return ptr;
      };

      // Search zones first.
      if (auto tz = search(zones, tz_name))
	return tz;

      // Search links second.
      if (auto tz_l = search(links, tz_name))
	{
	  // Handle the common case of a link that has a zone as the target.
	  if (auto tz = search(zones, tz_l->target())) [[likely]]
	    return tz;

	  // Either tz_l->target() doesn't exist, or we have a chain of links.
	  // Use Floyd's cycle-finding algorithm to avoid infinite loops,
	  // at the cost of extra lookups. In the common case we expect a
	  // chain of links to be short so the loop won't run many times.
	  // In particular, the duplicate lookups to move the tortoise
	  // never happen unless the chain has four or more links.
	  // When a chain contains a cycle we do multiple duplicate lookups,
	  // but that case should never happen with correct tzdata.zi,
	  // so there's no need to optimize cycle detection.

	  const time_zone_link* tortoise = tz_l;
	  const time_zone_link* hare = search(links, tz_l->target());
	  while (hare)
	    {
	      // Chains should be short, so first check if it ends here:
	      if (auto tz = search(zones, hare->target())) [[likely]]
		return tz;

	      // Otherwise follow the chain:
	      hare = search(links, hare->target());
	      if (!hare)
		break;

	      // Again, first check if the chain ends at a zone here:
	      if (auto tz = search(zones, hare->target())) [[likely]]
		return tz;

	      // Follow the chain again:
	      hare = search(links, hare->target());

	      if (hare == tortoise)
		{
		  string_view err = "std::chrono::tzdb: link cycle: ";
		  string str;
		  str.reserve(err.size() + tz_name.size());
		  str += err;
		  str += tz_name;
		  __throw_runtime_error(str.c_str());
		}
	      // Plod along the chain one step:
	      tortoise = search(links, tortoise->target());
	    }
	}

      return nullptr; // not found
    }

#ifdef _GLIBCXX_HAVE_WINDOWS_H
    string_view
    detect_windows_zone() noexcept
    {
      DYNAMIC_TIME_ZONE_INFORMATION information{};
      if (GetDynamicTimeZoneInformation(&information) == TIME_ZONE_ID_INVALID)
	return {};

      constexpr SYSTEMTIME all_zero_time{};
      const wstring_view zone_name{ information.TimeZoneKeyName };
      auto equal = [](const SYSTEMTIME &lhs, const SYSTEMTIME &rhs) noexcept
	{ return memcmp(&lhs, &rhs, sizeof(SYSTEMTIME)) == 0; };
      // The logic is copied from icu, couldn't find the source.
      // Detect if DST is disabled.
      if (information.DynamicDaylightTimeDisabled
	  && equal(information.StandardDate, information.DaylightDate)
	  && ((!zone_name.empty()
	       && equal(information.StandardDate, all_zero_time))
	      || (zone_name.empty()
		  && !equal(information.StandardDate, all_zero_time))))
	{
	  if (information.Bias == 0)
	    return "Etc/UTC";

	  if (information.Bias % 60 != 0)
	    // If the offset is not in full hours, we can't do anything really.
	    return {};

	  const auto raw_index = information.Bias / 60;

	  // The bias added to the local time equals UTC. And GMT+X corresponds
	  // to UTC-X, the sign is negated. Thus we can use the hourly bias as
	  // an index into an array.
	  if (raw_index < 0 && raw_index >= -14)
	    {
	      static constexpr array<string_view, 14> table{
		"Etc/GMT-1",  "Etc/GMT-2",  "Etc/GMT-3",  "Etc/GMT-4",
		"Etc/GMT-5",  "Etc/GMT-6",  "Etc/GMT-7",  "Etc/GMT-8",
		"Etc/GMT-9",  "Etc/GMT-10", "Etc/GMT-11", "Etc/GMT-12",
		"Etc/GMT-13", "Etc/GMT-14"
	      };
	      return table[-raw_index - 1];
	    }
	  else if (raw_index > 0 && raw_index <= 12)
	    {
	      static constexpr array<string_view, 12> table{
		"Etc/GMT+1", "Etc/GMT+2",  "Etc/GMT+3",	 "Etc/GMT+4",
		"Etc/GMT+5", "Etc/GMT+6",  "Etc/GMT+7",	 "Etc/GMT+8",
		"Etc/GMT+9", "Etc/GMT+10", "Etc/GMT+11", "Etc/GMT+12"
	      };
	      return table[raw_index - 1];
	    }
	  return {};
	}

#include "windows_zones-map.h"
#ifndef _GLIBCXX_WINDOWS_ZONES_MAP_COMPLETE
# error "Invalid windows_zones map"
#endif

      const auto zone_range
	  = ranges::equal_range(windows_zone_map, zone_name, {},
				&windows_zone_map_entry::windows_name);

      const auto size = ranges::size(zone_range);
      if (size == 0)
	// Unknown zone, we can't detect anything.
	return {};

      if (size == 1)
	// Some zones have only one territory, use the quick path.
	return zone_range.front().iana_name;

      const auto geo_id = GetUserGeoID(GEOCLASS_NATION);
      // We ask for a 2-letter country code plus the zero terminator. "001" is
      // only contained in the zone map, not returned by GetGeoInfoW.
      wchar_t territory[3] = {};
      if (GetGeoInfoW(geo_id, GEO_ISO2, territory, 3, 0) == 0)
	// Couldn't detect the territory, fallback to "001", which is the first
	// entry.
	return zone_range.front().iana_name;

      const auto iter = ranges::lower_bound(
	  zone_range, +territory, {}, &windows_zone_map_entry::territory);
      if (iter == zone_range.end() || iter->territory != territory)
	// Territory not within the the map, use "001".
	return zone_range.front().iana_name;

      return iter->iana_name;
    }
#endif
  } // namespace

  // Implementation of std::chrono::tzdb::locate_zone(string_view).
  const time_zone*
  tzdb::locate_zone(string_view tz_name) const
  {
    if (auto tz = do_locate_zone(zones, links, tz_name))
      return tz;
    string_view err = "std::chrono::tzdb: cannot locate zone: ";
    string str;
    str.reserve(err.size() + tz_name.size());
    str += err;
    str += tz_name;
    __throw_runtime_error(str.c_str());
  }

  // Implementation of std::chrono::tzdb::current_zone().
  const time_zone*
  tzdb::current_zone() const
  {
    // TODO cache this function's result?
    // Could check the modification time of /etc/localtime, and not re-read
    // it if it hasn't changed. reload_tzdb() could clear the cache too,
    // to have a way to force a re-read.

#if !defined(_AIX) && !defined(_GLIBCXX_HAVE_WINDOWS_H)
    // /etc/localtime should be a symlink that ends with a zone name,
    // e.g. /etc/localtime -> /usr/share/zoneinfo/Europe/London
    // https://www.freedesktop.org/software/systemd/man/latest/localtime.html
    // This should work on GNU/Linux, macOS, NetBSD, and OpenBSD.
    // Some FreeBSD systems also use a symlink for /etc/localtime (since 15.0).

    // N.B. we do not support dangling symlinks here. If that becomes necessary
    // then after realpath fails we could fallback to using
    // filesystem::weakly_canonical(filesystem::read_symlink("etc/localtime")).

    string_view str;
#if defined _GLIBCXX_USE_REALPATH && _XOPEN_VERSION >= 700
    unique_ptr<char[], void(*)(void*)> cbuf{ nullptr, &::free };
    // Use realpath directly to avoid std::filesystem overhead.
    // We use realpath not readlink to resolve multiple levels of symlinks.
    if (char* p = ::realpath("/etc/localtime", nullptr))
      {
	cbuf.reset(p);
	str = p;
      }
#else
    error_code ec;
    string sbuf = std::filesystem::canonical("/etc/localtime", ec).string();
    if (!ec)
      str = sbuf;
#endif

    if (!str.empty() && str != "/etc/localtime")
      {
	// Check the trailing components of the path against known zone names.
	// Valid IANA times zones can have one, two, or three parts, e.g.
	// "UTC", "Europe/London", and "America/Indiana/Indianapolis".
	// Custom tzdata.zi files could in theory use four or more parts.

	auto pos = str.rfind('/');
	while (pos != str.npos && pos != 0)
	  {
	    if (auto tz = do_locate_zone(this->zones, this->links,
					 str.substr(pos + 1)))
	      return tz;
	    pos = str.rfind('/', pos - 1);
	  }
	// If we didn't match yet, try once more so that we will match
	// a symlink to a relative path such as "Europe/London"
	// or symlink to an absolute path such as "/Europe/London".
	// Both cases seem unlikely because it would require either
	// /etc/Europe or /Europe to be a directory (or a symlink to one)
	// containing the TZif files, but it's theoretically possible.
	// If pos==npos then pos+1 wraps to 0 and we use the whole string.
	// If pos==0 then substr(1) discards the leading slash.
	if (auto tz = do_locate_zone(this->zones, this->links,
				     str.substr(pos + 1)))
	  return tz;
      }

    // Otherwise, look for a file naming the time zone.
    string_view files[] {
      "/etc/timezone",    // Debian derivates, non-systemd Gentoo
      "/var/db/zoneinfo", // FreeBSD
    };
    for (auto f : files)
      {
	std::ifstream tzf{string{f}};
	if (std::string name; std::getline(tzf, name))
	  if (auto tz = do_locate_zone(this->zones, this->links, name))
	    return tz;
      }

    if (ifstream tzf{"/etc/sysconfig/clock"})
      {
	string line;
	// Old versions of Suse use TIMEZONE. Old versions of RHEL use ZONE.
	const string_view keys[] = { "TIMEZONE=" , "ZONE=" };
	while (std::getline(tzf, line))
	  for (string_view key : keys)
	    if (line.starts_with(key))
	      {
		string_view name = line;
		name.remove_prefix(key.size());
		if (name.size() != 0 && name.front() == '"')
		  {
		    name.remove_prefix(1);
		    if (auto pos = name.find('"'); pos != name.npos)
		      name = name.substr(0, pos);
		  }
		if (auto tz = do_locate_zone(this->zones, this->links, name))
		  return tz;
	      }
      }

    // FIXME: For DragonFly BSD /etc/localtime is a copy of one of the
    // zone files in /usr/share/zoneinfo so we need to compare its contents
    // to each one until we find a match.

#elif defined(_GLIBCXX_HAVE_WINDOWS_H)
    if (auto tz
	= do_locate_zone(this->zones, this->links, detect_windows_zone()))
      return tz;
#else // defined(_AIX)
    // AIX stores current zone in $TZ in /etc/environment but the value
    // is typically a POSIX time zone name, not IANA zone.
    // https://developer.ibm.com/articles/au-aix-posix/
    // https://www.ibm.com/support/pages/managing-time-zone-variable-posix
    if (const char* env = std::getenv("TZ"))
      {
	// This will fail unless TZ contains an IANA time zone name.
	if (auto tz = do_locate_zone(this->zones, this->links, env))
	  return tz;
      }
#endif

    // Default to UTC.
    if (auto tz = do_locate_zone(this->zones, this->links, "UTC"))
      return tz;

    __throw_runtime_error("tzdb: cannot determine current zone");
  }

  // Implementation of std::chrono::locate_zone(string_view)
  // TODO define this inline in the header instead?
  const time_zone*
  locate_zone(string_view tz_name)
  {
    // Use begin() so the tzdb cannot be erased while this operation runs.
    return get_tzdb_list().begin()->locate_zone(tz_name);
  }

  // Implementation of std::chrono::current_zone()
  // TODO define this inline in the header instead?
  const time_zone*
  current_zone()
  {
    // Use begin() so the tzdb cannot be erased while this operation runs.
    return get_tzdb_list().begin()->current_zone();
  }

#ifndef TZDB_DISABLED
  namespace
  {
    istream& operator>>(istream& in, abbrev_month& am)
    {
      string s;
      in >> s;
      switch (s[0])
      {
      case 'j':
      case 'J':
	switch (s[1])
	{
	case 'a':
	case 'A':
	  am.m = January;
	  return in;
	case 'u':
	case 'U':
	  switch (s[2])
	  {
	  case 'n':
	  case 'N':
	    am.m = June;
	    return in;
	  case 'l':
	  case 'L':
	    am.m = July;
	    return in;
	  }
	  break;
	}
	break;
      case 'f':
      case 'F':
	am.m = February;
	return in;
      case 'm':
      case 'M':
	if (s[1] == 'a' || s[1] == 'A') [[likely]]
	  switch (s[2])
	  {
	  case 'r':
	  case 'R':
	    am.m = March;
	    return in;
	  case 'y':
	  case 'Y':
	    am.m = May;
	    return in;
	  }
	break;
      case 'a':
      case 'A':
	switch (s[1])
	{
	case 'p':
	case 'P':
	  am.m = April;
	  return in;
	case 'u':
	case 'U':
	  am.m = August;
	  return in;
	}
	break;
      case 's':
      case 'S':
	am.m = September;
	return in;
      case 'o':
      case 'O':
	am.m = October;
	return in;
      case 'n':
      case 'N':
	am.m = November;
	return in;
      case 'd':
      case 'D':
	am.m = December;
	return in;
      }
      in.setstate(ios::failbit);
      return in;
    }

    // Wrapper for chrono::weekday that can be extracted from an istream
    // as an abbreviated weekday name.
    // The weekday name can be any unambiguous portion of a weekday name,
    // e.g. "M" (Monday) or "Su" (Sunday), but not "T" (Tuesday/Thursday).
    struct abbrev_weekday
    {
      weekday wd;

      friend istream& operator>>(istream& in, abbrev_weekday& aw)
      {
	// Do not read a whole word from the stream, in some cases
	// the weekday is only part of a larger word like "Sun<=25".
	// Just peek at one char at a time.
	switch (in.peek())
	{
	case 'm':
	case 'M':
	  aw.wd = Monday;
	  break;
	case 't':
	case 'T':
	  in.ignore(1); // Discard the 'T'
	  switch (in.peek())
	  {
	  case 'u':
	  case 'U':
	    aw.wd = Tuesday;
	    break;
	  case 'h':
	  case 'H':
	    aw.wd = Thursday;
	    break;
	  default:
	    in.setstate(ios::failbit);
	  }
	  break;
	case 'w':
	case 'W':
	  aw.wd = Wednesday;
	  break;
	case 'f':
	case 'F':
	  aw.wd = Friday;
	  break;
	case 's':
	case 'S':
	  in.ignore(1); // Discard the 'S'
	  switch (in.peek())
	  {
	  case 'a':
	  case 'A':
	    aw.wd = Saturday;
	    break;
	  case 'u':
	  case 'U':
	    aw.wd = Sunday;
	    break;
	  default:
	    in.setstate(ios::failbit);
	  }
	  break;
	default:
	  in.setstate(ios::failbit);
	}
	in.ignore(1); // Discard whichever char we just looked at.

	// Discard any remaining chars from weekday, e.g. "onday".
	string_view day_chars = "ondayesrituONDAYESRITU";
	auto is_day_char = [&day_chars](int c) {
	  return c != char_traits<char>::eof()
		   && day_chars.find((char)c) != day_chars.npos;
	};
	while (is_day_char(in.peek()))
	  in.ignore(1);

	return in;
      }
    };

    // Read the MONTH DAY. Three forms are accepted for DAY:
    // * a plain day-of-month number (DayOfMonth),
    // * "lastWww" where Www is a weekday name (LastWeekday),
    // * "Www<=N" or "Www>=N" (LessEq / GreaterEq).
    // On failure to read either MONTH or DAY this function sets
    // failbit. If DAY is not parsed, only `on.month` is modified,
    // otherwise `on` is left unchanged.
    istream& operator>>(istream& in, on_month_day& on)
    {
      using enum on_month_day::Kind;
      if (abbrev_month m{}; in >> m)
	{
 	  on.month = static_cast<unsigned>(m.m);
	  if (int c = ws(in).peek(); '0' <= c && c <= '9')
	    {
	      if (unsigned d; (in >> d) && (d <= 31)) [[likely]]
		{
		  on.kind = DayOfMonth;
		  on.day_of_month = d;
		  return in;
		}
	    }
	  else if (c == 'l' || c == 'L') // lastSunday, lastWed, ...
	    {
	      in.ignore(4);
	      if (abbrev_weekday w{}; in >> w) [[likely]]
		{
		  on.kind = LastWeekday;
		  on.day_of_week = w.wd.c_encoding();
		  return in;
		}
	    }
	  else if (abbrev_weekday w; in >> w) [[likely]]
	    {
	      if (c = in.get(); c == '<' || c == '>')
		if (in.get() == '=')
		  if (unsigned d; (in >> d) && (d <= 31)) [[likely]]
		    {
		      on.kind = c == '<' ? LessEq : GreaterEq;
		      on.day_of_week = w.wd.c_encoding();
		      on.day_of_month = d;
		      return in;
		    }
	    }
	}
      in.setstate(ios::failbit);
      return in;
    }

    istream& operator>>(istream& in, at_time& at)
    {
      int sign = 1;
      if (ws(in).peek() == '-')
	{
	  in.ignore(1);
	  if (auto [val, yes] = at_time::is_indicator(in.peek());
	      in.eof() || yes)
	    {
	      in.ignore(1);
	      at.time = 0s;
	      at.indicator = val;
	      return in;
	    }
	  sign = -1;
	}
      int i;
      in >> i;
      hours h{i};
      minutes m{};
      seconds s{};
      if (!in.eof() && in.peek() == ':')
	{
	  in.ignore(1); // discard the colon.
	  in >> i;
	  m = minutes{i};
	  if (!in.eof() && in.peek() == ':')
	    {
	      in.ignore(1); // discard the colon.
	      in >> i;
	      if (!in.eof() && in.peek() == '.')
		{
		  double frac;
		  in >> frac;
		  // zic(8) rounds to nearest second, rounding ties to even.
		  s = chrono::round<seconds>(duration<double>(i + frac));
		}
	      else
		s = seconds{i};
	    }
	}
      if (in >> at.indicator)
	at.time = sign * (h + m + s);
      return in;
    }

    // Test whether the RULES field of a Zone line is a valid Rule name.
    inline bool
    is_rule_name(string_view rules) noexcept
    {
      // The NAME field of a Rule line must start with a character that is
      // neither an ASCII digit nor '-' nor '+'.
      if (('0' <= rules[0] && rules[0] <= '9') || rules[0] == '-')
	return false;
      // However, some older tzdata.zi files (e.g. in tzdata-2018e-3.el6 RPM)
      // used "+" as a Rule name, so we need to handle that special case.
      if (rules[0] == '+')
	return rules.size() == 1; // "+" is a rule name, "+1" is not.
      // Everything else is the name of a Rule.
      return true;
    }

    istream& operator>>(istream& in, ZoneInfo& inf)
    {
      // STDOFF  RULES  FORMAT  [UNTIL]
      at_time off;
      string rules;
      string fmt;

      in >> off >> quoted{rules} >> fmt;
      inf.m_offset = off.time;
      if (is_rule_name(rules))
	{
	  // `rules` refers to a named Rule which describes transitions.
	  inf.set_rules_and_format(rules, fmt);
	}
      else
	{
	  if (rules == "-")
	    {
	      // Standard time always applies, no DST.
	      inf.m_save = minutes(0);
	    }
	  else
	    {
	      // `rules` specifies the difference from standard time,
	      // e.g., "-2:30"
	      at_time rules_time;
	      istringstream in2(std::move(rules));
	      in2 >> rules_time;
	      inf.m_save = duration_cast<minutes>(rules_time.time);
	      // If the FORMAT is "STD/DST" then we can choose the right one
	      // now, so that we store a shorter string.
	      select_std_or_dst_abbrev(fmt, inf.m_save);
	    }
	  inf.set_abbrev(std::move(fmt));
	}

      // YEAR [MONTH [DAY [TIME]]]
      ios::iostate ex = in.exceptions();
      in.exceptions(ios::goodbit); // Don't throw ios::failure if YEAR absent.
      if (int y = int(year::max()); in >> y)
	{
	  on_month_day on{ .kind = on_month_day::DayOfMonth,
			   .month = 1, .day_of_month = 1 };
	  at_time t{};
	  in >> on >> t;
	  year_month_day ymd = on.pin(year(y));
	  inf.m_until = sys_days(ymd) + seconds(t.time);
	  if (t.indicator != at_time::Universal)
	    { // UNTIL uses "the rules in effect just before the transition"
	      // adjust by STDOFF
	      inf.m_until -= seconds(inf.m_offset);
	      if (t.indicator != at_time::Standard)
		{
		  if (inf.expanded()) // Not a named Rule, SAVE is known now.
		    inf.m_until -= inf.m_save;
		  else // else Named Rule, SAVE is unknown, mark as pending
		    inf.m_state = ZoneInfo::UntilPending;
		}
	    }
	}
      else
	{
	  inf.m_until = sys_days(year::max()/December/31);
	  // The line does not define UNTIL, so it is not affected by save
	  if (!inf.expanded())
	    inf.m_state = ZoneInfo::SaveKnown;
	}

      in.clear(in.rdstate() & ios::eofbit);
      in.exceptions(ex);
      if (!in.eof())
	// Not actually necessary, as we're only parsing a single line:
	in.ignore(numeric_limits<streamsize>::max(), '\n');
      return in;
    }
  } // namespace
#endif // TZDB_DISABLED
} // namespace std::chrono

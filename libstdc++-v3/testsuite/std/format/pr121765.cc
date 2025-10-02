// { dg-do compile { target c++23 } }

#include <format>
#include <utility>

struct MyPair 
{
  int x;
  int y;
};

template<typename CharT>
struct std::formatter<MyPair, CharT> 
{
  template<typename ParseContext>
  auto parse(ParseContext& pc)
  { return _formatter.parse(pc);  }

  template<typename FormatContext>
  auto format(const MyPair& mp, FormatContext& fc) const
  { return _formatter.format(std::make_pair(mp.x, mp.y), fc); }

private:
  std::formatter<std::pair<int, int>, CharT> _formatter;
};

static_assert(std::formattable<MyPair, char>);
static_assert(std::formattable<MyPair, wchar_t>);

struct MyRange
{
  int* begin;
  int* end;
};

template<typename CharT>
struct std::formatter<MyRange, CharT> 
{
  template<typename ParseContext>
  auto parse(ParseContext& pc)
  { return _formatter.parse(pc);  }

  template<typename FormatContext>
  auto format(const MyRange& mp, FormatContext& fc) const
  { return _formatter.format(std::span<int>(mp.begin, mp.end), fc); }

private:
  std::formatter<std::span<int>, CharT> _formatter;
};

static_assert(std::formattable<MyRange, char>);
static_assert(std::formattable<MyRange, wchar_t>);


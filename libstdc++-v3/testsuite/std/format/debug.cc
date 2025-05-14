// { dg-options "-fexec-charset=UTF-8 -fwide-exec-charset=UTF-32LE -DUNICODE_ENC" { target le } }
// { dg-options "-fexec-charset=UTF-8 -fwide-exec-charset=UTF-32BE -DUNICODE_ENC" { target be } }
// { dg-do run { target c++23 } }
// { dg-add-options no_pch }
// { dg-timeout-factor 2 }

#include <format>
#include <testsuite_hooks.h>

std::string
fdebug(char t)
{ return std::format("{:?}", t); }

std::wstring
fdebug(wchar_t t)
{ return std::format(L"{:?}", t); }

std::string
fdebug(std::string_view t)
{ return std::format("{:?}", t); }

std::wstring
fdebug(std::wstring_view t)
{ return std::format(L"{:?}", t); }


#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

template<typename _CharT>
void
test_basic_escapes()
{
  std::basic_string<_CharT> res;

  const auto tab = WIDEN("\t");
  res = fdebug(tab);
  VERIFY( res == WIDEN(R"("\t")") );
  res = fdebug(tab[0]);
  VERIFY( res == WIDEN(R"('\t')") );

  const auto nline = WIDEN("\n");
  res = fdebug(nline);
  VERIFY( res == WIDEN(R"("\n")") );
  res = fdebug(nline[0]);
  VERIFY( res == WIDEN(R"('\n')") );

  const auto carret = WIDEN("\r");
  res = fdebug(carret);
  VERIFY( res == WIDEN(R"("\r")") );
  res = fdebug(carret[0]);
  VERIFY( res == WIDEN(R"('\r')") );

  const auto bslash = WIDEN("\\");
  res = fdebug(bslash);
  VERIFY( res == WIDEN(R"("\\")") );
  res = fdebug(bslash[0]);
  VERIFY( res == WIDEN(R"('\\')") );

  const auto quote = WIDEN("\"");
  res = fdebug(quote);
  VERIFY( res == WIDEN(R"("\"")") );
  res = fdebug(quote[0]);
  VERIFY( res == WIDEN(R"('"')") );

  const auto apos = WIDEN("\'");
  res = fdebug(apos);
  VERIFY( res == WIDEN(R"("'")") );
  res = fdebug(apos[0]);
  VERIFY( res == WIDEN(R"('\'')") );
}

template<typename _CharT>
void
test_ascii_escapes()
{
  std::basic_string<_CharT> res;

  const auto in = WIDEN("\x10 abcde\x7f\t0123");
  res = fdebug(in);
  VERIFY( res == WIDEN(R"("\u{10} abcde\u{7f}\t0123")") );
  res = fdebug(in[0]);
  VERIFY( res == WIDEN(R"('\u{10}')") );
  res = fdebug(in[1]);
  VERIFY( res == WIDEN(R"(' ')") );
  res = fdebug(in[2]);
  VERIFY( res == WIDEN(R"('a')") );
}

template<typename _CharT>
void
test_extended_ascii()
{
  std::basic_string<_CharT> res;

  const auto in = WIDEN("Åëÿ");
  res = fdebug(in);
  VERIFY( res == WIDEN(R"("Åëÿ")") );

  static constexpr bool __test_characters
#if UNICODE_ENC
    = sizeof(_CharT) >= 2;
#else // ISO8859-1
    = true;
#endif // UNICODE_ENC

  if constexpr (__test_characters)
  {
    res = fdebug(in[0]);
    VERIFY( res == WIDEN(R"('Å')") );
    res = fdebug(in[1]);
    VERIFY( res == WIDEN(R"('ë')") );
    res = fdebug(in[2]);
    VERIFY( res == WIDEN(R"('ÿ')") );
  }
}

template<typename _CharT>
void
test_unicode_escapes()
{
#if UNICODE_ENC
  std::basic_string<_CharT> res;

  const auto in = WIDEN(
    "\u008a"     // Cc, Control,             Line Tabulation Set,
    "\u00ad"     // Cf, Format,              Soft Hyphen
    "\u1d3d"     // Lm, Modifier letter,     Modifier Letter Capital Ou
    "\u00a0"     // Zs, Space Separator,     No-Break Space (NBSP)
    "\u2029"     // Zp, Paragraph Separator, Paragraph Separator
    "\U0001f984" // So, Other Symbol,        Unicorn Face
  );
  const auto out = WIDEN("\""
   R"(\u{8a})"
   R"(\u{ad})"
   "\u1d3d"
   R"(\u{a0})"
   R"(\u{2029})"
   "\U0001f984"
  "\"");

  res = fdebug(in);
  VERIFY( res == out );

  if constexpr (sizeof(_CharT) >= 2)
  {
    res = fdebug(in[0]);
    VERIFY( res == WIDEN(R"('\u{8a}')") );
    res = fdebug(in[1]);
    VERIFY( res == WIDEN(R"('\u{ad}')") );
    res = fdebug(in[2]);
    VERIFY( res == WIDEN("'\u1d3d'") );
    res = fdebug(in[3]);
    VERIFY( res == WIDEN(R"('\u{a0}')") );
    res = fdebug(in[4]);
    VERIFY( res == WIDEN(R"('\u{2029}')") );
  }

  if constexpr (sizeof(_CharT) >= 4)
  {
    res = fdebug(in[5]);
    VERIFY( res == WIDEN("'\U0001f984'") );
  }
#endif // UNICODE_ENC
}

template<typename _CharT>
void
test_grapheme_extend()
{
#if UNICODE_ENC
  std::basic_string<_CharT> res;

  const auto vin = WIDEN("o\u0302\u0323");
  res = fdebug(vin);
  VERIFY( res == WIDEN("\"o\u0302\u0323\"") );

  std::basic_string_view<_CharT> in = WIDEN("\t\u0302\u0323");
  res = fdebug(in);
  VERIFY( res == WIDEN(R"("\t\u{302}\u{323}")") );

  res = fdebug(in.substr(1));
  VERIFY( res == WIDEN(R"("\u{302}\u{323}")") );

  if constexpr (sizeof(_CharT) >= 2)
  {
    res = fdebug(in[1]);
    VERIFY( res == WIDEN(R"('\u{302}')") );
  }
#endif // UNICODE_ENC
}

template<typename _CharT>
void
test_replacement_char()
{
#if UNICODE_ENC
  std::basic_string<_CharT> repl = WIDEN("\uFFFD");
  std::basic_string<_CharT> res = fdebug(repl);
  VERIFY( res == WIDEN("\"\uFFFD\"") );

  repl = WIDEN("\uFFFD\uFFFD");
  res = fdebug(repl);
  VERIFY( res == WIDEN("\"\uFFFD\uFFFD\"") );
#endif // UNICODE_ENC
}

void
test_ill_formed_utf8_seq()
{
#if UNICODE_ENC
  std::string_view seq = "\xf0\x9f\xa6\x84"; //  \U0001F984
  std::string res;

  res = fdebug(seq);
  VERIFY( res == "\"\U0001F984\"" );

  res = fdebug(seq.substr(1));
  VERIFY( res == R"("\x{9f}\x{a6}\x{84}")" );

  res = fdebug(seq.substr(2));
  VERIFY( res == R"("\x{a6}\x{84}")" );

  res = fdebug(seq[0]);
  VERIFY( res == R"('\x{f0}')" );
  res = fdebug(seq.substr(0, 1));
  VERIFY( res == R"("\x{f0}")" );

  res = fdebug(seq[1]);
  VERIFY( res == R"('\x{9f}')" );
  res = fdebug(seq.substr(1, 1));
  VERIFY( res == R"("\x{9f}")" );

  res = fdebug(seq[2]);
  VERIFY( res == R"('\x{a6}')" );
  res = fdebug(seq.substr(2, 1));
  VERIFY( res == R"("\x{a6}")" );

  res = fdebug(seq[3]);
  VERIFY( res == R"('\x{84}')" );
  res = fdebug(seq.substr(3, 1));
  VERIFY( res == R"("\x{84}")" );
#endif // UNICODE_ENC
}

void
test_ill_formed_utf32()
{
#if UNICODE_ENC
  std::wstring res;

  wchar_t ic1 = static_cast<wchar_t>(0xff'ffff);
  res = fdebug(ic1);
  VERIFY( res == LR"('\x{ffffff}')" );

  std::wstring is1(1, ic1);
  res = fdebug(is1);
  VERIFY( res == LR"("\x{ffffff}")" );

  wchar_t ic2 = static_cast<wchar_t>(0xffff'ffff);
  res = fdebug(ic2);
  VERIFY( res == LR"('\x{ffffffff}')" );

  std::wstring is2(1, ic2);
  res = fdebug(is2);
  VERIFY( res == LR"("\x{ffffffff}")" );
#endif // UNICODE_ENC
}

template<typename _CharT>
void
test_fill()
{
  std::basic_string<_CharT> res;

  std::basic_string_view<_CharT> in = WIDEN("a\t\x10\u00ad");
  res = std::format(WIDEN("{:10?}"), in.substr(0, 1));
  VERIFY( res == WIDEN(R"("a"       )") );

  res = std::format(WIDEN("{:->10?}"), in.substr(1, 1));
  VERIFY( res == WIDEN(R"(------"\t")") );

  res = std::format(WIDEN("{:+<10?}"), in.substr(2, 1));
  VERIFY( res == WIDEN(R"("\u{10}"++)") );


  res = std::format(WIDEN("{:10?}"), in[0]);
  VERIFY( res == WIDEN(R"('a'       )") );

  res = std::format(WIDEN("{:->10?}"), in[1]);
  VERIFY( res == WIDEN(R"(------'\t')") );

  res = std::format(WIDEN("{:+<10?}"), in[2]);
  VERIFY( res == WIDEN(R"('\u{10}'++)") );

#if UNICODE_ENC
  res = std::format(WIDEN("{:=^10?}"), in.substr(3));
  VERIFY( res == WIDEN(R"(="\u{ad}"=)") );

  // width is 2
  std::basic_string_view<_CharT> in2 = WIDEN("\u1100");
  res = std::format(WIDEN("{:*^10?}"), in2);
  VERIFY( res == WIDEN("***\"\u1100\"***") );

  if constexpr (sizeof(_CharT) >= 2)
  {
    res = std::format(WIDEN("{:=^10?}"), in[3]);
    VERIFY( res == WIDEN(R"(='\u{ad}'=)") );

    res = std::format(WIDEN("{:*^10?}"), in2[0]);
    VERIFY( res == WIDEN("***'\u1100'***") );
  }
#endif // UNICODE_ENC
}

template<typename _CharT>
void
test_prec()
{
  std::basic_string<_CharT> res;
  // with ? escpaed presentation is copied to ouput, same as source

  std::basic_string_view<_CharT> in = WIDEN("a\t\x10\u00ad");
  res = std::format(WIDEN("{:.2?}"), in.substr(0, 1));
  VERIFY( res == WIDEN(R"("a)") );

  res = std::format(WIDEN("{:.4?}"), in.substr(1, 1));
  VERIFY( res == WIDEN(R"("\t")") );

  res = std::format(WIDEN("{:.5?}"), in.substr(2, 1));
  VERIFY( res == WIDEN(R"("\u{1)") );

#if UNICODE_ENC
  res = std::format(WIDEN("{:.10?}"), in.substr(3));
  VERIFY( res == WIDEN(R"("\u{ad}")") );

  std::basic_string_view<_CharT> in2 = WIDEN("\u1100");
  res = std::format(WIDEN("{:.3?}"), in2);
  VERIFY( res == WIDEN("\"\u1100") );
#endif // UNICODE_ENC
}

bool strip_quote(std::string_view& v)
{
  if (!v.starts_with('"'))
    return false;
  v.remove_prefix(1);
  return true;
}

bool strip_quotes(std::string_view& v)
{
  if (!v.starts_with('"') || !v.ends_with('"'))
    return false;
  v.remove_prefix(1);
  v.remove_suffix(1);
  return true;
}

bool strip_prefix(std::string_view& v, size_t n, char c)
{
  size_t pos = v.find_first_not_of(c);
  if (pos == std::string_view::npos)
    pos = v.size();
  if (pos != n)
    return false;
  v.remove_prefix(n);
  return true;
}

void test_padding()
{
  std::string res;
  std::string_view resv;

  // width and size are 26
  std::string in = "abcdefghijklmnopqrstuvwxyz";
  in += in; // width and size are 52
  in += in; // width and size are 104
  in += in; // width and size are 208
  in += in; // width and size are 416
  std::string_view inv = in;

  resv = res = std::format("{}", in);
  VERIFY( resv == inv );

  resv = res = std::format("{:.500}", in);
  VERIFY( resv == inv );

  resv = res = std::format("{:.400}", in);
  VERIFY( resv == inv.substr(0, 400) );

  resv = res = std::format("{:.200}", in);
  VERIFY( resv == inv.substr(0, 200) );

  resv = res = std::format("{:.10}", in);
  VERIFY( resv == inv.substr(0, 10) );

  resv = res = std::format("{:.0}", in);
  VERIFY( resv == "" );

  resv = res = std::format("{:*>20}", in);
  VERIFY( resv == inv );

  resv = res = std::format("{:*>20.500}", in);
  VERIFY( resv == inv );

  resv = res = std::format("{:*>20.400}", in);
  VERIFY( resv == inv.substr(0, 400) );

  resv = res = std::format("{:*>20.200}", in);
  VERIFY( resv == inv.substr(0, 200) );

  resv = res = std::format("{:*>20.10}", in);
  VERIFY( strip_prefix(resv, 10, '*') );
  VERIFY( resv == inv.substr(0, 10) );

  resv = res = std::format("{:*>20.0}", in);
  VERIFY( strip_prefix(resv, 20, '*') );
  VERIFY( resv == "" );

  resv = res = std::format("{:*>450}", in);
  VERIFY( strip_prefix(resv, 34, '*') );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>450.500}", in);
  VERIFY( strip_prefix(resv, 34, '*') );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>450.420}", in);
  VERIFY( strip_prefix(resv, 34, '*') );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>450.400}", in);
  VERIFY( strip_prefix(resv, 50, '*') );
  VERIFY( resv == inv.substr(0, 400) );

  resv = res = std::format("{:*>450.200}", in);
  VERIFY( strip_prefix(resv, 250, '*') );
  VERIFY( resv == inv.substr(0, 200) );

  resv = res = std::format("{:*>450.10}", in);
  VERIFY( strip_prefix(resv, 440, '*') );
  VERIFY( resv == inv.substr(0, 10) );

  resv = res = std::format("{:*>450.0}", in);
  VERIFY( strip_prefix(resv, 450, '*') );
  VERIFY( resv == "" );

  resv = res = std::format("{:?}", in);
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:.500?}", in);
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:.400?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 399) );

  resv = res = std::format("{:.200?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 199) );

  resv = res = std::format("{:.10?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 9) );

  resv = res = std::format("{:.1?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == "" );

  resv = res = std::format("{:.0?}", in);
  VERIFY( resv == "" );

  resv = res = std::format("{:*>20?}", in);
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>20.500?}", in);
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>20.400?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 399) );

  resv = res = std::format("{:*>20.200?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 199) );

  resv = res = std::format("{:*>20.10?}", in);
  VERIFY( strip_prefix(resv, 10, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 9) );

  resv = res = std::format("{:*>20.1?}", in);
  VERIFY( strip_prefix(resv, 19, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == "" );

  resv = res = std::format("{:*>20.0?}", in);
  VERIFY( strip_prefix(resv, 20, '*') );
  VERIFY( resv == "" );

  resv = res = std::format("{:*>450?}", in);
  VERIFY( strip_prefix(resv, 32, '*') );
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>450.500?}", in);
  VERIFY( strip_prefix(resv, 32, '*') );
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>450.420?}", in);
  VERIFY( strip_prefix(resv, 32, '*') );
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>450.400?}", in);
  VERIFY( strip_prefix(resv, 50, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 399) );

  resv = res = std::format("{:*>450.200?}", in);
  VERIFY( strip_prefix(resv, 250, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 199) );

  resv = res = std::format("{:*>450.10?}", in);
  VERIFY( strip_prefix(resv, 440, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 9) );

  resv = res = std::format("{:*>450.1?}", in);
  VERIFY( strip_prefix(resv, 449, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == "" );

  resv = res = std::format("{:*>450.0?}", in);
  VERIFY( strip_prefix(resv, 450, '*') );
  VERIFY( resv == "" );

#if UNICODE_ENC
  // width is 3, size is 15
  in = "o\u0302\u0323i\u0302\u0323u\u0302\u0323";
  in += in; // width is 6, size is 30
  in += in; // width is 12, size is 60
  in += in; // width is 24, size is 120
  in += in; // width is 48, size is 240
  in += in; // width is 96, size is 480
  in += in; // width is 192, size is 960
  inv = in;

  resv = res = std::format("{:}", in);
  VERIFY( resv == inv );

  resv = res = std::format("{:.200}", in);
  VERIFY( resv == inv );

  resv = res = std::format("{:.96}", in);
  VERIFY( resv == inv.substr(0, 480) );

  resv = res = std::format("{:.12}", in);
  VERIFY( resv == inv.substr(0, 60) );

  resv = res = std::format("{:.3}", in);
  VERIFY( resv == inv.substr(0, 15) );

  resv = res = std::format("{:.0}", in);
  VERIFY( resv == "" );

  resv = res = std::format("{:*>10}", in);
  VERIFY( resv == inv );

  resv = res = std::format("{:*>10.200}", in);
  VERIFY( resv == inv );

  resv = res = std::format("{:*>10.96}", in);
  VERIFY( resv == inv.substr(0, 480) );

  resv = res = std::format("{:*>10.12}", in);
  VERIFY( resv == inv.substr(0, 60) );

  resv = res = std::format("{:*>10.3}", in);
  VERIFY( strip_prefix(resv, 7, '*') );
  VERIFY( resv == inv.substr(0, 15) );

  resv = res = std::format("{:*>10.0}", in);
  VERIFY( strip_prefix(resv, 10, '*') );
  VERIFY( resv == "" );

  resv = res = std::format("{:*>240s}", in);
  VERIFY( strip_prefix(resv, 48, '*') );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>300.200s}", in);
  VERIFY( strip_prefix(resv, 108, '*') );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>240.200s}", in);
  VERIFY( strip_prefix(resv, 48, '*') );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>240.96s}", in);
  VERIFY( strip_prefix(resv, 144, '*') );
  VERIFY( resv == inv.substr(0, 480) );

  resv = res = std::format("{:*>240.12}", in);
  VERIFY( strip_prefix(resv, 228, '*') );
  VERIFY( resv == inv.substr(0, 60) );

  resv = res = std::format("{:*>240.3s}", in);
  VERIFY( strip_prefix(resv, 237, '*') );
  VERIFY( resv == inv.substr(0, 15) );

  resv = res = std::format("{:*>240.0s}", in);
  VERIFY( strip_prefix(resv, 240, '*') );
  VERIFY( resv == "" );

  resv = res = std::format("{:?}", in);
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:.200?}", in);
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:.97?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 480) );

  resv = res = std::format("{:.13?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 60) );

  resv = res = std::format("{:.4?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 15) );

  resv = res = std::format("{:.1?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == "" );

  resv = res = std::format("{:.0?}", in);
  VERIFY( resv == "" );

  resv = res = std::format("{:*>10?}", in);
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>10.200?}", in);
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>10.97?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 480) );

  resv = res = std::format("{:*>10.13?}", in);
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 60) );

  resv = res = std::format("{:*>10.4?}", in);
  VERIFY( strip_prefix(resv, 6, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 15) );

  resv = res = std::format("{:*>10.1?}", in);
  VERIFY( strip_prefix(resv, 9, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == "" );

  resv = res = std::format("{:*>10.0?}", in);
  VERIFY( strip_prefix(resv, 10, '*') );
  VERIFY( resv == "" );

  resv = res = std::format("{:*>240?}", in);
  VERIFY( strip_prefix(resv, 46, '*') );
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>300.200?}", in);
  VERIFY( strip_prefix(resv, 106, '*') );
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>240.200?}", in);
  VERIFY( strip_prefix(resv, 46, '*') );
  VERIFY( strip_quotes(resv) );
  VERIFY( resv == inv );

  resv = res = std::format("{:*>240.97?}", in);
  VERIFY( strip_prefix(resv, 143, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 480) );

  resv = res = std::format("{:*>240.13?}", in);
  VERIFY( strip_prefix(resv, 227, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 60) );

  resv = res = std::format("{:*>240.4?}", in);
  VERIFY( strip_prefix(resv, 236, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == inv.substr(0, 15) );

  resv = res = std::format("{:*>240.1?}", in);
  VERIFY( strip_prefix(resv, 239, '*') );
  VERIFY( strip_quote(resv) );
  VERIFY( resv == "" );

  resv = res = std::format("{:*>240.0?}", in);
  VERIFY( strip_prefix(resv, 240, '*') );
  VERIFY( resv == "" );
#endif // UNICODE_ENC
}

void test_char_as_wchar()
{
  std::wstring res;

  res = std::format(L"{:?}", 'a');
  VERIFY( res == LR"('a')" );

  res = std::format(L"{:?}", '\t');
  VERIFY( res == LR"('\t')" );

  res = std::format(L"{:+<10?}", '\x10');
  VERIFY( res == LR"('\u{10}'++)" );
}

template<typename T>
struct DebugWrapper
{
  T val;
};

template<typename T, typename CharT>
struct std::formatter<DebugWrapper<T>, CharT>
{
  constexpr std::basic_format_parse_context<CharT>::iterator
  parse(std::basic_format_parse_context<CharT>& pc)
  {
    auto out = under.parse(pc);
    under.set_debug_format();
    return out;
  }

  template<typename Out>
  Out format(DebugWrapper<T> const& t,
	     std::basic_format_context<Out, CharT>& fc) const
  { return under.format(t.val, fc); }

private:
  std::formatter<T, CharT> under;
};

template<typename _CharT, typename StrT>
void
test_formatter_str()
{
  _CharT buf[]{ 'a', 'b', 'c', 0 };
  DebugWrapper<StrT> in{ buf };
  std::basic_string<_CharT> res = std::format(WIDEN("{:?}"), in );
  VERIFY( res == WIDEN(R"("abc")") );
}

template<typename _CharT>
void
test_formatter_arr()
{
  std::basic_string<_CharT> res;

  DebugWrapper<_CharT[3]> in3{ 'a', 'b', 'c' };
  res = std::format(WIDEN("{:?}"), in3 );
  VERIFY( res == WIDEN(R"("abc")") );

  // We print all characters, including null-terminator
  DebugWrapper<_CharT[4]> in4{ 'a', 'b', 'c', 0 };
  res = std::format(WIDEN("{:?}"), in4 );
  VERIFY( res == WIDEN(R"("abc\u{0}")") );
}

template<typename _CharT, typename SrcT>
void
test_formatter_char()
{
  DebugWrapper<SrcT> in{ 'a' };
  std::basic_string<_CharT> res = std::format(WIDEN("{:?}"), in);
  VERIFY( res == WIDEN(R"('a')") );
}

template<typename CharT>
void
test_formatters()
{
  test_formatter_char<CharT, CharT>();
  test_formatter_str<CharT, CharT*>();
  test_formatter_str<CharT, const CharT*>();
  test_formatter_str<CharT, std::basic_string<CharT>>();
  test_formatter_str<CharT, std::basic_string_view<CharT>>();
  test_formatter_arr<CharT>();
}

void
test_formatters_c()
{
  test_formatters<char>();
  test_formatters<wchar_t>();
  test_formatter_char<wchar_t, char>();
}

int main()
{
  test_basic_escapes<char>();
  test_basic_escapes<wchar_t>();
  test_ascii_escapes<char>();
  test_ascii_escapes<wchar_t>();
  test_extended_ascii<char>();
  test_extended_ascii<wchar_t>();

  test_unicode_escapes<char>();
  test_unicode_escapes<wchar_t>();
  test_grapheme_extend<char>();
  test_grapheme_extend<wchar_t>();
  test_replacement_char<char>();
  test_replacement_char<wchar_t>();
  test_ill_formed_utf8_seq();
  test_ill_formed_utf32();

  test_fill<char>();
  test_fill<wchar_t>();
  test_prec<char>();
  test_prec<wchar_t>();

  test_padding();

  test_formatters_c();
}

// CODYlib		-*- mode:c++ -*-
// Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
// License: Apache v2.0

// Cody
#include "internal.hh"
// C++
#include <algorithm>
// C
#include <cstring>
// OS
#include <unistd.h>
#include <cerrno>

// MessageBuffer code

// Lines consist of words and end with a NEWLINE (0xa) char
// Whitespace characters are TAB (0x9) and SPACE (0x20)
// Words consist of non-whitespace chars separated by whitespace.
// Multiple lines in one transaction are indicated by ending non-final
// lines with a SEMICOLON (0x3b) word, immediately before the NEWLINE
// Continuations with ; preceding it
// Words matching regexp [-+_/%.a-zA-Z0-9]+ need no quoting.
// Quoting with '...'
// Anything outside of [-+_/%.a-zA-Z0-9] needs quoting
// Anything outside of <= <space> or DEL or \' or \\ needs escaping.
// Escapes are \\, \', \n, \t, \_, everything else as \<hex><hex>?
// Spaces separate words, UTF8 encoding for non-ascii chars

namespace Cody {
namespace Detail {

static const char CONTINUE = S2C(u8";");

void MessageBuffer::BeginLine ()
{
  if (!buffer.empty ())
    {
      // Terminate the previous line with a continuation
      buffer.reserve (buffer.size () + 3);
      buffer.push_back (S2C(u8" "));
      buffer.push_back (CONTINUE);
      buffer.push_back (S2C(u8"\n"));
    }
  lastBol = buffer.size ();
}

// QUOTE means 'maybe quote', we search it for quote-needing chars

void MessageBuffer::Append (char const *str, bool quote, size_t len)
{
  if (len == ~size_t (0))
    len = strlen (str);

  if (!len && !quote)
    return;

  // We want to quote characters outside of [-+_A-Za-z0-9/%.], anything
  // that could remotely be shell-active.  UTF8 encoding for non-ascii.
  if (quote && len)
    {
      quote = false;
      // Scan looking for quote-needing characters.  We could just
      // append until we find one, but that's probably confusing
      for (size_t ix = len; ix--;)
	{
	  unsigned char c = (unsigned char)str[ix];
	  if (!((c >= S2C(u8"a") && c <= S2C(u8"z"))
		|| (c >= S2C(u8"A") && c <= S2C(u8"Z"))
		|| (c >= S2C(u8"0") && c <= S2C(u8"9"))
		|| c == S2C(u8"-") || c == S2C(u8"+") || c == S2C(u8"_")
		|| c == S2C(u8"/") || c == S2C(u8"%") || c == S2C(u8".")))
	    {
	      quote = true;
	      break;
	    }
	}
    }

  // Maximal length of appended string
  buffer.reserve (buffer.size () + len * (quote ? 3 : 1) + 2);

  if (quote)
    buffer.push_back (S2C(u8"'"));

  for (auto *end = str + len; str != end;)
    {
      auto *e = end;

      if (quote)
	// Look for next escape-needing char.  More relaxed than
	// the earlier needs-quoting check.
	for (e = str; e != end; ++e)
	  {
	    unsigned char c = (unsigned char)*e;
	    if (c < S2C(u8" ") || c == 0x7f
		|| c == S2C(u8"\\") || c == S2C(u8"'"))
	      break;
	  }
      buffer.insert (buffer.end (), str, e);
      str = e;

      if (str == end)
	break;

      buffer.push_back (S2C(u8"\\"));
      switch (unsigned char c = (unsigned char)*str++)
	{
	case S2C(u8"\t"):
	  c = S2C(u8"t");
	  goto append;

	case S2C(u8"\n"):
	  c = S2C(u8"n");
	  goto append;

	case S2C(u8"'"):
	case S2C(u8"\\"):
	append:
	  buffer.push_back (c);
	  break;

	default:
	  // Full-on escape.  Use 2 lower-case hex chars
	  for (unsigned shift = 8; shift;)
	    {
	      shift -= 4;

	      char nibble = (c >> shift) & 0xf;
	      nibble += S2C(u8"0");
	      if (nibble > S2C(u8"9"))
		nibble += S2C(u8"a") - (S2C(u8"9") + 1);
	      buffer.push_back (nibble);
	    }
	}
    }

  if (quote)
    buffer.push_back (S2C(u8"'"));
}

void MessageBuffer::Append (char c)
{
  buffer.push_back (c);
}

void MessageBuffer::AppendInteger (unsigned u)
{
  // Sigh, even though std::to_string is C++11, we support building on
  // gcc 4.8, which is a C++11 compiler lacking std::to_string.  so
  // have something horrible.
  std::string v (20, 0);
  size_t len = snprintf (const_cast<char *> (v.data ()), v.size (), "%u", u);
  v.erase (len);

  AppendWord (v);
}

int MessageBuffer::Write (int fd) noexcept
{
  size_t limit = buffer.size () - lastBol;
  ssize_t count = write (fd, &buffer.data ()[lastBol], limit);

  int err = 0;
  if (count < 0)
    err = errno;
  else
    {
      lastBol += count;
      if (size_t (count) != limit)
	err = EAGAIN;
    }

  if (err != EAGAIN && err != EINTR)
    {
      // Reset for next message
      buffer.clear ();
      lastBol = 0;
    }

  return err;
}

int MessageBuffer::Read (int fd) noexcept
{
  constexpr size_t blockSize = 200;

  size_t lwm = buffer.size ();
  size_t hwm = buffer.capacity ();
  if (hwm - lwm < blockSize / 2)
    hwm += blockSize;
  buffer.resize (hwm);

  auto iter = buffer.begin () + lwm;
  ssize_t count = read (fd, &*iter, hwm - lwm);
  buffer.resize (lwm + (count >= 0 ? count : 0));

  if (count < 0)
    return errno;

  if (!count)
    // End of file
    return -1;

  bool more = true;
  for (;;)
    {
      auto newline = std::find (iter, buffer.end (), S2C(u8"\n"));
      if (newline == buffer.end ())
	break;
      more = newline != buffer.begin () && newline[-1] == CONTINUE;
      iter = newline + 1;
	
      if (iter == buffer.end ())
	break;

      if (!more)
	{
	  // There is no continuation, but there are chars after the
	  // newline.  Truncate the buffer and return an error
	  buffer.resize (iter - buffer.begin ());
	  return EINVAL;
	}
    }

  return more ? EAGAIN : 0;
}

int MessageBuffer::Lex (std::vector<std::string> &result)
{
  result.clear ();

  if (IsAtEnd ())
    return ENOENT;

  Assert (buffer.back () == S2C(u8"\n"));

  auto iter = buffer.begin () + lastBol;

  for (std::string *word = nullptr;;)
    {
      char c = *iter;

      ++iter;
      if (c == S2C(u8" ") || c == S2C(u8"\t"))
	{
	  word = nullptr;
	  continue;
	}

      if (c == S2C(u8"\n"))
	break;

      if (c == CONTINUE)
	{
	  // Line continuation
	  if (word || *iter != S2C(u8"\n"))
	    goto malformed;
	  ++iter;
	  break;
	}

      if (c <= S2C(u8" ") || c >= 0x7f)
	goto malformed;

      if (!word)
	{
	  result.emplace_back ();
	  word = &result.back ();
	}

      if (c == S2C(u8"'"))
	{
	  // Quoted word
	  for (;;)
	    {
	      c = *iter;

	      if (c == S2C(u8"\n"))
		{
		malformed:;
		  result.clear ();
		  iter = std::find (iter, buffer.end (), S2C(u8"\n"));
		  auto back = iter;
		  if (back[-1] == CONTINUE  && back[-2] == S2C(u8" "))
		    // Smells like a line continuation
		    back -= 2;
		  result.emplace_back (&buffer[lastBol],
				       back - buffer.begin () - lastBol);
		  ++iter;
		  lastBol = iter - buffer.begin ();
		  return EINVAL;
		}

	      if (c < S2C(u8" ") || c >= 0x7f)
		goto malformed;

	      ++iter;
	      if (c == S2C(u8"'"))
		break;

	      if (c == S2C(u8"\\"))
		// escape
		switch (c = *iter)
		  {
		    case S2C(u8"\\"):
		    case S2C(u8"'"):
		      ++iter;
		      break;

		    case S2C(u8"n"):
		      c = S2C(u8"\n");
		      ++iter;
		      break;

		    case S2C(u8"_"):
		      // We used to escape SPACE as \_, so accept that
		      c = S2C(u8" ");
		      ++iter;
		      break;

		    case S2C(u8"t"):
		      c = S2C(u8"\t");
		      ++iter;
		      break;

		    default:
		      {
			unsigned v = 0;
			for (unsigned nibble = 0; nibble != 2; nibble++)
			  {
			    c = *iter;
			    if (c < S2C(u8"0"))
			      {
				if (!nibble)
				  goto malformed;
				break;
			      }
			    else if (c <= S2C(u8"9"))
			      c -= S2C(u8"0");
			    else if (c < S2C(u8"a"))
			      {
				if (!nibble)
				  goto malformed;
				break;
			      }
			    else if (c <= S2C(u8"f"))
			      c -= S2C(u8"a") - 10;
			    else
			      {
				if (!nibble)
				  goto malformed;
				break;
			      }
			    ++iter;
			    v = (v << 4) | c;
			  }
			c = v;
		      }
		  }
	      word->push_back (c);
	    }
	}
      else
	// Unquoted character
	word->push_back (c);
    }
  lastBol = iter - buffer.begin ();
  if (result.empty ())
    return ENOENT;

  return 0;
}

void MessageBuffer::LexedLine (std::string &str)
{
  if (lastBol)
    {
      size_t pos = lastBol - 1;
      for (; pos; pos--)
	if (buffer[pos-1] == S2C(u8"\n"))
	  break;

      size_t end = lastBol - 1;
      if (buffer[end-1] == CONTINUE && buffer[end-2] == S2C(u8" "))
	// Strip line continuation
	end -= 2;
      str.append (&buffer[pos], end - pos);
    }
}
} // Detail
} // Cody

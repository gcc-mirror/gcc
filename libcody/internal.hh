// CODYlib		-*- mode:c++ -*-
// Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
// License: Apache v2.0

#include "cody.hh"

#ifndef __has_builtin
#define __has_builtin(X) 0
#endif
#ifndef __has_include
#define __has_include(X) 0
#endif

// C++
#if __has_builtin(__builtin_FILE) && __has_builtin(__builtin_LINE)
#define CODY_LOC_BUILTIN 1
#elif __has_include (<source_location>)
#include <source_location>
#ifdef __cpp_lib_source_location
#define CODY_LOC_SOURCE 1
#endif
#endif

// C
#include <cstdio>

namespace Cody {

// Location is needed regardless of checking, to make the fatal
// handler simpler
class Location
{
protected:
  char const *file;
  unsigned line;

public:
  constexpr Location (char const *file_
#if CODY_LOC_BUILTIN
		      = __builtin_FILE ()
#elif !CODY_LOC_SOURCE
		      = nullptr
#endif
		      , unsigned line_
#if CODY_LOC_BUILTIN
		      = __builtin_LINE ()
#elif !CODY_LOC_SOURCE
		      = 0
#endif
		      )
    :file (file_), line (line_)
  {
  }

#if !CODY_LOC_BUILTIN && CODY_LOC_SOURCE
  using source_location = std::source_location;

  constexpr Location (source_location loc = source_location::current ())
    : Location (loc.file (), loc.line ())
  {
  }
#endif

public:
  constexpr char const *File () const
  {
    return file;
  }
  constexpr unsigned Line () const
  {
    return line;
  }
};

void HCF [[noreturn]]
(
 char const *msg
#if NMS_CHECKING
 , Location const = Location ()
#if !CODY_LOC_BUILTIN && !CODY_LOC_SOURCE
#define HCF(M) HCF ((M), Cody::Location (__FILE__, __LINE__))
#endif
#endif
 ) noexcept;

#if NMS_CHECKING
void AssertFailed [[noreturn]] (Location loc = Location ()) noexcept;
void Unreachable [[noreturn]] (Location loc = Location ()) noexcept;
#if !CODY_LOC_BUILTIN && !CODY_LOC_SOURCE
#define AssertFailed() AssertFailed (Cody::Location (__FILE__, __LINE__))
#define Unreachable() Unreachable (Cody::Location (__FILE__, __LINE__))
#endif

// Do we have __VA_OPT__, alas no specific feature macro for it :(
// From stack overflow
// https://stackoverflow.com/questions/48045470/portably-detect-va-opt-support
// Relies on having variadic macros, but they're a C++11 thing, so
// we're good
#define HAVE_ARG_3(a,b,c,...) c
#define HAVE_VA_OPT_(...) HAVE_ARG_3(__VA_OPT__(,),true,false,)
#define HAVE_VA_OPT HAVE_VA_OPT_(?)

// Oh, for lazily evaluated function parameters
#if HAVE_VA_OPT
// Assert is variadic, so you can write Assert (TPL<A,B>(C)) without
// extraneous parens.  I don't think we need that though.
#define Assert(EXPR, ...)						\
  (__builtin_expect (bool (EXPR __VA_OPT__ (, __VA_ARGS__)), true)	\
   ? (void)0 : AssertFailed ())
#else
// If you don't have the GNU ,##__VA_ARGS__ pasting extension, we'll
// need another fallback
#define Assert(EXPR, ...)						\
  (__builtin_expect (bool (EXPR, ##__VA_ARGS__), true)			\
   ? (void)0 : AssertFailed ())
#endif
#else
// Not asserting, use EXPR in an unevaluated context
#if  HAVE_VA_OPT
#define Assert(EXPR, ...)					\
  ((void)sizeof (bool (EXPR __VA_OPT__ (, __VA_ARGS__))), (void)0)
#else
#define Assert(EXPR, ...)					\
  ((void)sizeof (bool (EXPR, ##__VA_ARGS__)), (void)0)
#endif

inline void Unreachable () noexcept
{
  __builtin_unreachable ();
}
#endif

}

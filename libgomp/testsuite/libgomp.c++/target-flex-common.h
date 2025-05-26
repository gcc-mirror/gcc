#include <cstdio>

#if __cplusplus >= 201103L
  #define BL_NOEXCEPT noexcept
#else
  #define BL_NOEXCEPT throw()
#endif

#if defined __has_builtin
# if __has_builtin (__builtin_LINE)
#  define VERIFY_LINE __builtin_LINE ()
# endif
#endif
#if !defined VERIFY_LINE
# define VERIFY_LINE __LINE__
#endif

/* I'm not a huge fan of macros but in the interest of keeping the code that
   isn't being tested as simple as possible, we use them.  */

#define VERIFY(EXPR) \
  do {										\
    if (!(EXPR))								\
      {										\
	std::printf("VERIFY ln: %d `" #EXPR "` evaluated to false\n",		\
		    VERIFY_LINE);						\
	inner_ok = false;							\
	goto end;								\
      }										\
  } while (false)

#define VERIFY_NON_TARGET(EXPR) \
  do {										\
    if (!(EXPR))								\
      {										\
	std::printf("VERIFY ln: %d `" #EXPR "` evaluated to false\n",		\
		    VERIFY_LINE);						\
	return false;								\
      }										\
  } while (false)

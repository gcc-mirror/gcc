// -*- C++ -*- header wrapper.

// Copyright (C) 1997-1999 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

//
// ISO C++ 14882: 20.4.6  C library
//

#ifndef _CPP_CSTRING
#define _CPP_CSTRING 1
# if defined __GLIBC__ && __GLIBC__ >= 2
// We must not see the optimized string functions GNU libc defines.
#  define __NO_STRING_INLINES
# endif

# include <bits/std_cstddef.h>  /* pick up size_t, NULL */

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <string.h>
    }

    // size_t
    // NULL

    // We do inline captures of most of these in case they
    // have been optimized with macros.  

    inline void* _CPP_memcpy_capture(void* __s1, void const* __s2, size_t __n)
      { return memcpy(__s1,__s2,__n); }
    inline void* _CPP_memmove_capture(void* __s1, void const* __s2, size_t __n)
      { return memmove(__s1,__s2,__n); }
    inline void* _CPP_strcpy_capture(char* __s1, char const* __s2)
      { return strcpy(__s1,__s2); }
    inline char* _CPP_strncpy_capture(char* __s1, char const* __s2, size_t __n)
      { return strncpy(__s1,__s2,__n); }
    inline char* _CPP_strcat_capture(char* __s1, char const* __s2)
      { return strcat(__s1,__s2); }
    inline char* _CPP_strncat_capture(char* __s1, char const* __s2, size_t __n)
      { return strncat(__s1,__s2,__n); }
    inline int _CPP_memcmp_capture(void const* __s1, 
				   void const* __s2, size_t __n)
      { return memcmp(__s1,__s2,__n); }
    inline int _CPP_strcmp_capture(char const* __s1, char const* __s2)
      { return strcmp(__s1,__s2); }
    inline int _CPP_strcoll_capture(char const* __s1, char const* __s2)
      { return strcoll(__s1,__s2); }
    inline int _CPP_strncmp_capture(char const* __s1, 
		                    char const* __s2, size_t __n)
      { return strncmp(__s1,__s2,__n); }
    inline size_t _CPP_strxfrm_capture(char* __b, char const* __s, size_t __n)
      { return strxfrm(__b,__s,__n); }
    inline void* _CPP_memchr_capture(void const* __s1, int __c, size_t __n)
      { return memchr(__s1,__c,__n); }
    inline char* _CPP_strchr_capture(char const* __s1, int __c)
      { return strchr(__s1,__c); }
    inline size_t _CPP_strcspn_capture(char const* __s1, char const* __s2)
      { return strcspn(__s1,__s2); }
    inline char* _CPP_strpbrk_capture(char const* __s1, char const* __s2)
      { return strpbrk(__s1,__s2); }
    inline char* _CPP_strrchr_capture(char const* __s1, int __c)
      { return strrchr(__s1,__c); }
    inline size_t _CPP_strspn_capture(char const* __s1, char const* __s2)
      { return strspn(__s1,__s2); }
    inline char* _CPP_strstr_capture(char const* __s1, char const* __s2)
      { return strstr(__s1,__s2); }
    inline char* _CPP_strtok_capture(char* __s1, char const* __s2)
      { return strtok(__s1,__s2); }
    inline void* _CPP_memset_capture(void* __s, int __c, size_t __n)
      { return memset(__s,__c,__n); }
    // inline char* _CPP_strerror_capture(int __num)
    //  { return strerror(__num); }
    inline size_t _CPP_strlen_capture(char const* __s)
      { return strlen(__s); }

    namespace _C_Shadow { }
  } // close namespace ::_C_Swamp::

// size_t, NULL
# undef memcpy
# undef memmove
# undef strcpy
# undef strncpy
# undef strcat
# undef strncat
# undef memcmp
# undef strcmp
# undef strcoll
# undef strncmp
# undef strxfrm
# undef memchr
# undef strchr
# undef strcspn
# undef strpbrk
# undef strrchr
# undef strspn
# undef strstr
# undef strtok
# undef memset
# undef strerror
# undef strlen

  namespace _C_Swamp {
    namespace _C_Shadow {
    }
  }
  namespace std {

    // Redefine most of these inline.  Note that the 
    // C++ definition differs from C in some cases.

    inline void* memcpy(void* __s1, void const* __s2, size_t __n)
      { return ::_C_Swamp::_CPP_memcpy_capture(__s1,__s2,__n); }
    inline void* memmove(void* __s1, void const* __s2, size_t __n)
      { return ::_C_Swamp::_CPP_memmove_capture(__s1,__s2,__n); }
    inline void* strcpy(char* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strcpy_capture(__s1,__s2); }
    inline char* strncpy(char* __s1, char const* __s2, size_t __n)
      { return ::_C_Swamp::_CPP_strncpy_capture(__s1,__s2,__n); }
    inline char* strcat(char* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strcat_capture(__s1,__s2); }
    inline char* strncat(char* __s1, char const* __s2, size_t __n)
      { return ::_C_Swamp::_CPP_strncat_capture(__s1,__s2,__n); }
    inline int memcmp(void const* __s1, 
		      void const* __s2, size_t __n)
      { return ::_C_Swamp::_CPP_memcmp_capture(__s1,__s2,__n); }
    inline int strcmp(char const* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strcmp_capture(__s1,__s2); }
    inline int strcoll(char const* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strcoll_capture(__s1,__s2); }
    inline int strncmp(char const* __s1, 
		                    char const* __s2, size_t __n)
      { return ::_C_Swamp::_CPP_strncmp_capture(__s1,__s2,__n); }
    inline size_t strxfrm(char* __b, char const* __s, size_t __n)
      { return ::_C_Swamp::_CPP_strxfrm_capture(__b,__s,__n); }

    inline void const* memchr(void const* __s1, int __c, size_t __n)
      { return ::_C_Swamp::_CPP_memchr_capture(__s1,__c,__n); }
    inline       void* memchr(      void* __s1, int __c, size_t __n)
      { return ::_C_Swamp::_CPP_memchr_capture(__s1,__c,__n); }

    inline char const* strchr(char const* __s1, int __c)
      { return ::_C_Swamp::_CPP_strchr_capture(__s1,__c); }
    inline       char* strchr(      char* __s1, int __c)
      { return ::_C_Swamp::_CPP_strchr_capture(__s1,__c); }

    inline size_t strcspn(char const* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strcspn_capture(__s1,__s2); }

    inline char const* strpbrk(char const* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strpbrk_capture(__s1,__s2); }
    inline       char* strpbrk(      char* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strpbrk_capture(__s1,__s2); }

    inline char const* strrchr(char const* __s1, int __c)
      { return ::_C_Swamp::_CPP_strrchr_capture(__s1,__c); }
    inline       char* strrchr(      char* __s1, int __c)
      { return ::_C_Swamp::_CPP_strrchr_capture(__s1,__c); }

    inline size_t strspn(char const* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strspn_capture(__s1,__s2); }

    inline char const* strstr(char const* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strstr_capture(__s1,__s2); }
    inline       char* strstr(      char* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strstr_capture(__s1,__s2); }

    inline char* strtok(char* __s1, char const* __s2)
      { return ::_C_Swamp::_CPP_strtok_capture(__s1,__s2); }
    inline void* memset(void* __s, int __c, size_t __n)
      { return ::_C_Swamp::_CPP_memset_capture(__s,__c,__n); }

    using ::_C_Swamp::strerror;

    inline size_t strlen(char const* __s)
      { return ::_C_Swamp::_CPP_strlen_capture(__s); }

  } // close namespace std::
  
  namespace _C_Swamp {
    namespace _C_Shadow {
      // adopt names back into C
      using ::std::memcpy;
      using ::std::memmove;
      using ::std::strcpy;
      using ::std::strncpy;
      using ::std::strcat;
      using ::std::strncat;
      using ::std::memcmp;
      using ::std::strcmp;
      using ::std::strcoll;
      using ::std::strncmp;
      using ::std::strxfrm;
      using ::std::memchr;
      using ::std::strchr;
      using ::std::strcspn;
      using ::std::strpbrk;
      using ::std::strrchr;
      using ::std::strspn;
      using ::std::strstr;
      using ::std::strtok;
      using ::std::memset;
      // using ::std::strerror;
      using ::std::strlen;
    }
  }

# undef _IN_C_SWAMP_

#endif


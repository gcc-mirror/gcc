## Linker script for GNU ld 2.11.94+ only.
##
## Copyright (C) 2002 Free Software Foundation, Inc.
##
## This file is part of the libstdc++ version 3 distribution.
##
## This file is part of the GNU ISO C++ Library.  This library is free
## software; you can redistribute it and/or modify it under the
## terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this library; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
## USA.

GLIBCPP_3.2 {

  global:

    # Names inside the 'extern' block are demangled names.
    # All but the last are terminated with a semicolon.
    extern "C++"
    {
      std::[A-Za-k]*;
      std::length_error*;
      std::logic_error*;
      std::locale::[A-Za-z]*;
      std::locale::_Impl*;
      std::locale::_S_classic;
      std::locale::_S_global;
      std::locale::_S_num_categories;
      std::locale::facet*;
      std::locale::id*;
      std::locale::locale*;
      std::[A-Zm-z]*;
      std::__throw_*;
      std::__basic_file*;
      std::__num_base*;
      std::__timepunct*;
      std::__numeric_limits_base*;
      std::_S_bit_count;
      std::_S_first_one;
      std::__default_alloc_template*;
      std::__malloc_alloc_template*
    };

    # Names not in an 'extern' block are mangled names.

    _ZNSt6localeC1E*;
    _ZNSt6locale11_M_coalesceERKS_S1_j;
    _ZNSt6locale21_S_normalize_categoryEj;
    _ZNSt6localeD*;

    # std::has_facet*
    _ZSt9has_facet*;

    # operator new(unsigned)
    _Znwj;
    # operator new(unsigned, std::nothrow_t const&)
    _ZnwjRKSt9nothrow_t;
    # operator new(unsigned long)
    _Znwm;
    # operator new(unsigned long, std::nothrow_t const&)
    _ZnwmRKSt9nothrow_t;

    # operator delete(void*)
    _ZdlPv;
    # operator delete(void*, std::nothrow_t const&)
    _ZdlPvRKSt9nothrow_t;

    # operator new[](unsigned)
    _Znaj;
    # operator new[](unsigned, std::nothrow_t const&)
    _ZnajRKSt9nothrow_t;
    # operator new[](unsigned long)
    _Znam;
    # operator new[](unsigned long, std::nothrow_t const&)
    _ZnamRKSt9nothrow_t;

    # operator delete[](void*)
    _ZdaPv;
    # operator delete[](void*, std::nothrow_t const&)
    _ZdaPvRKSt9nothrow_t;

    # vtable
    _ZTV*;
    _ZTT*;

    # typeinfo
    _ZTI*;
    _ZTS*;

    # function-scope static objects requires a guard variable.
    _ZGV*;

    # virtual function thunks
    _ZTh*;
    _ZTv*;
    _ZTc*;

    # std::__convert_to_v
    _ZSt14__convert_to_v*;

  local:
    *;
};

# Symbols added after GLIBCPP_3.2
GLIBCPP_3.2.1 {

  _ZNSt7codecvtIcc11__mbstate_tEC1EP15__locale_structj;
  _ZNSt7codecvtIcc11__mbstate_tEC2EP15__locale_structj;
  _ZNSt7codecvtIwc11__mbstate_tEC1EP15__locale_structj;
  _ZNSt7codecvtIwc11__mbstate_tEC2EP15__locale_structj;

} GLIBCPP_3.2;

# Symbols in the support library (libsupc++) have their own tag.
CXXABI_1.2 {

  global:
    __cxa_*;
    __gxx_personality_v0;
    __gxx_personality_sj0;
    __dynamic_cast;

    # __gnu_cxx::_verbose_terminate_handler()
    _ZN9__gnu_cxx27__verbose_terminate_handlerEv;

  local:
    *;
};

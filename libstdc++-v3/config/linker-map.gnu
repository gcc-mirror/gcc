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
      std::[A-Za]*;
      std::ba[a-r]*;
      std::basic_[a-r]*;
      std::basic_streambuf*;
      std::basic_stringbuf*;
      std::basic_stringstream*;
      std::basic_[t-z]*;
      std::ba[t-z]*;
      std::b[b-z]*;
      std::c[a-n]*;
      std::co[a-c]*;
      std::codecvt_byname*;
      std::codecvt::[A-Za-b]*;
      std::codecvt::[A-Zd-z]*;
      std::codecvt_c;
      std::codecvt_w;
      std::co[e-z]*;
      std::c[p-z]*;
      std::c_[a-z]*;	
      std::[A-Zd-k]*;
      std::length_error*;
      std::logic_error*;
      std::locale::[A-Za-z]*;
      std::locale::_[A-Ra-z]*;
      std::locale::_S_classic;
      std::locale::_S_global;
      std::locale::_S_num_categories;
      std::locale::_S_normalize_category*;
      std::locale::_[T-Za-z]*;
      std::[A-Zm-z]*;
      std::__throw_*;
      std::__basic_file*;
      std::__num_base*;
      std::__timepunct*;
      std::__numeric_limits_base*;
      std::_S_bit_count;
      std::_S_first_one
    };

    # Names not in an 'extern' block are mangled names.

    # std::string minus binary operator plus
    _ZNKSs*;
    _ZNKSb*;
    _ZNSs[A-Za-z]*;
    _ZNSs[0-9][A-Za-z]*;
    _ZNSs[0-9][0-9][A-Za-z]*;
    _ZNSs[0-9]_[A-Ra-z]*;
    _ZNSs[0-9][0-9]_[A-Ra-z]*;
    _ZNSs12_S_empty_repEv;
    _ZNSs20_S_empty_rep_storageE;
    _ZNSbIwSt11char_traitsIwESaIwEE20_S_empty_rep_storageE;
    _ZNSs12_S_constructE*;
    _ZNSs13_S_copy_charsE*;
    _ZNSbIwSt11char_traitsIwESaIwEE[A-Ra-z]*;
    _ZNSbIwSt11char_traitsIwESaIwEE[0-9][A-Ra-z]*;
    _ZNSbIwSt11char_traitsIwESaIwEE[0-9][0-9][A-Ra-z]*;
    _ZNSbIwSt11char_traitsIwESaIwEE[0-9]_[A-Ra-z]*;
    _ZNSbIwSt11char_traitsIwESaIwEE[0-9][0-9]_[A-Ra-z]*;
    _ZNSbIwSt11char_traitsIwESaIwEE13_S_copy_chars*;
    _ZNSbIwSt11char_traitsIwESaIwEE12_S_constructE[jm]wRKS1_;
    _ZNSbIwSt11char_traitsIwESaIwEE12_S_empty_repEv;
    _ZSt24__uninitialized_copy_auxIN9*;
    _ZSt26__uninitialized_fill_n_aux*;
    _ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_;
    _ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_ES3_RKS6_;
    _ZStplIwSt11char_traitsIwESaIwEESbIT_T0_T1_EPKS3_RKS6_;
    _ZStplIwSt11char_traitsIwESaIwEESbIT_T0_T1_ES3_RKS6_;

    # std::locale destructors
    _ZNSt6localeD*;
	 
    # std::codecvt<char> members.
    _ZNKSt7codecvtIcc11__mbstate_tE*;
    # std::codecvt<char>::~codecvt
    _ZNSt7codecvtIcc11__mbstate_tED*;
    # std::codecvt<char>::codecvt(size_t), where size_t variable.
    _ZNSt7codecvtIcc11__mbstate_tEC[12]E[jm];
    # std::codecvt<char>::id
    _ZNSt7codecvtIcc11__mbstate_tE2idE;

    # std::codecvt<wchar_t> members.
    _ZNKSt7codecvtIwc11__mbstate_tE*;
    # std::codecvt<wchar_t>::~codecvt
    _ZNSt7codecvtIwc11__mbstate_tED*;
    # std::codecvt<wchar_t>::codecvt(size_t), where size_t variable.
    _ZNSt7codecvtIwc11__mbstate_tEC[12]E[jm];
    # std::codecvt<wchar_t>::id
    _ZNSt7codecvtIwc11__mbstate_tE2idE;

     # std::use_facet<codecvt>
    _ZSt9use_facetISt7codecvtIcc11__mbstate_tEERKT_RKSt6locale;
    _ZSt9use_facetISt7codecvtIwc11__mbstate_tEERKT_RKSt6locale;

    # std::has_facet*
    _ZSt9has_facet*;

    # std::__default_alloc_template
    _ZNSt24__default_alloc_templateILb1ELi0EE10deallocate*;
    _ZNSt24__default_alloc_templateILb1ELi0EE8allocate*;
    _ZNSt24__default_alloc_templateILb1ELi0EE12_S_free_listE;
    _ZNSt24__default_alloc_templateILb1ELi0EE22_S_node_allocator_lockE;
    _ZNSt24__default_alloc_templateILb1ELi0EE9_S_refillE*;

    # std::__default_alloc_template to be removed in the future
    _ZNSt24__default_alloc_templateILb1ELi0EE10reallocateEPv*;
    _ZNSt24__default_alloc_templateILb1ELi0EE11_S_round_upE*;
    _ZNSt24__default_alloc_templateILb1ELi0EE14_S_chunk_allocE*;
    _ZNSt24__default_alloc_templateILb1ELi0EE17_S_freelist_indexE*;
    _ZNSt24__default_alloc_templateILb1ELi0EE11_S_end_freeE;
    _ZNSt24__default_alloc_templateILb1ELi0EE12_S_heap_sizeE;
    _ZNSt24__default_alloc_templateILb1ELi0EE13_S_start_freeE;
    _ZNSt24__default_alloc_templateILb1ELi0EE5_Lock*;

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

  _ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_ERKS6_S8_;
  _ZStplIwSt11char_traitsIwESaIwEESbIT_T0_T1_ERKS6_S8_;

  _ZNSt24__default_alloc_templateILb1ELi0EE12_S_force_newE;

  # stub functions from libmath
  sinf;
  sinl;
  sinhf;
  sinhl;
  cosf;
  cosl;
  coshf;
  coshl;
  tanf;
  tanl;
  tanhf;
  tanhl;
  atan2f;
  atan2l;
  expf;
  expl;
  hypotf;
  hypotl;
  hypot;
  logf;
  logl;
  log10f;
  log10l;
  powf;
  powl;
  sqrtf;
  sqrtl;
  copysignf;
  nan;
  __signbit;
  __signbitf;
  __signbitl;

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

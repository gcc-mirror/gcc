## Linker script for GNU ld 2.11.94+ only.
##
## Copyright (C) 2002, 2003 Free Software Foundation, Inc.
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

GLIBCPP_3.4 {

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
      std::[A-Zc-k]*;
      std::length_error*;
      std::logic_error*;
      std::locale::[A-Za-e]*;
      std::locale::facet::[A-Za-z]*;
      std::locale::facet::_M*;
      std::locale::facet::_S_c_locale;	
      std::locale::facet::_S_clone_c_locale*;
      std::locale::facet::_S_create_c_locale*;
      std::locale::facet::_S_destroy_c_locale*;
      std::locale::[A-Zg-h]*;
      std::locale::id::[A-Za-z]*;
      std::locale::id::_S_highwater;
      std::locale::[A-Zj-z]*;
      std::locale::_[A-Ha-z]*;
      std::locale::_Impl::[A-Za-z]*;
      std::locale::_Impl::_M_[A-Za-z]*;
      std::locale::_[J-Ra-z]*;
      std::locale::_S_normalize_category*;
      std::locale::_[T-Za-z]*;
      std::[A-Zm-z]*;
      std::__throw_*;
      std::__basic_file*;
      std::__num_base*;
      std::__timepunct*;
      std::__numeric_limits_base*;
      std::_S_bit_count
    };

    # Names not in an 'extern' block are mangled names.

    # std::locale destructors
    _ZNSt6localeD*;
	
    # std::locale::facet destructors
    _ZNSt6locale5facetD*;
	 
    # std::locale::_Impl constructors, destrutors
    _ZNSt6locale5_ImplC*;
    _ZNSt6locale5_ImplD*;

    # bool has_facet 
    _ZSt9has_facet*;

    # std::string, std::wstring minus static data members
    _ZNKS[sb]*;
    _ZNS[sb]D*;
    _ZNS[sb]C*;
    _ZStplI[cw]St11char_traitsI[cw]ESaI[cw]EESbIT_T0_T1_EPKS3_RKS6_;
    _ZStplI[cw]St11char_traitsI[cw]ESaI[cw]EESbIT_T0_T1_ES3_RKS6_;
    _ZNS[sb]I[cw]St11char_traitsI[cw]ESaI[cw]EE[A-Ra-z]*;
    _ZNS[sb]I[cw]St11char_traitsI[cw]ESaI[cw]EE[0-9][A-Ra-z]*;
    _ZNS[sb]I[cw]St11char_traitsI[cw]ESaI[cw]EE[0-9][0-9][A-Ra-z]*;
    _ZNS[sb]I[cw]St11char_traitsI[cw]ESaI[cw]EE[0-9]_[A-Ra-z]*;
    _ZNS[sb]I[cw]St11char_traitsI[cw]ESaI[cw]EE[0-9][0-9]_[A-Ra-z]*;
    _ZNS[sb]I[cw]St11char_traitsI[cw]ESaI[cw]EE20_S_empty_rep_storageE;

    # std::__pool_alloc
    _ZNSt12__pool_allocILb1ELi0EE10deallocateEPvj*;
    _ZNSt12__pool_allocILb1ELi0EE8allocateEj*;
    _ZNSt12__pool_allocILb1ELi0EE5_Lock*;
    _ZNSt12__pool_allocILb1ELi0EE12_S_force_newE;
    _ZNSt12__pool_allocILb1ELi0EE12_S_free_listE;
    _ZNSt12__pool_allocILb1ELi0EE7_S_lockE;
    _ZNSt12__pool_allocILb1ELi0EE9_S_refillE[jm];

    # operator new(size_t)
    _Znw[jm];
    # operator new(size_t, std::nothrow_t const&)
    _Znw[jm]RKSt9nothrow_t;

    # operator delete(void*)
    _ZdlPv;
    # operator delete(void*, std::nothrow_t const&)
    _ZdlPvRKSt9nothrow_t;

    # operator new[](size_t)
    _Zna[jm];
    # operator new[](size_t, std::nothrow_t const&)
    _Zna[jm]RKSt9nothrow_t;

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

  local:
    *;
};


# Symbols in the support library (libsupc++) have their own tag.
CXXABI_1.3 {

  global:
    __cxa_allocate_exception;
    __cxa_bad_cast;
    __cxa_bad_typeid;
    __cxa_begin_catch;
    __cxa_call_unexpected;
    __cxa_current_exception_type;
    __cxa_demangle;
    __cxa_end_catch;
    __cxa_free_exception;
    __cxa_get_globals;
    __cxa_get_globals_fast;
    __cxa_guard_abort;
    __cxa_guard_acquire;
    __cxa_guard_release;
    __cxa_pure_virtual;
    __cxa_rethrow;
    __cxa_throw;
    __cxa_vec_cctor;
    __cxa_vec_cleanup;
    __cxa_vec_ctor;
    __cxa_vec_delete2;
    __cxa_vec_delete3;
    __cxa_vec_delete;
    __cxa_vec_dtor;
    __cxa_vec_new2;
    __cxa_vec_new3;
    __cxa_vec_new;
    __gxx_personality_v0;
    __gxx_personality_sj0;
    __dynamic_cast;

    # __gnu_cxx::_verbose_terminate_handler()
    _ZN9__gnu_cxx27__verbose_terminate_handlerEv;

  local:
    *;
};

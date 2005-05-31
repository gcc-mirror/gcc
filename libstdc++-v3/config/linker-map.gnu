## Linker script for GNU ld 2.13.91+ only.
##
## Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
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

GLIBCXX_3.4 {

  global:

    # Names inside the 'extern' block are demangled names.
    extern "C++"
    {
      std::[A-Za-h]*;
      std::i[a-n]*;
      std::ios_base::[A-Ha-z]*;
      std::ios_base::_M_grow_words*;
      std::ios_base::_M_init*;
      std::ios_base::Init::[A-Za-z]*;
      std::ios_base::[J-Za-z]*;
      std::i[p-z]*;
      std::[A-Zj-k]*;
      std::length_error*;
      std::logic_error*;
      std::locale::[A-Za-e]*;
      std::locale::facet::[A-Za-z]*;
      std::locale::facet::_S_get_c_locale*;
      std::locale::facet::_S_clone_c_locale*;
      std::locale::facet::_S_create_c_locale*;
      std::locale::facet::_S_destroy_c_locale*;
      std::locale::[A-Zg-h]*;
      std::locale::id::[A-Za-z]*;
      std::locale::id::_M_id*;
      std::locale::[A-Zj-z]*;
      std::locale::_[A-Ha-z]*;
      std::locale::_Impl::[A-Za-z]*;
      std::locale::_Impl::_M_[A-Za-z]*;
      std::locale::_[J-Ra-z]*;
      std::locale::_S_normalize_category*;
      std::locale::_[T-Za-z]*;
      std::[A-Zm-z]*;
      std::_List_node_base::hook*;
      std::_List_node_base::swap*;
      std::_List_node_base::unhook*;
      std::_List_node_base::reverse*;
      std::_List_node_base::transfer*;
      std::__throw_*;
      std::__timepunct*;
      std::__numeric_limits_base*;
      std::__num_base::_S_format_float*;
      std::__num_base::_S_format_int*;
      std::__num_base::_S_atoms_in;
      std::__num_base::_S_atoms_out;
      std::__moneypunct_cache*;
      std::__numpunct_cache*;
      std::__timepunct_cache*;
      __gnu_debug::_Safe_iterator_base*;
      __gnu_debug::_Safe_sequence_base*;
      __gnu_debug::_Error_formatter*;
      __gnu_norm::_List_node_base::hook*;
      __gnu_norm::_List_node_base::swap*;
      __gnu_norm::_List_node_base::unhook*;
      __gnu_norm::_List_node_base::reverse*;
      __gnu_norm::_List_node_base::transfer*
    };

    # Names not in an 'extern' block are mangled names.

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

    # std::basic_iostream constructors, destructors
    _ZNSdC*;
    _ZNSdD*;

    # std::locale destructors
    _ZNSt6localeD*;
	
    # std::locale::facet destructors
    _ZNSt6locale5facetD*;
	 
    # std::locale::_Impl constructors, destructors
    _ZNSt6locale5_ImplC*;
    _ZNSt6locale5_ImplD*;

    # std::ios_base, std::ios_base::Init destructors
    _ZNSt8ios_baseD*;
    _ZNSt8ios_base4InitD*;

    # bool has_facet 
    _ZSt9has_facet*;

    # _Rb_tree
    _ZSt18_Rb_tree_decrementPKSt18_Rb_tree_node_base;
    _ZSt18_Rb_tree_decrementPSt18_Rb_tree_node_base;
    _ZSt18_Rb_tree_incrementPKSt18_Rb_tree_node_base;
    _ZSt18_Rb_tree_incrementPSt18_Rb_tree_node_base;
    _ZSt20_Rb_tree_black_countPKSt18_Rb_tree_node_baseS1_;
    _ZSt20_Rb_tree_rotate_leftPSt18_Rb_tree_node_baseRS0_;
    _ZSt21_Rb_tree_rotate_rightPSt18_Rb_tree_node_baseRS0_;
    _ZSt28_Rb_tree_rebalance_for_erasePSt18_Rb_tree_node_baseRS_;
    _ZSt29_Rb_tree_insert_and_rebalancebPSt18_Rb_tree_node_baseS0_RS_;

    # std::__basic_file
    _ZNKSt12__basic_fileIcE7is_openEv;
    _ZNSt12__basic_fileIcE2fdEv;
    _ZNSt12__basic_fileIcE4openEPKcSt13_Ios_Openmodei;
    _ZNSt12__basic_fileIcE4syncEv;
    _ZNSt12__basic_fileIcE5closeEv;
    _ZNSt12__basic_fileIcE6xsgetn*;
    _ZNSt12__basic_fileIcE6xsputn*;
    _ZNSt12__basic_fileIcE7seekoff*;
    _ZNSt12__basic_fileIcE8sys_openE*St13_Ios_Openmode;
    _ZNSt12__basic_fileIcE8xsputn_2*;
    _ZNSt12__basic_fileIcE9showmanycEv;
    _ZNSt12__basic_fileIcEC*;
    _ZNSt12__basic_fileIcED*;

    # virtual table
    _ZTVNSt8ios_base7failureE;
    _ZTVNSt6locale5facetE;
    _ZTVS[a-z];
    _ZTVSt[0-9][A-Za-z]*;
    _ZTVSt[0-9][0-9][A-Za-z]*;
    _ZTVSt11__timepunctI[cw]E;
    _ZTVSt23__codecvt_abstract_baseI[cw]c11__mbstate_tE;
    _ZTVSt21__ctype_abstract_baseI[cw]E;

    # VTT structure
    _ZTTS[a-z];
    _ZTTSt[0-9][A-Za-z]*;
    _ZTTSt[0-9][0-9][A-Za-z]*;

    # typeinfo structure
    _ZTIS[a-z];
    _ZTINSt8ios_base7failureE;
    _ZTINSt6locale5facetE;
    _ZTISt[0-9][A-Za-z]*;
    _ZTISt[0-9][0-9][A-Za-z]*;
    _ZTISt11__timepunctI[cw]E;
    _ZTISt10__num_base;
    _ZTISt21__ctype_abstract_baseI[cw]E;
    _ZTISt23__codecvt_abstract_baseI[cw]c11__mbstate_tE;
    _ZTIN9__gnu_cxx18stdio_sync_filebufI[cw]St11char_traitsI[cw]EEE;
    _ZTIN9__gnu_cxx13stdio_filebufI[cw]St11char_traitsI[cw]EEE;

    # typeinfo name
    _ZTSNSt8ios_base7failureE;
    _ZTSNSt6locale5facetE;
    _ZTSS[a-z];
    _ZTSSt[0-9][A-Za-z]*;
    _ZTSSt[0-9][0-9][A-Za-z]*;
    _ZTSSt11__timepunctI[cw]E;
    _ZTSSt10__num_base;
    _ZTSSt21__ctype_abstract_baseI[cw]E;
    _ZTSSt23__codecvt_abstract_baseI[cw]c11__mbstate_tE;
    _ZTSN9__gnu_cxx18stdio_sync_filebufI[cw]St11char_traitsI[cw]EEE;
    _ZTSN9__gnu_cxx13stdio_filebufI[cw]St11char_traitsI[cw]EEE;

    # function-scope static objects requires a guard variable.
    _ZGVNSt*;

    # virtual function thunks
    _ZThn8_NS*;
    _ZThn16_NS*;
    _ZTv0_n12_NS*;
    _ZTv0_n24_NS*;

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
    __signbit;
    __signbitf;
    __signbitl;

    # __gnu_cxx::stdio_sync_filebuf
    _ZTVN9__gnu_cxx18stdio_sync_filebufI[cw]St11char_traitsI[cw]EEE;

    # __gnu_cxx::__atomic_add
    # __gnu_cxx::__exchange_and_add
    _ZN9__gnu_cxx12__atomic_add*;
    _ZN9__gnu_cxx18__exchange_and_add*;

  # DO NOT DELETE THIS LINE.  Port-specific symbols, if any, will be here.

  local:
    *;
};

GLIBCXX_3.4.1 {
 
    _ZNSt12__basic_fileIcE4fileEv;
 
} GLIBCXX_3.4;
 
GLIBCXX_3.4.2 {

    _ZN9__gnu_cxx18stdio_sync_filebufI[cw]St11char_traitsI[cw]EE4fileEv;

    _ZN9__gnu_cxx17__pool_alloc_base9_M_refillE[jm];
    _ZN9__gnu_cxx17__pool_alloc_base16_M_get_free_listE[jm];
    _ZN9__gnu_cxx17__pool_alloc_base12_M_get_mutexEv;

} GLIBCXX_3.4.1;

GLIBCXX_3.4.3 {
 
    # stub functions from libmath
    acosf;
    acosl;
    asinf;
    asinl;
    atanf;
    atanl;
    ceilf;
    ceill;
    floorf;
    floorl;
    fmodf;
    fmodl;
    frexpf;
    frexpl;
    ldexpf;
    ldexpl;
    modff;
    modfl;

} GLIBCXX_3.4.2;

GLIBCXX_3.4.4 {

    _ZN9__gnu_cxx6__poolILb0EE13_M_initializeEv;
    _ZN9__gnu_cxx6__poolILb1EE13_M_initializeEPFvPvE;
    _ZN9__gnu_cxx6__poolILb1EE21_M_destroy_thread_keyEPv;
    _ZN9__gnu_cxx6__poolILb1EE16_M_get_thread_idEv;
    _ZN9__gnu_cxx6__poolILb[01]EE16_M_reserve_blockE[jm][jm];
    _ZN9__gnu_cxx6__poolILb[01]EE16_M_reclaim_blockEPc[jm];
    _ZN9__gnu_cxx6__poolILb[01]EE10_M_destroyEv;

    _ZN9__gnu_cxx9free_list6_M_getE*;
    _ZN9__gnu_cxx9free_list8_M_clearEv;

} GLIBCXX_3.4.3;

GLIBCXX_3.4.5 {

    _ZSt17__copy_streambufsI[cw]St11char_traitsI[cw]EEiPSt15basic_streambuf*;
    _ZNSt8ios_base17_M_call_callbacksENS_5eventE;
    _ZNSt8ios_base20_M_dispose_callbacksEv;
    _ZNSt6locale5facet13_S_get_c_nameEv;
    _ZN11__gnu_debug13__fancy_abortEPKciS1_S1_;

} GLIBCXX_3.4.4;

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

    # *_type_info classes, ctor and dtor
    _ZN10__cxxabiv117__array_type_info*;
    _ZN10__cxxabiv117__class_type_info*;
    _ZN10__cxxabiv116__enum_type_info*;
    _ZN10__cxxabiv120__function_type_info*;
    _ZN10__cxxabiv123__fundamental_type_info*;
    _ZN10__cxxabiv117__pbase_type_info*;
    _ZN10__cxxabiv129__pointer_to_member_type_info*;
    _ZN10__cxxabiv119__pointer_type_info*;
    _ZN10__cxxabiv120__si_class_type_info*;
    _ZN10__cxxabiv121__vmi_class_type_info*;

    # *_type_info classes, member functions
    _ZNK10__cxxabiv117__class_type_info*;
    _ZNK10__cxxabiv120__function_type_info*;
    _ZNK10__cxxabiv117__pbase_type_info*;
    _ZNK10__cxxabiv129__pointer_to_member_type_info*;
    _ZNK10__cxxabiv119__pointer_type_info*;
    _ZNK10__cxxabiv120__si_class_type_info*;
    _ZNK10__cxxabiv121__vmi_class_type_info*;

    # virtual table
    _ZTVN10__cxxabiv117__array_type_infoE;
    _ZTVN10__cxxabiv117__class_type_infoE;
    _ZTVN10__cxxabiv116__enum_type_infoE;
    _ZTVN10__cxxabiv120__function_type_infoE;
    _ZTVN10__cxxabiv123__fundamental_type_infoE;
    _ZTVN10__cxxabiv117__pbase_type_infoE;
    _ZTVN10__cxxabiv129__pointer_to_member_type_infoE;
    _ZTVN10__cxxabiv119__pointer_type_infoE;
    _ZTVN10__cxxabiv120__si_class_type_infoE;
    _ZTVN10__cxxabiv121__vmi_class_type_infoE;

    # typeinfo structure (and some names)
    _ZTI[a-z];
    _ZTIP[a-z];
    _ZTIPK[a-z];
    _ZTIN10__cxxabiv117__array_type_infoE;
    _ZTIN10__cxxabiv117__class_type_infoE;
    _ZTIN10__cxxabiv116__enum_type_infoE;
    _ZTIN10__cxxabiv120__function_type_infoE;
    _ZTIN10__cxxabiv123__fundamental_type_infoE;
    _ZTIN10__cxxabiv117__pbase_type_infoE;
    _ZTIN10__cxxabiv129__pointer_to_member_type_infoE;
    _ZTIN10__cxxabiv119__pointer_type_infoE;
    _ZTIN10__cxxabiv120__si_class_type_infoE;
    _ZTIN10__cxxabiv121__vmi_class_type_infoE;

    # typeinfo name
    _ZTS[a-z];
    _ZTSP[a-z];
    _ZTSPK[a-z];
    _ZTSN10__cxxabiv117__array_type_infoE;
    _ZTSN10__cxxabiv117__class_type_infoE;
    _ZTSN10__cxxabiv116__enum_type_infoE;
    _ZTSN10__cxxabiv120__function_type_infoE;
    _ZTSN10__cxxabiv123__fundamental_type_infoE;
    _ZTSN10__cxxabiv117__pbase_type_infoE;
    _ZTSN10__cxxabiv129__pointer_to_member_type_infoE;
    _ZTSN10__cxxabiv119__pointer_type_infoE;
    _ZTSN10__cxxabiv120__si_class_type_infoE;
    _ZTSN10__cxxabiv121__vmi_class_type_infoE;

    # __gnu_cxx::_verbose_terminate_handler()
    _ZN9__gnu_cxx27__verbose_terminate_handlerEv;

  local:
    *;
};

CXXABI_1.3.1 {

    __cxa_get_exception_ptr;

} CXXABI_1.3;

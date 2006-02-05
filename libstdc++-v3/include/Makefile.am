## Makefile for the include subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006
## Free Software Foundation, Inc.
##
## This file is part of the libstdc++ version 3 distribution.
## Process this file with automake to produce Makefile.in.

## This file is part of the GNU ISO C++ Library.  This library is free
## software; you can redistribute it and/or modify it under the
## terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option)
## any later version.

## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License along
## with this library; see the file COPYING.  If not, write to the Free
## Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
## USA.

include $(top_srcdir)/fragment.am

# Standard C++ includes.
std_srcdir = ${glibcxx_srcdir}/include/std
std_builddir = .
std_headers = \
	${std_srcdir}/std_algorithm.h \
	${std_srcdir}/std_bitset.h \
	${std_srcdir}/std_complex.h \
	${std_srcdir}/std_deque.h \
	${std_srcdir}/std_fstream.h \
	${std_srcdir}/std_functional.h \
	${std_srcdir}/std_iomanip.h \
	${std_srcdir}/std_ios.h \
	${std_srcdir}/std_iosfwd.h \
	${std_srcdir}/std_iostream.h \
	${std_srcdir}/std_istream.h \
	${std_srcdir}/std_iterator.h \
	${std_srcdir}/std_limits.h \
	${std_srcdir}/std_list.h \
	${std_srcdir}/std_locale.h \
	${std_srcdir}/std_map.h \
	${std_srcdir}/std_memory.h \
	${std_srcdir}/std_numeric.h \
	${std_srcdir}/std_ostream.h \
	${std_srcdir}/std_queue.h \
	${std_srcdir}/std_set.h \
	${std_srcdir}/std_sstream.h \
	${std_srcdir}/std_stack.h \
	${std_srcdir}/std_stdexcept.h \
	${std_srcdir}/std_streambuf.h \
	${std_srcdir}/std_string.h \
	${std_srcdir}/std_utility.h \
	${std_srcdir}/std_valarray.h \
	${std_srcdir}/std_vector.h
# Renamed at build time.
std_headers_rename = \
	algorithm \
	bitset \
	complex \
	deque \
	fstream \
	functional \
	iomanip \
	ios \
	iosfwd \
	iostream \
	istream \
	iterator \
	limits \
	list \
	locale \
	map \
	memory \
	numeric \
	ostream \
	queue \
	set \
	sstream \
	stack \
	stdexcept \
	streambuf \
	string \
	utility \
	valarray \
	vector

bits_srcdir = ${glibcxx_srcdir}/include/bits
bits_builddir = ./bits
bits_headers = \
	${bits_srcdir}/allocator.h \
	${bits_srcdir}/atomicity.h \
	${bits_srcdir}/basic_ios.h \
	${bits_srcdir}/basic_ios.tcc \
	${bits_srcdir}/basic_string.h \
	${bits_srcdir}/basic_string.tcc \
	${bits_srcdir}/boost_concept_check.h \
	${bits_srcdir}/char_traits.h \
	${bits_srcdir}/codecvt.h \
	${bits_srcdir}/concept_check.h \
	${bits_srcdir}/concurrence.h \
	${bits_srcdir}/cpp_type_traits.h \
	${bits_srcdir}/deque.tcc \
	${bits_srcdir}/fstream.tcc \
	${bits_srcdir}/functexcept.h \
	${bits_srcdir}/gslice.h \
	${bits_srcdir}/gslice_array.h \
	${bits_srcdir}/indirect_array.h \
	${bits_srcdir}/ios_base.h \
	${bits_srcdir}/istream.tcc \
	${bits_srcdir}/list.tcc \
	${bits_srcdir}/locale_classes.h \
	${bits_srcdir}/locale_facets.h \
	${bits_srcdir}/locale_facets.tcc \
	${bits_srcdir}/localefwd.h \
	${bits_srcdir}/mask_array.h \
	${bits_srcdir}/ostream.tcc \
	${bits_srcdir}/postypes.h \
	${bits_srcdir}/stream_iterator.h \
	${bits_srcdir}/streambuf_iterator.h \
	${bits_srcdir}/slice_array.h \
	${bits_srcdir}/sstream.tcc \
	${bits_srcdir}/stl_algo.h \
	${bits_srcdir}/stl_algobase.h \
	${bits_srcdir}/stl_bvector.h \
	${bits_srcdir}/stl_construct.h \
	${bits_srcdir}/stl_deque.h \
	${bits_srcdir}/stl_function.h \
	${bits_srcdir}/stl_heap.h \
	${bits_srcdir}/stl_iterator.h \
	${bits_srcdir}/stl_iterator_base_funcs.h \
	${bits_srcdir}/stl_iterator_base_types.h \
	${bits_srcdir}/stl_list.h \
	${bits_srcdir}/stl_map.h \
	${bits_srcdir}/stl_multimap.h \
	${bits_srcdir}/stl_multiset.h \
	${bits_srcdir}/stl_numeric.h \
	${bits_srcdir}/stl_pair.h \
	${bits_srcdir}/stl_queue.h \
	${bits_srcdir}/stl_raw_storage_iter.h \
	${bits_srcdir}/stl_relops.h \
	${bits_srcdir}/stl_set.h \
	${bits_srcdir}/stl_stack.h \
	${bits_srcdir}/stl_tempbuf.h \
	${bits_srcdir}/stl_tree.h \
	${bits_srcdir}/stl_uninitialized.h \
	${bits_srcdir}/stl_vector.h \
	${bits_srcdir}/streambuf.tcc \
	${bits_srcdir}/stringfwd.h \
	${bits_srcdir}/valarray_array.h \
	${bits_srcdir}/valarray_array.tcc \
	${bits_srcdir}/valarray_before.h \
	${bits_srcdir}/valarray_after.h \
	${bits_srcdir}/vector.tcc

backward_srcdir = ${glibcxx_srcdir}/include/backward
backward_builddir = ./backward
backward_headers = \
	${backward_srcdir}/complex.h \
	${backward_srcdir}/iomanip.h \
	${backward_srcdir}/istream.h \
	${backward_srcdir}/ostream.h \
	${backward_srcdir}/stream.h \
	${backward_srcdir}/streambuf.h \
	${backward_srcdir}/algo.h \
	${backward_srcdir}/algobase.h \
	${backward_srcdir}/alloc.h \
	${backward_srcdir}/bvector.h \
	${backward_srcdir}/defalloc.h \
	${backward_srcdir}/deque.h \
	${backward_srcdir}/function.h \
	${backward_srcdir}/hash_map.h \
	${backward_srcdir}/hash_set.h \
	${backward_srcdir}/hashtable.h \
	${backward_srcdir}/heap.h \
	${backward_srcdir}/iostream.h \
	${backward_srcdir}/iterator.h \
	${backward_srcdir}/list.h \
	${backward_srcdir}/map.h \
	${backward_srcdir}/multimap.h \
	${backward_srcdir}/new.h \
	${backward_srcdir}/multiset.h \
	${backward_srcdir}/pair.h \
	${backward_srcdir}/queue.h \
	${backward_srcdir}/rope.h \
	${backward_srcdir}/set.h \
	${backward_srcdir}/slist.h \
	${backward_srcdir}/stack.h \
	${backward_srcdir}/tempbuf.h \
	${backward_srcdir}/tree.h \
	${backward_srcdir}/vector.h \
	${backward_srcdir}/fstream.h \
	${backward_srcdir}/strstream \
	${backward_srcdir}/backward_warning.h


assoc_srcdir = ${glibcxx_srcdir}/include/ext/pb_assoc
assoc_builddir = ./ext/pb_assoc

assoc_subdirs = \
	${assoc_builddir}/detail \
	${assoc_builddir}/detail/tree_assoc_cntnr \
	${assoc_builddir}/detail/basic_assoc_cntnr \
	${assoc_builddir}/detail/splay_tree_ \
	${assoc_builddir}/detail/typelist \
	${assoc_builddir}/detail/basic_hash_assoc_cntnr \
	${assoc_builddir}/detail/cc_ht_map_ \
	${assoc_builddir}/detail/gp_ht_map_ \
	${assoc_builddir}/detail/tree_policy \
	${assoc_builddir}/detail/resize_policy \
	${assoc_builddir}/detail/bin_search_tree_ \
	${assoc_builddir}/detail/basic_tree_assoc_cntnr \
	${assoc_builddir}/detail/cc_hash_assoc_cntnr \
	${assoc_builddir}/detail/value_type_adapter \
	${assoc_builddir}/detail/lu_policy \
	${assoc_builddir}/detail/gp_hash_assoc_cntnr \
	${assoc_builddir}/detail/unordered_iterator \
	${assoc_builddir}/detail/ov_tree_map_ \
	${assoc_builddir}/detail/lu_assoc_cntnr \
	${assoc_builddir}/detail/hash_fn \
	${assoc_builddir}/detail/eq_fn \
	${assoc_builddir}/detail/rb_tree_map_ \
	${assoc_builddir}/detail/lu_map_

# The ability for make and the underlying host to deal with this
# unweildy list as one entire entity is not a sure thing, and may
# cause build errors. Thus, split one list into five smaller ones.
assoc_headers1 = \
	${assoc_srcdir}/detail/tree_assoc_cntnr/constructor_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/type_utils.hpp \
	${assoc_srcdir}/detail/order_statistics_imp.hpp \
	${assoc_srcdir}/detail/basic_assoc_cntnr/d_find_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_assoc_cntnr/d_insert_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_assoc_cntnr/extract_key.hpp \
	${assoc_srcdir}/detail/basic_assoc_cntnr/erase_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_assoc_cntnr/insert_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_assoc_cntnr/constructor_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_assoc_cntnr/info_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_assoc_cntnr/iterators_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_assoc_cntnr/d_extract_key.hpp \
	${assoc_srcdir}/detail/basic_assoc_cntnr/constructors_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/splay_tree_/erase_fn_imps.hpp \
	${assoc_srcdir}/detail/splay_tree_/splay_tree_.hpp \
	${assoc_srcdir}/detail/splay_tree_/node.hpp \
	${assoc_srcdir}/detail/splay_tree_/insert_fn_imps.hpp \
	${assoc_srcdir}/detail/splay_tree_/find_fn_imps.hpp \
	${assoc_srcdir}/detail/splay_tree_/splay_fn_imps.hpp \
	${assoc_srcdir}/detail/splay_tree_/split_join_fn_imps.hpp \
	${assoc_srcdir}/detail/splay_tree_/info_fn_imps.hpp \
	${assoc_srcdir}/detail/splay_tree_/debug_fn_imps.hpp \
	${assoc_srcdir}/detail/splay_tree_/constructors_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/typelist/typelist_transform.hpp \
	${assoc_srcdir}/detail/typelist/typelist_apply.hpp \
	${assoc_srcdir}/detail/typelist/typelist_filter.hpp \
	${assoc_srcdir}/detail/typelist/typelist_at_index.hpp \
	${assoc_srcdir}/detail/typelist/typelist_contains.hpp \
	${assoc_srcdir}/detail/typelist/typelist_append.hpp \
	${assoc_srcdir}/detail/typelist/typelist_typelist_append.hpp \
	${assoc_srcdir}/detail/ds_trait_imp.hpp \
	${assoc_srcdir}/detail/basic_hash_assoc_cntnr/resize_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_hash_assoc_cntnr/constructor_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_hash_assoc_cntnr/constructors_destructor_fn_imps.hpp 

assoc_headers2 = \
	${assoc_srcdir}/detail/cc_ht_map_/resize_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/find_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/erase_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/resize_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/debug_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/erase_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/policy_access_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/cmp_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/insert_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/insert_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/find_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/find_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/cond_key_dtor_entry_dealtor.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/constructor_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/entry_list_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/constructor_destructor_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/erase_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/cc_ht_map_.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/constructor_destructor_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/standard_policies.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/info_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/iterators_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/insert_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/debug_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/size_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/debug_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_ht_map_/resize_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/resize_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/find_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/erase_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/resize_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/debug_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/erase_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/policy_access_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/insert_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/insert_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/find_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/find_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/constructor_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/constructor_destructor_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/erase_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/constructor_destructor_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/info_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/insert_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/debug_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/gp_ht_map_.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/iterator_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/debug_no_store_hash_fn_imps.hpp \
	${assoc_srcdir}/detail/gp_ht_map_/resize_no_store_hash_fn_imps.hpp 

assoc_headers3 = \
	${assoc_srcdir}/detail/tree_policy/order_statistics_imp.hpp \
	${assoc_srcdir}/detail/tree_policy/null_node_updator_imp.hpp \
	${assoc_srcdir}/detail/types_traits.hpp \
	${assoc_srcdir}/detail/mapping_level_imp.hpp \
	${assoc_srcdir}/detail/map_debug_base.hpp \
	${assoc_srcdir}/detail/resize_policy/ht_prime_size_policy_imp.hpp \
	${assoc_srcdir}/detail/resize_policy/hash_load_check_resize_trigger_imp.hpp \
	${assoc_srcdir}/detail/resize_policy/hash_prime_size_policy_imp.hpp \
	${assoc_srcdir}/detail/resize_policy/size_base.hpp \
	${assoc_srcdir}/detail/resize_policy/cc_hash_max_collision_resize_trigger_imp.hpp \
	${assoc_srcdir}/detail/resize_policy/hash_exponential_size_policy_imp.hpp \
	${assoc_srcdir}/detail/resize_policy/hash_standard_resize_policy_imp.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/cond_dtor_entry_dealtor.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/find_iterators.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/erase_fn_imps.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/insert_fn_imps.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/find_fn_imps.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/cond_key_dtor_entry_dealtor.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/node_iterators.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/bin_search_tree_.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/split_join_fn_imps.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/rotate_fn_imps.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/info_fn_imps.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/iterators_fn_imps.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/r_erase_fn_imps.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/debug_fn_imps.hpp \
	${assoc_srcdir}/detail/bin_search_tree_/constructors_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_tree_assoc_cntnr/r_range_iteration_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_tree_assoc_cntnr/erase_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_tree_assoc_cntnr/policy_access_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_tree_assoc_cntnr/constructor_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_tree_assoc_cntnr/split_join_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_tree_assoc_cntnr/r_erase_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_tree_assoc_cntnr/range_iteration_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_tree_assoc_cntnr/constructors_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/basic_tree_assoc_cntnr/node_iteration_fn_imps.hpp \
	${assoc_srcdir}/detail/cc_hash_assoc_cntnr/constructor_destructor_fn_imps.hpp 

assoc_headers4 = \
	${assoc_srcdir}/detail/assoc_cntnr_base.hpp \
	${assoc_srcdir}/detail/ms_category_imp.hpp \
	${assoc_srcdir}/detail/hash_types_traits.hpp \
	${assoc_srcdir}/detail/typelist.hpp \
	${assoc_srcdir}/detail/value_type_adapter/invalidation_guarantee_selector.hpp \
	${assoc_srcdir}/detail/value_type_adapter/erase_fn_imps.hpp \
	${assoc_srcdir}/detail/value_type_adapter/iterator.hpp \
	${assoc_srcdir}/detail/value_type_adapter/constructor_destructor_and_related.hpp \
	${assoc_srcdir}/detail/value_type_adapter/value_type_traits.hpp \
	${assoc_srcdir}/detail/value_type_adapter/insert_fn_imps.hpp \
	${assoc_srcdir}/detail/value_type_adapter/find_fn_imps.hpp \
	${assoc_srcdir}/detail/value_type_adapter/ref_pair.hpp \
	${assoc_srcdir}/detail/value_type_adapter/it_value_type_traits.hpp \
	${assoc_srcdir}/detail/value_type_adapter/info_fn_imps.hpp \
	${assoc_srcdir}/detail/value_type_adapter/erase_if_pred.hpp \
	${assoc_srcdir}/detail/value_type_adapter/value_type_adapter.hpp \
	${assoc_srcdir}/detail/value_type_adapter/iterator_fn_imps.hpp \
	${assoc_srcdir}/detail/lu_policy/mtf_lu_policy_imp.hpp \
	${assoc_srcdir}/detail/lu_policy/counter_lu_policy_imp.hpp \
	${assoc_srcdir}/detail/lu_policy/counter_lu_metadata_imp.hpp \
	${assoc_srcdir}/detail/gp_hash_assoc_cntnr/constructor_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/unordered_iterator/find_iterator.hpp \
	${assoc_srcdir}/detail/unordered_iterator/iterator.hpp \
	${assoc_srcdir}/detail/unordered_iterator/const_iterator.hpp \
	${assoc_srcdir}/detail/unordered_iterator/const_find_iterator.hpp \
	${assoc_srcdir}/detail/standard_policies.hpp \
	${assoc_srcdir}/detail/standard_sizes.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/erase_fn_imps.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/insert_fn_imps.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/find_fn_imps.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/ov_tree_map_.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/node_iterators.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/split_join_fn_imps.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/cond_dtor.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/info_fn_imps.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/iterators_fn_imps.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/debug_fn_imps.hpp \
	${assoc_srcdir}/detail/ov_tree_map_/constructors_destructor_fn_imps.hpp

assoc_headers5 = \
	${assoc_srcdir}/detail/lu_assoc_cntnr/policy_access_fn_imps.hpp \
	${assoc_srcdir}/detail/lu_assoc_cntnr/constructor_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/hash_fn/linear_probe_fn_imp.hpp \
	${assoc_srcdir}/detail/hash_fn/mod_based_range_hashing.hpp \
	${assoc_srcdir}/detail/hash_fn/direct_mod_range_hashing_imp.hpp \
	${assoc_srcdir}/detail/hash_fn/probe_fn_base.hpp \
	${assoc_srcdir}/detail/hash_fn/ranged_probe_fn.hpp \
	${assoc_srcdir}/detail/hash_fn/quadratic_probe_fn_imp.hpp \
	${assoc_srcdir}/detail/hash_fn/direct_mask_range_hashing_imp.hpp \
	${assoc_srcdir}/detail/hash_fn/ranged_hash_fn.hpp \
	${assoc_srcdir}/detail/hash_fn/mask_based_range_hashing.hpp \
	${assoc_srcdir}/detail/eq_fn/eq_by_less.hpp \
	${assoc_srcdir}/detail/eq_fn/hash_eq_fn.hpp \
	${assoc_srcdir}/detail/rb_tree_map_/erase_fn_imps.hpp \
	${assoc_srcdir}/detail/rb_tree_map_/node.hpp \
	${assoc_srcdir}/detail/rb_tree_map_/insert_fn_imps.hpp \
	${assoc_srcdir}/detail/rb_tree_map_/find_fn_imps.hpp \
	${assoc_srcdir}/detail/rb_tree_map_/split_join_fn_imps.hpp \
	${assoc_srcdir}/detail/rb_tree_map_/rb_tree_.hpp \
	${assoc_srcdir}/detail/rb_tree_map_/info_fn_imps.hpp \
	${assoc_srcdir}/detail/rb_tree_map_/debug_fn_imps.hpp \
	${assoc_srcdir}/detail/rb_tree_map_/constructors_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/lu_map_/lu_map_.hpp \
	${assoc_srcdir}/detail/lu_map_/erase_fn_imps.hpp \
	${assoc_srcdir}/detail/lu_map_/policy_access_fn_imps.hpp \
	${assoc_srcdir}/detail/lu_map_/insert_fn_imps.hpp \
	${assoc_srcdir}/detail/lu_map_/find_fn_imps.hpp \
	${assoc_srcdir}/detail/lu_map_/constructor_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/lu_map_/info_fn_imps.hpp \
	${assoc_srcdir}/detail/lu_map_/iterators_fn_imps.hpp \
	${assoc_srcdir}/detail/lu_map_/debug_fn_imps.hpp \
	${assoc_srcdir}/detail/constructors_destructor_fn_imps.hpp \
	${assoc_srcdir}/detail/ms_trait_imp.hpp \
	${assoc_srcdir}/detail/cond_dealtor.hpp \
	${assoc_srcdir}/hash_policy.hpp \
	${assoc_srcdir}/trivial_iterator_def.hpp \
	${assoc_srcdir}/ds_trait.hpp \
	${assoc_srcdir}/exception.hpp \
	${assoc_srcdir}/tree_policy.hpp \
	${assoc_srcdir}/lu_policy.hpp \
	${assoc_srcdir}/ms_trait.hpp \
	${assoc_srcdir}/assoc_cntnr.hpp \
	${assoc_srcdir}/data_type.hpp 


ext_srcdir = ${glibcxx_srcdir}/include/ext
ext_builddir = ./ext 
ext_headers = \
	${ext_srcdir}/algorithm \
	${ext_srcdir}/array_allocator.h \
	${ext_srcdir}/bitmap_allocator.h \
	${ext_srcdir}/codecvt_specializations.h \
	${ext_srcdir}/debug_allocator.h \
	${ext_srcdir}/stdio_filebuf.h \
	${ext_srcdir}/stdio_sync_filebuf.h \
	${ext_srcdir}/functional \
	${ext_srcdir}/hash_map \
	${ext_srcdir}/hash_set \
	${ext_srcdir}/hash_fun.h \
	${ext_srcdir}/hashtable.h \
	${ext_srcdir}/iterator \
	${ext_srcdir}/malloc_allocator.h \
	${ext_srcdir}/memory \
	${ext_srcdir}/mt_allocator.h \
	${ext_srcdir}/new_allocator.h \
	${ext_srcdir}/numeric \
	${ext_srcdir}/pod_char_traits.h \
	${ext_srcdir}/pool_allocator.h \
	${ext_srcdir}/rb_tree \
	${ext_srcdir}/rope \
	${ext_srcdir}/ropeimpl.h \
	${ext_srcdir}/slist \
	${ext_srcdir}/typelist.h \
	${ext_srcdir}/rc_string_base.h \
	${ext_srcdir}/sso_string_base.h \
	${ext_srcdir}/vstring.h \
	${ext_srcdir}/vstring.tcc \
	${ext_srcdir}/vstring_fwd.h \
	${ext_srcdir}/vstring_util.h	


tr1_srcdir = ${glibcxx_srcdir}/include/tr1
tr1_builddir = ./tr1
tr1_headers = \
	${tr1_srcdir}/array \
	${tr1_srcdir}/bind_repeat.h \
	${tr1_srcdir}/bind_iterate.h \
	${tr1_srcdir}/boost_shared_ptr.h \
	${tr1_srcdir}/cctype \
	${tr1_srcdir}/cfenv \
	${tr1_srcdir}/cfloat \
	${tr1_srcdir}/cinttypes \
	${tr1_srcdir}/climits \
	${tr1_srcdir}/common.h \
	${tr1_srcdir}/complex \
	${tr1_srcdir}/cstdarg \
	${tr1_srcdir}/cstdint \
	${tr1_srcdir}/cstdio \
	${tr1_srcdir}/ctype.h \
	${tr1_srcdir}/cwchar \
	${tr1_srcdir}/cwctype \
	${tr1_srcdir}/fenv.h \
	${tr1_srcdir}/float.h \
	${tr1_srcdir}/functional \
	${tr1_srcdir}/functional_iterate.h \
	${tr1_srcdir}/hashtable \
	${tr1_srcdir}/inttypes.h \
	${tr1_srcdir}/limits.h \
	${tr1_srcdir}/memory \
	${tr1_srcdir}/mu_iterate.h \
	${tr1_srcdir}/ref_fwd.h \
	${tr1_srcdir}/ref_wrap_iterate.h \
	${tr1_srcdir}/repeat.h \
	${tr1_srcdir}/stdarg.h \
	${tr1_srcdir}/stdint.h \
	${tr1_srcdir}/stdio.h \
	${tr1_srcdir}/tuple \
	${tr1_srcdir}/tuple_iterate.h \
	${tr1_srcdir}/type_traits \
	${tr1_srcdir}/type_traits_fwd.h \
	${tr1_srcdir}/unordered_set \
	${tr1_srcdir}/unordered_map \
	${tr1_srcdir}/utility \
	${tr1_srcdir}/wchar.h \
	${tr1_srcdir}/wctype.h


# This is the common subset of files that all three "C" header models use.
c_base_srcdir = $(C_INCLUDE_DIR)
c_base_builddir = .
c_base_headers = \
	${c_base_srcdir}/std_cassert.h \
	${c_base_srcdir}/std_cctype.h \
	${c_base_srcdir}/std_cerrno.h \
	${c_base_srcdir}/std_cfloat.h \
	${c_base_srcdir}/std_ciso646.h \
	${c_base_srcdir}/std_climits.h \
	${c_base_srcdir}/std_clocale.h \
	${c_base_srcdir}/std_cmath.h \
	${c_base_srcdir}/std_csetjmp.h \
	${c_base_srcdir}/std_csignal.h \
	${c_base_srcdir}/std_cstdarg.h \
	${c_base_srcdir}/std_cstddef.h \
	${c_base_srcdir}/std_cstdio.h \
	${c_base_srcdir}/std_cstdlib.h \
	${c_base_srcdir}/std_cstring.h \
	${c_base_srcdir}/std_ctime.h \
	${c_base_srcdir}/std_cwchar.h \
	${c_base_srcdir}/std_cwctype.h
c_base_headers_rename = \
	cassert \
	cctype \
	cerrno \
	cfloat \
	ciso646 \
	climits \
	clocale \
	cmath \
	csetjmp \
	csignal \
	cstdarg \
	cstddef \
	cstdio \
	cstdlib \
	cstring \
	ctime \
	cwchar \
	cwctype

# "C" compatibility headers.
c_compatibility_srcdir = ${glibcxx_srcdir}/include/c_compatibility
c_compatibility_builddir = .
c_compatibility_headers = \
	${c_compatibility_srcdir}/assert.h \
	${c_compatibility_srcdir}/ctype.h \
	${c_compatibility_srcdir}/errno.h \
	${c_compatibility_srcdir}/float.h \
	${c_compatibility_srcdir}/iso646.h \
	${c_compatibility_srcdir}/limits.h \
	${c_compatibility_srcdir}/locale.h \
	${c_compatibility_srcdir}/math.h \
	${c_compatibility_srcdir}/setjmp.h \
	${c_compatibility_srcdir}/signal.h \
	${c_compatibility_srcdir}/stdarg.h \
	${c_compatibility_srcdir}/stddef.h \
	${c_compatibility_srcdir}/stdio.h \
	${c_compatibility_srcdir}/stdlib.h \
	${c_compatibility_srcdir}/string.h \
	${c_compatibility_srcdir}/time.h \
	${c_compatibility_srcdir}/wchar.h \
	${c_compatibility_srcdir}/wctype.h

# Debug mode headers
debug_srcdir = ${glibcxx_srcdir}/include/debug
debug_builddir = ./debug
debug_headers = \
	${debug_srcdir}/bitset \
	${debug_srcdir}/debug.h \
	${debug_srcdir}/deque \
	${debug_srcdir}/formatter.h \
	${debug_srcdir}/functions.h \
	${debug_srcdir}/hash_map \
	${debug_srcdir}/hash_map.h \
	${debug_srcdir}/hash_multimap.h \
	${debug_srcdir}/hash_multiset.h \
	${debug_srcdir}/hash_set \
	${debug_srcdir}/hash_set.h \
	${debug_srcdir}/list \
	${debug_srcdir}/map \
	${debug_srcdir}/macros.h \
	${debug_srcdir}/map.h \
	${debug_srcdir}/multimap.h \
	${debug_srcdir}/multiset.h \
	${debug_srcdir}/safe_base.h \
	${debug_srcdir}/safe_iterator.h \
	${debug_srcdir}/safe_iterator.tcc \
	${debug_srcdir}/safe_sequence.h \
	${debug_srcdir}/set \
	${debug_srcdir}/set.h \
	${debug_srcdir}/string \
	${debug_srcdir}/vector

# Some of the different "C" header models need extra files.
# Some "C" header schemes require the "C" compatibility headers.
# For --enable-cheaders=c_std
if GLIBCXX_C_HEADERS_C_STD
c_base_headers_extra = ${c_base_srcdir}/cmath.tcc
else
c_base_headers_extra =
endif

if GLIBCXX_C_HEADERS_COMPATIBILITY
c_compatibility_headers_extra = ${c_compatibility_headers}
else
c_compatibility_headers_extra =
endif

host_srcdir = ${glibcxx_srcdir}/$(OS_INC_SRCDIR)
host_builddir = ./${host_alias}/bits
host_installdir = ${gxx_include_dir}/${host_alias}$(MULTISUBDIR)/bits
host_headers = \
	${host_srcdir}/ctype_base.h \
	${host_srcdir}/ctype_inline.h \
	${host_srcdir}/ctype_noninline.h \
	${host_srcdir}/os_defines.h \
	${glibcxx_srcdir}/$(ATOMIC_WORD_SRCDIR)/atomic_word.h \
	${glibcxx_srcdir}/$(ABI_TWEAKS_SRCDIR)/cxxabi_tweaks.h \
	${glibcxx_srcdir}/$(CPU_DEFINES_SRCDIR)/cpu_defines.h	

# Non-installed host_header files.
COMPATIBILITY_H = config/abi/compatibility.h
host_headers_noinst = \
	${glibcxx_srcdir}/$(CLOCALE_INTERNAL_H) \
	${glibcxx_srcdir}/$(COMPATIBILITY_H)

# These host_headers_extra files are all built with ad hoc naming rules.
host_headers_extra = \
	${host_builddir}/basic_file.h \
	${host_builddir}/c++config.h \
	${host_builddir}/c++allocator.h \
	${host_builddir}/c++io.h \
	${host_builddir}/c++locale.h \
	${host_builddir}/messages_members.h \
	${host_builddir}/time_members.h 

thread_host_headers = \
	${host_builddir}/gthr.h \
	${host_builddir}/gthr-single.h \
	${host_builddir}/gthr-posix.h \
	${host_builddir}/gthr-tpf.h \
	${host_builddir}/gthr-default.h

pch_input = ${host_builddir}/stdc++.h
pch_output_builddir = ${host_builddir}/stdc++.h.gch
pch_output_installdir = ${host_installdir}/stdc++.h.gch
pch_source = ${glibcxx_srcdir}/include/stdc++.h
PCHFLAGS=-Winvalid-pch -Wno-deprecated -x c++-header $(CXXFLAGS)
if GLIBCXX_BUILD_PCH
pch_build = ${pch_input}
pch_install = install-pch
else
pch_build =
pch_install =
endif

# List of all timestamp files.  By keeping only one copy of this list, both
# CLEANFILES and all-local are kept up-to-date.
allstamped = \
	stamp-std stamp-bits stamp-c_base stamp-c_compatibility \
	stamp-backward stamp-ext stamp-assoc stamp-tr1 stamp-debug stamp-host

# List of all files that are created by explicit building, editing, or
# catenation.
allcreated = \
	${host_builddir}/c++config.h \
	${thread_host_headers} \
	${pch_build}

# Here are the rules for building the headers
all-local: ${allstamped} ${allcreated}

# This rule is slightly different, in that we must change the name of the
# local file from std_foo.h to foo.
stamp-std: ${std_headers}
	@if [ ! -d "${std_builddir}" ]; then \
	  mkdir -p ${std_builddir} ;\
	fi ;\
	if [ ! -f stamp-std ]; then \
	  (cd ${std_builddir} && for h in $?; do \
	    build_name=`echo $$h | sed -e 's,.*/std_,,' -e 's,\.h$$,,'` ;\
	    $(LN_S) $$h ./$${build_name} || true ;\
	  done) ;\
	fi ;\
	$(STAMP) stamp-std

stamp-bits: ${bits_headers}
	@if [ ! -d "${bits_builddir}" ]; then \
	  mkdir -p ${bits_builddir} ;\
	fi ;\
	if [ ! -f stamp-bits ]; then \
	  (cd ${bits_builddir} && $(LN_S) $? . || true) ;\
	fi ;\
	$(STAMP) stamp-bits

stamp-c_base: stamp-bits ${c_base_headers} ${c_base_headers_extra}
	@if [ ! -d "${c_base_builddir}" ]; then \
	  mkdir -p ${c_base_builddir} ;\
	fi ;\
	if [ ! -f stamp-c_base ]; then \
	  (cd ${c_base_builddir} && for h in ${c_base_headers}; do \
	    build_name=`echo $$h | sed -e 's,.*/std_,,' -e 's,\.h$$,,'` ;\
	    $(LN_S) $$h ./$${build_name} || true ;\
	  done) ;\
	  if [ ! -z "${c_base_headers_extra}" ]; then \
	  (cd ${bits_builddir} && $(LN_S) ${c_base_headers_extra}  . || true) ;\
	  fi ;\
	fi ;\
	$(STAMP) stamp-c_base

stamp-c_compatibility: ${c_compatibility_headers_extra}
	@if [ ! -d "${c_compatibility_builddir}" ]; then \
	  mkdir -p ${c_compatibility_builddir} ;\
	fi ;\
	if [ ! -f stamp-c_compatibility ]; then \
	  if [ ! -z "${c_compatibility_headers_extra}" ]; then \
	    (cd ${c_compatibility_builddir} && $(LN_S) $? . || true) ;\
	  fi ;\
	fi ;\
	$(STAMP) stamp-c_compatibility

stamp-backward: ${backward_headers}
	@if [ ! -d "${backward_builddir}" ]; then \
	  mkdir -p ${backward_builddir} ;\
	fi ;\
	if [ ! -f stamp-backward ]; then \
	  (cd ${backward_builddir} && $(LN_S) $? . || true) ;\
	fi ;\
	$(STAMP) stamp-backward

stamp-ext: ${ext_headers}
	@if [ ! -d "${ext_builddir}" ]; then \
	    mkdir -p ${ext_builddir} ;\
	fi ;\
	if [ ! -f stamp-ext ]; then \
	  (cd ${ext_builddir} && $(LN_S) $? . || true) ;\
	fi ;\
	$(STAMP) stamp-ext

# Have to deal with nested include directories, gah! Strip off source
# directory before making the link.
# XXX check  ${assoc_headers}
stamp-assoc:
	@if [ ! -d "${assoc_builddir}" ]; then \
	    mkdir -p ${assoc_subdirs} ;\
	fi 
	@if [ ! -f stamp-assoc ]; then \
	    cd ${assoc_builddir} && for h in ${assoc_headers1}; do \
		build_name=`echo $$h | sed -e "s|${assoc_srcdir}|.|g"` ;\
		$(LN_S) $$h $${build_name} || true ;\
	      done ;\
	fi
	@if [ ! -f stamp-assoc ]; then \
	    cd ${assoc_builddir} && for h in ${assoc_headers2}; do \
		build_name=`echo $$h | sed -e "s|${assoc_srcdir}|.|g"` ;\
		$(LN_S) $$h $${build_name} || true ;\
	      done ;\
	fi
	@if [ ! -f stamp-assoc ]; then \
	    cd ${assoc_builddir} && for h in ${assoc_headers3}; do \
		build_name=`echo $$h | sed -e "s|${assoc_srcdir}|.|g"` ;\
		$(LN_S) $$h $${build_name} || true ;\
	      done ;\
	fi
	@if [ ! -f stamp-assoc ]; then \
	    cd ${assoc_builddir} && for h in ${assoc_headers4}; do \
		build_name=`echo $$h | sed -e "s|${assoc_srcdir}|.|g"` ;\
		$(LN_S) $$h $${build_name} || true ;\
	      done ;\
	fi
	@if [ ! -f stamp-assoc ]; then \
	    cd ${assoc_builddir} && for h in ${assoc_headers5}; do \
		build_name=`echo $$h | sed -e "s|${assoc_srcdir}|.|g"` ;\
		$(LN_S) $$h $${build_name} || true ;\
	      done ;\
	fi
	$(STAMP) stamp-assoc

stamp-tr1: ${tr1_headers}
	@if [ ! -d "${tr1_builddir}" ]; then \
	    mkdir -p ${tr1_builddir} ;\
	fi ;\
	if [ ! -f stamp-tr1 ]; then \
	  (cd ${tr1_builddir} && $(LN_S) $? . || true) ;\
	fi ;\
	$(STAMP) stamp-tr1

stamp-debug: ${debug_headers}
	@if [ ! -d "${debug_builddir}" ]; then \
	  mkdir -p ${debug_builddir} ;\
	fi ;\
	if [ ! -f stamp-debug ]; then \
	  (cd ${debug_builddir} && @LN_S@ $? . || true) ;\
	fi ;\
	$(STAMP) stamp-debug

stamp-${host_alias}:
	@if [ ! -d ${host_builddir} ]; then \
	  mkdir -p ${host_builddir} ;\
	fi ;\
	$(STAMP) stamp-${host_alias}

# Host includes static.
# XXX Missing dependency info for {host_headers_extra}
stamp-host: ${host_headers} ${host_headers_noinst} stamp-${host_alias}
	@if [ ! -f stamp-host ]; then \
	  (cd ${host_builddir} ;\
	  $(LN_S) ${host_headers} . || true ;\
	  $(LN_S) ${glibcxx_srcdir}/$(BASIC_FILE_H) basic_file.h || true ;\
	  $(LN_S) ${glibcxx_srcdir}/$(ALLOCATOR_H) c++allocator.h || true ;\
	  $(LN_S) ${glibcxx_srcdir}/$(CSTDIO_H) c++io.h || true ;\
	  $(LN_S) ${glibcxx_srcdir}/$(CLOCALE_H) c++locale.h || true ;\
	  $(LN_S) ${glibcxx_srcdir}/$(CLOCALE_INTERNAL_H) . || true ;\
	  $(LN_S) ${glibcxx_srcdir}/$(COMPATIBILITY_H) . || true ;\
	  $(LN_S) ${glibcxx_srcdir}/$(CMESSAGES_H) messages_members.h || true ;\
	  $(LN_S) ${glibcxx_srcdir}/$(CTIME_H) time_members.h || true ;\
	  $(LN_S) ${glibcxx_srcdir}/$(CCODECVT_H) codecvt_specializations.h || true);\
	fi ;\
	$(STAMP) stamp-host

# Host includes dynamic.
if ENABLE_SYMVERS_GNU_NAMESPACE
stamp-namespace-version:
	echo 1 > stamp-namespace-version
else
stamp-namespace-version:
	echo 0 > stamp-namespace-version
endif
${host_builddir}/c++config.h: ${CONFIG_HEADER} \
			      ${glibcxx_srcdir}/include/bits/c++config \
			      stamp-${host_alias} \
			      ${toplevel_srcdir}/gcc/DATESTAMP \
			      stamp-namespace-version
	@date=`cat ${toplevel_srcdir}/gcc/DATESTAMP` ;\
	nsa_version=`cat stamp-namespace-version` ;\
	sed -e "s,define __GLIBCXX__,define __GLIBCXX__ $$date," \
	-e "s,define _GLIBCXX_NAMESPACE_ASSOCIATION_VERSION, define _GLIBCXX_NAMESPACE_ASSOCIATION_VERSION $$nsa_version," \
            < ${glibcxx_srcdir}/include/bits/c++config > $@ ;\
	sed -e 's/HAVE_/_GLIBCXX_HAVE_/g' \
	    -e 's/PACKAGE/_GLIBCXX_PACKAGE/g' \
	    -e 's/VERSION/_GLIBCXX_VERSION/g' \
	    -e 's/WORDS_/_GLIBCXX_WORDS_/g' \
	    < ${CONFIG_HEADER} >> $@ ;\
	echo "" >> $@ ;\
	echo "#endif // _CXXCONFIG_" >> $@

# Host includes for threads
uppercase = [ABCDEFGHIJKLMNOPQRSTUVWXYZ_]

${host_builddir}/gthr.h: ${toplevel_srcdir}/gcc/gthr.h stamp-${host_alias}
	sed -e '/^#pragma/b' \
	    -e '/^#/s/\(${uppercase}${uppercase}*\)/_GLIBCXX_\1/g' \
	    -e 's/_GLIBCXX_SUPPORTS_WEAK/__GXX_WEAK__/g' \
	    -e 's,^#include "\(.*\)",#include <bits/\1>,g' \
	    < ${toplevel_srcdir}/gcc/gthr.h > $@

${host_builddir}/gthr-single.h: ${toplevel_srcdir}/gcc/gthr-single.h \
				  stamp-${host_alias}
	sed -e 's/\(UNUSED\)/_GLIBCXX_\1/g' \
	    -e 's/\(GCC${uppercase}*_H\)/_GLIBCXX_\1/g' \
	    < ${toplevel_srcdir}/gcc/gthr-single.h > $@

${host_builddir}/gthr-posix.h: ${toplevel_srcdir}/gcc/gthr-posix.h \
				   stamp-${host_alias}
	sed -e 's/\(UNUSED\)/_GLIBCXX_\1/g' \
	    -e 's/\(GCC${uppercase}*_H\)/_GLIBCXX_\1/g' \
	    -e 's/SUPPORTS_WEAK/__GXX_WEAK__/g' \
	    -e 's/\(${uppercase}*USE_WEAK\)/_GLIBCXX_\1/g' \
	    < ${toplevel_srcdir}/gcc/gthr-posix.h > $@

${host_builddir}/gthr-tpf.h: ${toplevel_srcdir}/gcc/gthr-tpf.h \
				   stamp-${host_alias}
	sed -e 's/\(UNUSED\)/_GLIBCXX_\1/g' \
	    -e 's/\(GCC${uppercase}*_H\)/_GLIBCXX_\1/g' \
	    -e 's/SUPPORTS_WEAK/__GXX_WEAK__/g' \
	    -e 's/\(${uppercase}*USE_WEAK\)/_GLIBCXX_\1/g' \
	    < ${toplevel_srcdir}/gcc/gthr-tpf.h > $@

${host_builddir}/gthr-default.h: ${toplevel_srcdir}/gcc/${glibcxx_thread_h} \
				   stamp-${host_alias}
	sed -e 's/\(UNUSED\)/_GLIBCXX_\1/g' \
	    -e 's/\(GCC${uppercase}*_H\)/_GLIBCXX_\1/g' \
	    -e 's/SUPPORTS_WEAK/__GXX_WEAK__/g' \
	    -e 's/\(${uppercase}*USE_WEAK\)/_GLIBCXX_\1/g' \
	    -e 's,^#include "\(.*\)",#include <bits/\1>,g' \
	    < ${toplevel_srcdir}/gcc/${glibcxx_thread_h} > $@

# Build a precompiled C++ include, stdc++.h.gch.
${pch_input}: ${allstamped} ${host_builddir}/c++config.h ${pch_source}
	touch ${pch_input}; \
	if [ ! -d "${pch_output_builddir}" ]; then \
	  mkdir -p ${pch_output_builddir}; \
	fi; \
	$(CXX) $(PCHFLAGS) $(AM_CPPFLAGS) ${pch_source} -O0 -g -o ${pch_output_builddir}/O0g.gch; \
	$(CXX) $(PCHFLAGS) $(AM_CPPFLAGS) ${pch_source} -O2 -g -o ${pch_output_builddir}/O2g.gch;

# For robustness sake (in light of junk files or in-source
# configuration), copy from the build or source tree to the install
# tree using only the human-maintained file lists and directory
# components.  Yes, with minor differences, this is sheer duplication
# of the staging rules above using $(INSTALL_DATA) instead of LN_S and
# `$(mkinstalldirs)' instead of `mkdir -p'.  In particular,
# host_headers_extra are taken out of the build tree staging area;
# the rest are taken from the original source tree.

if GLIBCXX_HOSTED
install-data-local: install-headers ${pch_install}
else
install-data-local: install-freestanding-headers
endif

# This is a subset of the full install-headers rule.  We only need <cstddef>,
# <limits>, <cstdlib>, <cstdarg>, <new>, <typeinfo>, <exception>, and any
# files which they include (and which we provide).  The last three headers
# are installed by libsupc++, so only the first four and the sub-includes
# are copied here.
install-freestanding-headers:
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}
	$(mkinstalldirs) $(DESTDIR)${host_installdir}
	for file in ${host_srcdir}/os_defines.h ${host_builddir}/c++config.h; do \
	  $(INSTALL_DATA) $${file} $(DESTDIR)${host_installdir}; done
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/${std_builddir}
	$(INSTALL_DATA) ${std_builddir}/limits $(DESTDIR)${gxx_include_dir}/${std_builddir}
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/${c_base_builddir}
	for file in cstddef cstdlib cstdarg; do \
	  $(INSTALL_DATA) ${c_base_builddir}/$${file} $(DESTDIR)${gxx_include_dir}/${c_base_builddir}; done

# The real deal.
install-headers:
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/${bits_builddir}
	for file in ${bits_headers}; do \
	  $(INSTALL_DATA) $${file} $(DESTDIR)${gxx_include_dir}/${bits_builddir}; done
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/${backward_builddir}
	for file in ${backward_headers}; do \
	  $(INSTALL_DATA) $${file} $(DESTDIR)${gxx_include_dir}/${backward_builddir}; done
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/${ext_builddir}
	for file in ${ext_headers}; do \
	  $(INSTALL_DATA) $${file} $(DESTDIR)${gxx_include_dir}/${ext_builddir}; done
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/${assoc_builddir}
	for dir in ${assoc_subdirs}; do \
	  $(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/$${dir} ; done
	for file in ${assoc_headers1}; do \
	  install_base=$(DESTDIR)${gxx_include_dir}/${assoc_builddir} ; \
	  relative_name=`echo $$file | sed -e "s|${assoc_srcdir}|.|g"` ;\
	  $(INSTALL_DATA) $${file} $${install_base}/$${relative_name} ; done
	for file in ${assoc_headers2}; do \
	  install_base=$(DESTDIR)${gxx_include_dir}/${assoc_builddir} ; \
	  relative_name=`echo $$file | sed -e "s|${assoc_srcdir}|.|g"` ;\
	  $(INSTALL_DATA) $${file} $${install_base}/$${relative_name} ; done
	for file in ${assoc_headers3}; do \
	  install_base=$(DESTDIR)${gxx_include_dir}/${assoc_builddir} ; \
	  relative_name=`echo $$file | sed -e "s|${assoc_srcdir}|.|g"` ;\
	  $(INSTALL_DATA) $${file} $${install_base}/$${relative_name} ; done
	for file in ${assoc_headers4}; do \
	  install_base=$(DESTDIR)${gxx_include_dir}/${assoc_builddir} ; \
	  relative_name=`echo $$file | sed -e "s|${assoc_srcdir}|.|g"` ;\
	  $(INSTALL_DATA) $${file} $${install_base}/$${relative_name} ; done
	for file in ${assoc_headers5}; do \
	  install_base=$(DESTDIR)${gxx_include_dir}/${assoc_builddir} ; \
	  relative_name=`echo $$file | sed -e "s|${assoc_srcdir}|.|g"` ;\
	  $(INSTALL_DATA) $${file} $${install_base}/$${relative_name} ; done
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/${tr1_builddir}
	for file in ${tr1_headers}; do \
	  $(INSTALL_DATA) $${file} $(DESTDIR)${gxx_include_dir}/${tr1_builddir}; done
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/${c_base_builddir}
	for file in ${c_base_headers_rename}; do \
	  $(INSTALL_DATA) ${c_base_builddir}/$${file} $(DESTDIR)${gxx_include_dir}/${c_base_builddir}; done
	c_base_headers_extra_install='$(c_base_headers_extra)';\
	for file in $$c_base_headers_extra_install; do \
	  $(INSTALL_DATA) $$file $(DESTDIR)${gxx_include_dir}/${bits_builddir}; done
	c_compatibility_headers_install='$(c_compatibility_headers_extra)';\
	for file in $$c_compatibility_headers_install; do \
	  $(INSTALL_DATA) $$file $(DESTDIR)${gxx_include_dir}; done
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/${std_builddir}
	for file in ${std_headers_rename}; do \
	  $(INSTALL_DATA) ${std_builddir}/$${file} $(DESTDIR)${gxx_include_dir}/${std_builddir}; done
	$(mkinstalldirs) $(DESTDIR)${gxx_include_dir}/${debug_builddir}
	for file in ${debug_headers}; do \
	  $(INSTALL_DATA) $${file} $(DESTDIR)${gxx_include_dir}/${debug_builddir}; done
	$(mkinstalldirs) $(DESTDIR)${host_installdir}
	for file in ${host_headers} ${host_headers_extra} \
	 ${thread_host_headers}; do \
	  $(INSTALL_DATA) $${file} $(DESTDIR)${host_installdir}; done

install-pch:
	$(mkinstalldirs) $(DESTDIR)${pch_output_installdir}
	for file in ${pch_output_builddir}/*; do \
	  $(INSTALL_DATA) $$file $(DESTDIR)${pch_output_installdir}; done

# By adding these files here, automake will remove them for 'make clean'
CLEANFILES = ${pch_input} ${pch_output_builddir}/*

# Stop implicit '.o' make rules from ever stomping on extensionless
# headers, in the improbable case where some foolish, crack-addled
# developer tries to create them via make in the include build
# directory. (This is more of an example of how this kind of rule can
# be made.)
.PRECIOUS: $(std_headers_rename) $(c_base_headers_rename)
$(std_headers_rename): ; @:
$(c_base_headers_rename): ; @:

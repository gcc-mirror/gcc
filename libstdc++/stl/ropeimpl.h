/*
 * Copyright (c) 1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/* NOTE: This is an internal header file, included by other STL headers.
 *   You should not attempt to use it directly.
 */

# include <stdio.h>
# include <iostream.h>

__STL_BEGIN_NAMESPACE

#if defined(__sgi) && !defined(__GNUC__) && (_MIPS_SIM != _MIPS_SIM_ABI32)
#pragma set woff 1174
#endif

// Set buf_start, buf_end, and buf_ptr appropriately, filling tmp_buf
// if necessary.  Assumes path_end[leaf_index] and leaf_pos are correct.
// Results in a valid buf_ptr if the iterator can be legitimately
// dereferenced.
template <class charT, class Alloc>
void __rope_iterator_base<charT,Alloc>::setbuf
(__rope_iterator_base<charT,Alloc> &x)
{
    const RopeBase * leaf = x.path_end[x.leaf_index];
    size_t leaf_pos = x.leaf_pos;
    size_t pos = x.current_pos;

    switch(leaf -> tag) {
	case RopeBase::leaf:
	    x.buf_start = ((__rope_RopeLeaf<charT,Alloc> *)leaf) -> data;
	    x.buf_ptr = x.buf_start + (pos - leaf_pos);
	    x.buf_end = x.buf_start + leaf -> size;
	    break;
	case RopeBase::function:
	case RopeBase::substringfn:
	    {
		size_t len = iterator_buf_len;
		size_t buf_start_pos = leaf_pos;
		size_t leaf_end = leaf_pos + leaf -> size;
		char_producer<charT> *fn =
			((__rope_RopeFunction<charT,Alloc> *)leaf) -> fn;

		if (buf_start_pos + len <= pos) {
		    buf_start_pos = pos - len/4;
		    if (buf_start_pos + len > leaf_end) {
			buf_start_pos = leaf_end - len;
		    }
		}
		if (buf_start_pos + len > leaf_end) {
		    len = leaf_end - buf_start_pos;
		}
		(*fn)(buf_start_pos - leaf_pos, len, x.tmp_buf);
		x.buf_ptr = x.tmp_buf + (pos - buf_start_pos);
		x.buf_start = x.tmp_buf;
		x.buf_end = x.tmp_buf + len;
	    }
	    break;
	default:
	    __stl_assert(0);
    }
}

// Set path and buffer inside a rope iterator.  We assume that 
// pos and root are already set.
template <class charT, class Alloc>
void __rope_iterator_base<charT,Alloc>::setcache
(__rope_iterator_base<charT,Alloc> &x)
{
    const RopeBase * path[RopeBase::max_rope_depth+1];
    const RopeBase * curr_rope;
    int curr_depth = -1;  /* index into path    */
    size_t curr_start_pos = 0;
    size_t pos = x.current_pos;
    unsigned char dirns = 0;	// Bit vector indicating right turns in the path

    __stl_assert(pos <= x.root -> size);
    if (pos >= x.root -> size) {
	x.buf_ptr = 0;
	return;
    }
    curr_rope = x.root;
    if (0 != curr_rope -> c_string) {
	/* Treat the root as a leaf. */
	x.buf_start = curr_rope -> c_string;
	x.buf_end = curr_rope -> c_string + curr_rope -> size;
	x.buf_ptr = curr_rope -> c_string + pos;
	x.path_end[0] = curr_rope;
	x.leaf_index = 0;
	x.leaf_pos = 0;
	return;
    }
    for(;;) {
	++curr_depth;
	__stl_assert(curr_depth <= RopeBase::max_rope_depth);
	path[curr_depth] = curr_rope;
	switch(curr_rope -> tag) {
	  case RopeBase::leaf:
	  case RopeBase::function:
	  case RopeBase::substringfn:
	    x.leaf_pos = curr_start_pos;
	    goto done;
	  case RopeBase::concat:
	    {
		__rope_RopeConcatenation<charT,Alloc> *c =
			(__rope_RopeConcatenation<charT,Alloc> *)curr_rope;
		RopeBase * left = c -> left;
		size_t left_len = left -> size;
		
		dirns <<= 1;
		if (pos >= curr_start_pos + left_len) {
		    dirns |= 1;
		    curr_rope = c -> right;
		    curr_start_pos += left_len;
		} else {
		    curr_rope = left;
		}
	    }
	    break;
	}
    }
  done:
    // Copy last section of path into path_end.
      {
	int i = -1;
	int j = curr_depth  + 1 - path_cache_len;

	if (j < 0) j = 0;
	while (j <= curr_depth) {
	    x.path_end[++i] = path[j++];
	}
	x.leaf_index = i;
      }
      x.path_directions = dirns;
    setbuf(x);
}

// Specialized version of the above.  Assumes that
// the path cache is valid for the previous position.
template <class charT, class Alloc>
void __rope_iterator_base<charT,Alloc>::setcache_for_incr
(__rope_iterator_base<charT,Alloc> &x)
{
    int current_index = x.leaf_index;
    const RopeBase * current_node = x.path_end[current_index];
    size_t len = current_node -> size;
    size_t node_start_pos = x.leaf_pos;
    unsigned char dirns = x.path_directions;
    __rope_RopeConcatenation<charT,Alloc> * c;

    __stl_assert(x.current_pos <= x.root -> size);
    if (x.current_pos - node_start_pos < len) {
	/* More stuff in this leaf, we just didn't cache it. */
	setbuf(x);
	return;
    }
    __stl_assert(node_start_pos + len == x.current_pos);
    //  node_start_pos is starting position of last_node.
    while (--current_index >= 0) {
	if (!(dirns & 1) /* Path turned left */) break;
	current_node = x.path_end[current_index];
	c = (__rope_RopeConcatenation<charT,Alloc> *)current_node;
	// Otherwise we were in the right child.  Thus we should pop
	// the concatenation node.
	node_start_pos -= c -> left -> size;
	dirns >>= 1;
    }
    if (current_index < 0) {
	// We underflowed the cache. Punt.
	setcache(x);
	return;
    }
    current_node = x.path_end[current_index];
    c = (__rope_RopeConcatenation<charT,Alloc> *)current_node;
    // current_node is a concatenation node.  We are positioned on the first
    // character in its right child.
    // node_start_pos is starting position of current_node.
    node_start_pos += c -> left -> size;
    current_node = c -> right;
    x.path_end[++current_index] = current_node;
    dirns |= 1;
    while (RopeBase::concat == current_node -> tag) {
	++current_index;
	if (path_cache_len == current_index) {
	    int i;
	    for (i = 0; i < path_cache_len-1; i++) {
		x.path_end[i] = x.path_end[i+1];
	    }
	    --current_index;
	}
	current_node =
	    ((__rope_RopeConcatenation<charT,Alloc> *)current_node) -> left;
	x.path_end[current_index] = current_node;
	dirns <<= 1;
	// node_start_pos is unchanged.
    }
    x.leaf_index = current_index;
    x.leaf_pos = node_start_pos;
    x.path_directions = dirns;
    setbuf(x);
}

template <class charT, class Alloc>
void __rope_iterator_base<charT,Alloc>::incr(size_t n) {
    current_pos += n;
    if (0 != buf_ptr) {
        size_t chars_left = buf_end - buf_ptr;
        if (chars_left > n) {
            buf_ptr += n;
        } else if (chars_left == n) {
            buf_ptr += n;
            setcache_for_incr(*this);
        } else {
            buf_ptr = 0;
        }
    }
}

template <class charT, class Alloc>
void __rope_iterator_base<charT,Alloc>::decr(size_t n) {
    if (0 != buf_ptr) {
        size_t chars_left = buf_ptr - buf_start;
        if (chars_left >= n) {
            buf_ptr -= n;
        } else {
            buf_ptr = 0;
        }
    }
    current_pos -= n;
}

template <class charT, class Alloc>
void __rope_iterator<charT,Alloc>::check() {
    if (root_rope -> tree_ptr != root) {
        // Rope was modified.  Get things fixed up.
        RopeBase::unref(root);
        root = root_rope -> tree_ptr;
        RopeBase::ref(root);
        buf_ptr = 0;
    }
}

template <class charT, class Alloc>
inline __rope_const_iterator<charT, Alloc>::__rope_const_iterator
(const __rope_iterator<charT,Alloc> & x)
: __rope_iterator_base<charT,Alloc>(x) { }

template <class charT, class Alloc>
inline __rope_iterator<charT,Alloc>::__rope_iterator
(rope<charT,Alloc>& r, size_t pos)
        : __rope_iterator_base<charT,Alloc>(r.tree_ptr, pos), root_rope(&r) {
    RopeBase::ref(root);
}

template <class charT, class Alloc>
inline size_t rope<charT,Alloc>::char_ptr_len(const charT *s)
{
    const charT *p = s;

    while (!is0(*p)) { ++p; }
    return(p - s);
}

template <class charT, class Alloc>
rope<charT,Alloc>::RopeLeaf *
rope<charT,Alloc>::RopeLeaf_from_char_ptr(__GC_CONST charT *s, size_t size)
{
    RopeLeaf *t = LAlloc::allocate();

    t -> tag = RopeBase::leaf;
    if (__is_basic_char_type((charT *)0)) {
	// already eos terminated.
	t -> c_string = s;
    } else {
	t -> c_string = 0;
    }
    t -> is_balanced = true;
    t -> depth = 0;
    t -> size = size;
    t -> data = s;
#   ifndef __GC
	t -> refcount = 1;
	t -> init_refcount_lock();
#   endif
    return (t);
}

# ifdef __GC
template <class charT, class Alloc>
void __rope_RopeBase<charT,Alloc>::fn_finalization_proc(void * tree, void *)
{
    delete ((__rope_RopeFunction<charT,Alloc> *)tree) -> fn;
}
# endif

template <class charT, class Alloc>
rope<charT,Alloc>::RopeFunction *
rope<charT,Alloc>::RopeFunction_from_fn
(char_producer<charT> *fn, size_t size, bool delete_fn)
{
    if (0 == size) return 0;
    RopeFunction *t = FAlloc::allocate();
    t -> tag = RopeBase::function;
    t -> c_string = 0;
    t -> is_balanced = true;
    t -> depth = 0;
    t -> size = size;
    t -> fn = fn;
#   ifdef __GC
	if (delete_fn) {
	    GC_REGISTER_FINALIZER(t, RopeBase::fn_finalization_proc, 0, 0, 0);
	}
#   else
	t -> delete_when_done = delete_fn;
	t -> refcount = 1;
	t -> init_refcount_lock();
#   endif
    return (t);
}

#ifndef __GC

template <class charT, class Alloc>
inline void __rope_RopeBase<charT,Alloc>::free_c_string()
{
    charT * cstr = c_string;
    if (0 != cstr) {
	size_t sz = size + 1;
	destroy(cstr, cstr + sz);
	DataAlloc::deallocate(cstr, sz);
    }
}

template <class charT, class Alloc>
inline void __rope_RopeBase<charT,Alloc>::free_string(charT* s, size_t n)
{
    if (!__is_basic_char_type((charT *)0)) {
	destroy(s, s + n);
    }
    DataAlloc::deallocate(s, rounded_up_size(n));
}

template <class charT, class Alloc>
void __rope_RopeBase<charT,Alloc>::free_tree()
{
    switch(tag) {
	case leaf:
	    {
	        __rope_RopeLeaf<charT,Alloc> * l =
			(__rope_RopeLeaf<charT,Alloc> *)this;
		charT * d = l -> data;
		
		if (d != c_string) {
		    free_c_string();
		}
		free_string(d, size);
		LAlloc::deallocate(l);
	    }
	    break;
	case concat:
	    {
		__rope_RopeConcatenation<charT,Alloc> * c =
			(__rope_RopeConcatenation<charT,Alloc> *)this;
		__rope_RopeBase * left = c -> left;
		__rope_RopeBase * right = c -> right;
		free_c_string();
		left -> unref_nonnil();
		right -> unref_nonnil();
		CAlloc::deallocate(c);
	    }
	    break;
	case function:
	    {
		__rope_RopeFunction<charT,Alloc> * fn =
		  	(__rope_RopeFunction<charT,Alloc> *)this;
	        free_c_string();
	        if ( fn -> delete_when_done) {
		    delete fn -> fn;
	        }
	        FAlloc::deallocate(fn);
	        break;
	    }
	case substringfn:
	    {
	        __rope_RopeSubstring<charT,Alloc> * ss =
			(__rope_RopeSubstring<charT,Alloc> *)this;
		__rope_RopeBase *base = ss -> base;
		free_c_string();
		base -> unref_nonnil();
		SAlloc::deallocate(ss);
		break;
	    }
    }
}
#else

template <class charT, class Alloc>
inline void __rope_RopeBase<charT,Alloc>::free_string(charT* s, size_t n)
{}

#endif


// Concatenate a C string onto a leaf rope by copying the rope data.
// Used for short ropes.
template <class charT, class Alloc>
rope<charT,Alloc>::RopeLeaf *
rope<charT,Alloc>::leaf_concat_char_iter
		(RopeLeaf * r, const charT * iter, size_t len)
{
    size_t old_len = r -> size;
    charT * new_data = (charT *)
	DataAlloc::allocate(rounded_up_size(old_len + len));
    RopeLeaf * result;
    
    uninitialized_copy_n(r -> data, old_len, new_data);
    uninitialized_copy_n(iter, len, new_data + old_len);
    __cond_store_eos(new_data[old_len + len]);
    __STL_TRY {
	result = RopeLeaf_from_char_ptr(new_data, old_len + len);
    }
    __STL_UNWIND(RopeBase::free_string(new_data, old_len + len));
    return result;
}

#ifndef __GC
// As above, but it's OK to clobber original if refcount is 1
template <class charT, class Alloc>
rope<charT,Alloc>::RopeLeaf *
rope<charT,Alloc>::destr_leaf_concat_char_iter
		(RopeLeaf * r, const charT * iter, size_t len)
{
    __stl_assert(r -> refcount >= 1);
    if (r -> refcount > 1) return leaf_concat_char_iter(r, iter, len);
    size_t old_len = r -> size;
    if (allocated_capacity(old_len) >= old_len + len) {
	// The space has been partially initialized for the standard
	// character types.  But that doesn't matter for those types.
	uninitialized_copy_n(iter, len, r -> data + old_len);
	if (__is_basic_char_type((charT *)0)) {
	    __cond_store_eos(r -> data[old_len + len]);
	    __stl_assert(r -> c_string == r -> data);
	} else if (r -> c_string != r -> data && 0 != r -> c_string) {
	    r -> free_c_string();
	    r -> c_string = 0;
	}
	r -> size = old_len + len;
	__stl_assert(r -> refcount == 1);
	r -> refcount = 2;
	return r;
    } else {
	RopeLeaf * result = leaf_concat_char_iter(r, iter, len);
	__stl_assert(result -> refcount == 1);
	return result;
    }
}
#endif

// Assumes left and right are not 0.
// Does not increment (nor decrement on exception) child reference counts.
// Result has ref count 1.
template <class charT, class Alloc>
rope<charT,Alloc>::RopeBase *
rope<charT,Alloc>::tree_concat (RopeBase * left, RopeBase * right)
{
    RopeConcatenation * result = CAlloc::allocate();
    unsigned char child_depth = left -> depth;
    size_t rsize;

    result -> tag = RopeBase::concat;
    result -> c_string = 0;
    result -> is_balanced = false;
    result -> size = rsize = left -> size + right -> size;
    if (right -> depth > child_depth) child_depth = right -> depth;
    unsigned char depth = (unsigned char)(child_depth + 1);
    result -> depth = depth;
    result -> left = left;
    result -> right = right;
#   ifndef __GC
	result -> refcount = 1;
	result -> init_refcount_lock();
#   endif
    if (depth > 20 && (rsize < 1000 || depth > RopeBase::max_rope_depth)) {
	RopeBase * balanced;

	__STL_TRY {
	   balanced = balance(result);
#          ifndef __GC
	     if (result != balanced) {
		__stl_assert(1 == result -> refcount
			     && 1 == balanced -> refcount);
	     }
#          endif
	   result -> unref_nonnil();
        }
	__STL_UNWIND(CAlloc::deallocate(result));
		// In case of exception, we need to deallocate
		// otherwise dangling result node.  But caller
		// still owns its children.  Thus unref is
		// inappropriate.
	return balanced;
    } else {
	return result;
    }
}

template <class charT, class Alloc>
rope<charT,Alloc>::RopeBase * rope<charT,Alloc>::concat_char_iter
		(RopeBase * r, const charT *s, size_t slen)
{
    RopeBase *result;
    if (0 == slen) {
	ref(r);
	return r;
    }
    if (0 == r) return RopeLeaf_from_unowned_char_ptr(s, slen);
    if (RopeBase::leaf == r -> tag && r -> size + slen <= copy_max) {
	result = leaf_concat_char_iter((RopeLeaf *)r, s, slen);
#       ifndef __GC
	  __stl_assert(1 == result -> refcount);
#       endif
	return result;
    }
    if (RopeBase::concat == r -> tag
	&& RopeBase::leaf == ((RopeConcatenation *)r) -> right -> tag) {
	RopeLeaf *right = (RopeLeaf *)(((RopeConcatenation *)r) -> right);
	if (right -> size + slen <= copy_max) {
	  RopeBase * left = ((RopeConcatenation *)r) -> left;
	  RopeBase * nright = leaf_concat_char_iter((RopeLeaf *)right, s, slen);
	  left -> ref_nonnil();
	  __STL_TRY {
	    result = tree_concat(left, nright);
          }
	  __STL_UNWIND(unref(left); unref(nright));
#         ifndef __GC
	    __stl_assert(1 == result -> refcount);
#         endif
	  return result;
	}
    }
    RopeBase * nright = RopeLeaf_from_unowned_char_ptr(s, slen);
    __STL_TRY {
      r -> ref_nonnil();
      result = tree_concat(r, nright);
    }
    __STL_UNWIND(unref(r); unref(nright));
#   ifndef __GC
      __stl_assert(1 == result -> refcount);
#   endif
    return result;
}

#ifndef __GC
template <class charT, class Alloc>
rope<charT,Alloc>::RopeBase * rope<charT,Alloc>
::destr_concat_char_iter
		(RopeBase * r, const charT *s, size_t slen)
{
    RopeBase *result;
    if (0 == r) return RopeLeaf_from_unowned_char_ptr(s, slen);
    size_t count = r -> refcount;
    size_t orig_size = r -> size;
    __stl_assert(count >= 1);
    if (count > 1) return concat_char_iter(r, s, slen);
    if (0 == slen) {
	r -> refcount = 2;      // One more than before
	return r;
    }
    if (orig_size + slen <= copy_max && RopeBase::leaf == r -> tag) {
	result = destr_leaf_concat_char_iter((RopeLeaf *)r, s, slen);
	return result;
    }
    if (RopeBase::concat == r -> tag) {
	RopeLeaf *right = (RopeLeaf *)(((RopeConcatenation *)r) -> right);
	if (RopeBase::leaf == right -> tag
	    && right -> size + slen <= copy_max) {
	  RopeBase * new_right = destr_leaf_concat_char_iter(right, s, slen);
	  if (right == new_right) {
	      __stl_assert(new_right -> refcount == 2);
	      new_right -> refcount = 1;
	  } else {
	      __stl_assert(new_right -> refcount >= 1);
	      right -> unref_nonnil();
	  }
	  __stl_assert(r -> refcount == 1);
	  r -> refcount = 2;    // One more than before.
	  ((RopeConcatenation *)r) -> right = new_right;
	  r -> size = orig_size + slen;
	  if (0 != r -> c_string) {
	      r -> free_c_string();
	      r -> c_string = 0;
	  }
	  return r;
	}
    }
    RopeBase *right = RopeLeaf_from_unowned_char_ptr(s, slen);
    r -> ref_nonnil();
    __STL_TRY {
      result = tree_concat(r, right);
    }
    __STL_UNWIND(unref(r); unref(right))
    __stl_assert(1 == result -> refcount);
    return result;
}
#endif /* !__GC */

template <class charT, class Alloc>
rope<charT,Alloc>::RopeBase *
rope<charT,Alloc>::concat(RopeBase * left, RopeBase * right)
{
    if (0 == left) {
	ref(right);
	return right;
    }
    if (0 == right) {
	left -> ref_nonnil();
	return left;
    }
    if (RopeBase::leaf == right -> tag) {
	if (RopeBase::leaf == left -> tag) {
	  if (right -> size + left -> size <= copy_max) {
	    return leaf_concat_char_iter((RopeLeaf *)left,
					 ((RopeLeaf *)right) -> data,
					 right -> size);
	  }
	} else if (RopeBase::concat == left -> tag
		   && RopeBase::leaf ==
		      ((RopeConcatenation *)left) -> right -> tag) {
	  RopeLeaf * leftright =
		    (RopeLeaf *)(((RopeConcatenation *)left) -> right); 
	  if (leftright -> size + right -> size <= copy_max) {
	    RopeBase * leftleft = ((RopeConcatenation *)left) -> left;
	    RopeBase * rest = leaf_concat_char_iter(leftright,
					   ((RopeLeaf *)right) -> data,
					   right -> size);
	    leftleft -> ref_nonnil();
	    __STL_TRY {
	      return(tree_concat(leftleft, rest));
            }
	    __STL_UNWIND(unref(leftleft); unref(rest))
	  }
	}
    }
    left -> ref_nonnil();
    right -> ref_nonnil();
    __STL_TRY {
      return(tree_concat(left, right));
    }
    __STL_UNWIND(unref(left); unref(right));
}

template <class charT, class Alloc>
rope<charT,Alloc>::RopeBase *
rope<charT,Alloc>::substring(RopeBase * base, size_t start, size_t endp1)
{
    if (0 == base) return 0;
    size_t len = base -> size;
    size_t adj_endp1;
    const size_t lazy_threshold = 128;
    
    if (endp1 >= len) {
	if (0 == start) {
	    base -> ref_nonnil();
	    return base;
	} else {
	    adj_endp1 = len;
	}
    } else {
	adj_endp1 = endp1;
    }
    switch(base -> tag) {
	case RopeBase::concat:
	    {
		RopeConcatenation *c = (RopeConcatenation *)base;
		RopeBase *left = c -> left;
		RopeBase *right = c -> right;
		size_t left_len = left -> size;
		RopeBase * result;

		if (adj_endp1 <= left_len) {
		    return substring(left, start, endp1);
		} else if (start >= left_len) {
		    return substring(right, start - left_len,
				  adj_endp1 - left_len);
		}
		self_destruct_ptr left_result(substring(left, start,
							left_len));
		self_destruct_ptr right_result(
				substring(right, 0, endp1 - left_len));
		result = concat(left_result, right_result);
#               ifndef __GC
		  __stl_assert(1 == result -> refcount);
#               endif
		return result;
	    }
	case RopeBase::leaf:
	    {
		RopeLeaf * l = (RopeLeaf *)base;
		RopeLeaf * result;
		size_t result_len;
		if (start >= adj_endp1) return 0;
		result_len = adj_endp1 - start;
		if (result_len > lazy_threshold) goto lazy;
#               ifdef __GC
		    const charT *section = l -> data + start;
		    result = RopeLeaf_from_char_ptr(section, result_len);
		    result -> c_string = 0;  // Not eos terminated.
#               else
		    // We should sometimes create substring node instead.
		    result = RopeLeaf_from_unowned_char_ptr(
					l -> data + start, result_len);
#               endif
		return result;
	    }
	case RopeBase::substringfn:
	    // Avoid introducing mutiple layers of substring nodes.
	    {
		RopeSubstring *old = (RopeSubstring *)base;
		size_t result_len;
		if (start >= adj_endp1) return 0;
		result_len = adj_endp1 - start;
		if (result_len > lazy_threshold) {
		    RopeSubstring * space = SAlloc::allocate();
		    RopeSubstring * result =
			new(space) RopeSubstring(old -> base,
						 start + old -> start,
						 adj_endp1 - start);
		    return result;
		} // else fall through:
	    }
	case RopeBase::function:
	    {
		RopeFunction * f = (RopeFunction *)base;
		charT *section;
		size_t result_len;
		if (start >= adj_endp1) return 0;
		result_len = adj_endp1 - start;

		if (result_len > lazy_threshold) goto lazy;
		section = (charT *)
			DataAlloc::allocate(rounded_up_size(result_len));
		__STL_TRY {
		  (*(f -> fn))(start, result_len, section);
                }
		__STL_UNWIND(RopeBase::free_string(section, result_len));
		__cond_store_eos(section[result_len]);
		return RopeLeaf_from_char_ptr(section, result_len);
	    }
    }
    /*NOTREACHED*/
    __stl_assert(false);
  lazy:
    {
	// Create substring node.
	RopeSubstring * space = SAlloc::allocate();
	RopeSubstring * result = new(space) RopeSubstring(base, start,
							  adj_endp1 - start);
	return result;
    }
}

template<class charT>
class __rope_flatten_char_consumer : public __rope_char_consumer<charT> {
    private:
	charT * buf_ptr;
    public:
	charT * buffer;
	__rope_flatten_char_consumer(charT * buffer) {
	    buf_ptr = buffer;
	};
	~__rope_flatten_char_consumer() {}
	bool operator() (const charT* leaf, size_t n) {
	    uninitialized_copy_n(leaf, n, buf_ptr);
	    buf_ptr += n;
	    return true;
	}
};
	    
template<class charT>
class __rope_find_char_char_consumer : public __rope_char_consumer<charT> {
    private:
	charT pattern;
    public:
	size_t count;  // Number of nonmatching characters
	__rope_find_char_char_consumer(charT p) : pattern(p), count(0) {}
	~__rope_find_char_char_consumer() {}
	bool operator() (const charT* leaf, size_t n) {
	    size_t i;
	    for (i = 0; i < n; i++) {
		if (leaf[i] == pattern) {
		    count += i; return false;
		}
	    }
	    count += n; return true;
	}
};
	    
template<class charT>
class __rope_insert_char_consumer : public __rope_char_consumer<charT> {
    private:
	typedef ostream insert_ostream;
	insert_ostream & o;
    public:
	charT * buffer;
	__rope_insert_char_consumer(insert_ostream & writer) : o(writer) {};
	~__rope_insert_char_consumer() { };
		// Caller is presumed to own the ostream
	bool operator() (const charT* leaf, size_t n);
		// Returns true to continue traversal.
};
	    
template<class charT>
bool __rope_insert_char_consumer<charT>::operator()
					(const charT * leaf, size_t n)
{
    size_t i;
    //  We assume that formatting is set up correctly for each element.
    for (i = 0; i < n; i++) o << leaf[i];
    return true;
}

inline bool __rope_insert_char_consumer<char>::operator()
					(const char * leaf, size_t n)
{
    size_t i;
    for (i = 0; i < n; i++) o.put(leaf[i]);
    return true;
}

#if !defined(_MSC_VER) && !defined(__BORLANDC__)
// I couldn't get this to work with the VC++ version of basic_ostream.
inline bool __rope_insert_char_consumer<wchar_t>::operator()
					(const wchar_t * leaf, size_t n)
{
    size_t i;
    for (i = 0; i < n; i++) o.put(leaf[i]);
    return true;
}
#endif /* !_MSC_VER  && !BORLAND */

template <class charT, class Alloc>
bool rope<charT, Alloc>::apply_to_pieces(
				__rope_char_consumer<charT>& c,
				const RopeBase * r,
				size_t begin, size_t end)
{
    if (0 == r) return true;
    switch(r -> tag) {
	case RopeBase::concat:
	    {
		RopeConcatenation *conc = (RopeConcatenation *)r;
		RopeBase *left = conc -> left;
		size_t left_len = left -> size;
		if (begin < left_len) {
		    size_t left_end = min(left_len, end);
		    if (!apply_to_pieces(c, left, begin, left_end)) {
			return false;
		    }
		}
		if (end > left_len) {
		    RopeBase *right = conc -> right;
		    size_t right_start = max(left_len, begin);
		    if (!apply_to_pieces(c, right,
					 right_start - left_len,
					 end - left_len)) {
			return false;
		    }
		}
	    }
	    return true;
	case RopeBase::leaf:
	    {
		RopeLeaf * l = (RopeLeaf *)r;
		return c(l -> data + begin, end - begin);
	    }
	case RopeBase::function:
	case RopeBase::substringfn:
	    {
		RopeFunction * f = (RopeFunction *)r;
		size_t len = end - begin;
		bool result;
		charT * buffer = DataAlloc::allocate(len);
		__STL_TRY {
		  (*(f -> fn))(begin, end, buffer);
		  result = c(buffer, len);
                  DataAlloc::deallocate(buffer, len);
                }
		__STL_UNWIND(DataAlloc::deallocate(buffer, len))
		return result;
	    }
	default:
	    __stl_assert(false);
	    /*NOTREACHED*/
	    return false;
    }
}

inline void __rope_fill(ostream& o, size_t n)
{
    char f = o.fill();
    size_t i;

    for (i = 0; i < n; i++) o.put(f);
}
    

template <class charT> inline bool __rope_is_simple(charT *) { return false; }
inline bool __rope_is_simple(char *) { return true; }
inline bool __rope_is_simple(wchar_t *) { return true; }


template<class charT, class Alloc>
ostream& operator<< (ostream& o, const rope<charT, Alloc>& r)
{
    size_t w = o.width();
    bool left = bool(o.flags() & ios::left);
    size_t pad_len;
    size_t rope_len = r.size();
    __rope_insert_char_consumer<charT> c(o);
    bool is_simple = __rope_is_simple((charT *)0);
    
    if (rope_len < w) {
	pad_len = w - rope_len;
    } else {
	pad_len = 0;
    }
    if (!is_simple) o.width(w/rope_len);
    __STL_TRY {
      if (is_simple && !left && pad_len > 0) {
	__rope_fill(o, pad_len);
      }
      r.apply_to_pieces(0, r.size(), c);
      if (is_simple && left && pad_len > 0) {
	__rope_fill(o, pad_len);
      }
      if (!is_simple)
        o.width(w);
    }
    __STL_UNWIND(if (!is_simple) o.width(w))
    return o;
}

template <class charT, class Alloc>
charT *
rope<charT,Alloc>::flatten(RopeBase * r,
				 size_t start, size_t len,
				 charT * buffer)
{
    __rope_flatten_char_consumer<charT> c(buffer);
    apply_to_pieces(c, r, start, start + len);
    return(buffer + len);
}

template <class charT, class Alloc>
size_t
rope<charT,Alloc>::find(charT pattern, size_t start) const
{
    __rope_find_char_char_consumer<charT> c(pattern);
    apply_to_pieces(c, tree_ptr, start, size());
    return start + c.count;
}

template <class charT, class Alloc>
charT *
rope<charT,Alloc>::flatten(RopeBase * r, charT * buffer)
{
    if (0 == r) return buffer;
    switch(r -> tag) {
	case RopeBase::concat:
	    {
		RopeConcatenation *c = (RopeConcatenation *)r;
		RopeBase *left = c -> left;
		RopeBase *right = c -> right;
		charT * rest = flatten(left, buffer);
		return flatten(right, rest);
	    }
	case RopeBase::leaf:
	    {
		RopeLeaf * l = (RopeLeaf *)r;
		return copy_n(l -> data, l -> size, buffer).second;
	    }
	case RopeBase::function:
	case RopeBase::substringfn:
	    // We dont yet do anything with substring nodes.
	    // This needs to be fixed before ropefiles will work well.
	    {
		RopeFunction * f = (RopeFunction *)r;
		(*(f -> fn))(0, f -> size, buffer);
		return buffer + f -> size;
	    }
	default:
	    __stl_assert(false);
	    /*NOTREACHED*/
	    return 0;
    }
}


// This needs work for charT != char
template <class charT, class Alloc>
void
rope<charT,Alloc>::dump(RopeBase * r, int indent)
{
    for (int i = 0; i < indent; i++) putchar(' ');
    if (0 == r) {
	printf("NULL\n"); return;
    }
    if (RopeBase::concat == r -> tag) {
	RopeConcatenation *c = (RopeConcatenation *)r;
	RopeBase *left = c -> left;
	RopeBase *right = c -> right;

#       ifdef __GC
	  printf("Concatenation %p (depth = %d, len = %ld, %s balanced)\n",
		 r, r -> depth, r -> size, r -> is_balanced? "" : "not");
#       else
	  printf("Concatenation %p (rc = %ld, depth = %d, len = %ld, %s balanced)\n",
		 r, r -> refcount, r -> depth, r -> size,
		 r -> is_balanced? "" : "not");
#       endif
	dump(left, indent + 2);
	dump(right, indent + 2);
	return;
    } else {
	char * kind;

	switch (r -> tag) {
	    case RopeBase::leaf:
		kind = "Leaf";
		break;
	    case RopeBase::function:
		kind = "Function";
		break;
	    case RopeBase::substringfn:
		kind = "Function representing substring";
		break;
	    default:
		kind = "(corrupted kind field!)";
	}
#       ifdef __GC
	  printf("%s %p (depth = %d, len = %ld) ",
		 kind, r, r -> depth, r -> size);
#       else
	  printf("%s %p (rc = %ld, depth = %d, len = %ld) ",
		 kind, r, r -> refcount, r -> depth, r -> size);
#       endif
	if (__is_one_byte_char_type((charT *)0)) {
	    const int max_len = 40;
	    self_destruct_ptr prefix(substring(r, 0, max_len));
	    charT buffer[max_len + 1];
	    bool too_big = r -> size > prefix-> size;

	    flatten(prefix, buffer);
	    buffer[prefix -> size] = __eos((charT *)0); 
	    printf("%s%s\n", (char *)buffer, too_big? "...\n" : "\n");
	} else {
	    printf("\n");
	}
    }
}

template <class charT, class Alloc>
const unsigned long
rope<charT,Alloc>::min_len[__rope_RopeBase<charT,Alloc>::max_rope_depth + 1] = {
/* 0 */1, /* 1 */2, /* 2 */3, /* 3 */5, /* 4 */8, /* 5 */13, /* 6 */21,
/* 7 */34, /* 8 */55, /* 9 */89, /* 10 */144, /* 11 */233, /* 12 */377,
/* 13 */610, /* 14 */987, /* 15 */1597, /* 16 */2584, /* 17 */4181,
/* 18 */6765, /* 19 */10946, /* 20 */17711, /* 21 */28657, /* 22 */46368,
/* 23 */75025, /* 24 */121393, /* 25 */196418, /* 26 */317811,
/* 27 */514229, /* 28 */832040, /* 29 */1346269, /* 30 */2178309,
/* 31 */3524578, /* 32 */5702887, /* 33 */9227465, /* 34 */14930352,
/* 35 */24157817, /* 36 */39088169, /* 37 */63245986, /* 38 */102334155,
/* 39 */165580141, /* 40 */267914296, /* 41 */433494437,
/* 42 */701408733, /* 43 */1134903170, /* 44 */1836311903,
/* 45 */2971215073 };
// These are Fibonacci numbers < 2**32.

template <class charT, class Alloc>
rope<charT,Alloc>::RopeBase *
rope<charT,Alloc>::balance(RopeBase *r)
{
    RopeBase * forest[RopeBase::max_rope_depth + 1];
    RopeBase * result = 0;
    int i;
    // Inariant:
    // The concatenation of forest in descending order is equal to r.
    // forest[i].size >= min_len[i]
    // forest[i].depth = i
    // References from forest are included in refcount.

    for (i = 0; i <= RopeBase::max_rope_depth; ++i) forest[i] = 0;
    __STL_TRY {
      add_to_forest(r, forest);
      for (i = 0; i <= RopeBase::max_rope_depth; ++i) if (0 != forest[i]) {
#	ifndef __GC
	  self_destruct_ptr old(result);
#	endif
	result = concat(forest[i], result);
	forest[i] -> unref_nonnil();
#	if !defined(__GC) && defined(__STL_USE_EXCEPTIONS)
	  forest[i] = 0;
#	endif
      }
    }
    __STL_UNWIND(for(i = 0; i <= RopeBase::max_rope_depth; i++)
		 unref(forest[i]))
    if (result -> depth > RopeBase::max_rope_depth) abort();
    return(result);
}


template <class charT, class Alloc>
void
rope<charT,Alloc>::add_to_forest(RopeBase *r, RopeBase **forest)
{
    if (r -> is_balanced) {
	add_leaf_to_forest(r, forest);
	return;
    }
    __stl_assert(r -> tag == RopeBase::concat);
    {
	RopeConcatenation *c = (RopeConcatenation *)r;

	add_to_forest(c -> left, forest);
	add_to_forest(c -> right, forest);
    }
}


template <class charT, class Alloc>
void
rope<charT,Alloc>::add_leaf_to_forest(RopeBase *r, RopeBase **forest)
{
    RopeBase * insertee;   		// included in refcount
    RopeBase * too_tiny = 0;    	// included in refcount
    int i;  				// forest[0..i-1] is empty
    size_t s = r -> size;

    for (i = 0; s >= min_len[i+1]/* not this bucket */; ++i) {
	if (0 != forest[i]) {
#	    ifndef __GC
	      self_destruct_ptr old(too_tiny);
#	    endif
	    too_tiny = concat_and_set_balanced(forest[i], too_tiny);
	    forest[i] -> unref_nonnil();
	    forest[i] = 0;
	}
    }
    {
#	ifndef __GC
	  self_destruct_ptr old(too_tiny);
#	endif
	insertee = concat_and_set_balanced(too_tiny, r);
    }
    // Too_tiny dead, and no longer included in refcount.
    // Insertee is live and included.
    __stl_assert(is_almost_balanced(insertee));
    __stl_assert(insertee -> depth <= r -> depth + 1);
    for (;; ++i) {
	if (0 != forest[i]) {
#	    ifndef __GC
	      self_destruct_ptr old(insertee);
#	    endif
	    insertee = concat_and_set_balanced(forest[i], insertee);
	    forest[i] -> unref_nonnil();
	    forest[i] = 0;
	    __stl_assert(is_almost_balanced(insertee));
	}
	__stl_assert(min_len[i] <= insertee -> size);
	__stl_assert(forest[i] == 0);
	if (i == RopeBase::max_rope_depth
	    || insertee -> size < min_len[i+1]) {
	    forest[i] = insertee;
	    // refcount is OK since insertee is now dead.
	    return;
	}
    }
}

template <class charT, class Alloc>
charT
rope<charT,Alloc>::fetch(RopeBase *r, size_type i)
{
    __GC_CONST charT * cstr = r -> c_string;

    __stl_assert(i < r -> size);
    if (0 != cstr) return cstr[i]; 
    for(;;) {
      switch(r -> tag) {
	case RopeBase::concat:
	    {
		RopeConcatenation *c = (RopeConcatenation *)r;
		RopeBase *left = c -> left;
		size_t left_len = left -> size;

		if (i >= left_len) {
		    i -= left_len;
		    r = c -> right;
		} else {
		    r = left;
		}
	    }
	    break;
	case RopeBase::leaf:
	    {
		RopeLeaf * l = (RopeLeaf *)r;
		return l -> data[i];
	    }
	case RopeBase::function:
	case RopeBase::substringfn:
	    {
		RopeFunction * f = (RopeFunction *)r;
		charT result;

		(*(f -> fn))(i, 1, &result);
		return result;
	    }
      }
    }
}

# ifndef __GC
// Return a uniquely referenced character slot for the given
// position, or 0 if that's not possible.
template <class charT, class Alloc>
charT*
rope<charT,Alloc>::fetch_ptr(RopeBase *r, size_type i)
{
    RopeBase * clrstack[RopeBase::max_rope_depth];
    size_t csptr = 0;

    for(;;) {
      if (r -> refcount > 1) return 0;
      switch(r -> tag) {
	case RopeBase::concat:
	    {
		RopeConcatenation *c = (RopeConcatenation *)r;
		RopeBase *left = c -> left;
		size_t left_len = left -> size;

		if (c -> c_string != 0) clrstack[csptr++] = c;
		if (i >= left_len) {
		    i -= left_len;
		    r = c -> right;
		} else {
		    r = left;
		}
	    }
	    break;
	case RopeBase::leaf:
	    {
		RopeLeaf * l = (RopeLeaf *)r;
		if (l -> c_string != l -> data && l -> c_string != 0)
		    clrstack[csptr++] = l;
		while (csptr > 0) {
		    -- csptr;
		    RopeBase * d = clrstack[csptr];
		    d -> free_c_string();
		    d -> c_string = 0;
		}
		return l -> data + i;
	    }
	case RopeBase::function:
	case RopeBase::substringfn:
	    return 0;
      }
    }
}
# endif /* __GC */

// The following could be implemented trivially using
// lexicographical_compare_3way.
// We do a little more work to avoid dealing with rope iterators for
// flat strings.
template <class charT, class Alloc>
int
rope<charT,Alloc>::compare (const RopeBase *left, const RopeBase *right)
{
    size_t left_len;
    size_t right_len;

    if (0 == right) return 0 != left;
    if (0 == left) return -1;
    left_len = left -> size;
    right_len = right -> size;
    if (RopeBase::leaf == left -> tag) {
	RopeLeaf *l = (RopeLeaf *) left;
	if (RopeBase::leaf == right -> tag) {
	    RopeLeaf *r = (RopeLeaf *) right;
	    return lexicographical_compare_3way(
			l -> data, l -> data + left_len,
			r -> data, r -> data + right_len);
	} else {
	    const_iterator rstart(right, 0);
	    const_iterator rend(right, right_len);
	    return lexicographical_compare_3way(
			l -> data, l -> data + left_len,
			rstart, rend);
	}
    } else {
	const_iterator lstart(left, 0);
	const_iterator lend(left, left_len);
	if (RopeBase::leaf == right -> tag) {
	    RopeLeaf *r = (RopeLeaf *) right;
	    return lexicographical_compare_3way(
				   lstart, lend,
				   r -> data, r -> data + right_len);
	} else {
	    const_iterator rstart(right, 0);
	    const_iterator rend(right, right_len);
	    return lexicographical_compare_3way(
				   lstart, lend,
				   rstart, rend);
	}
    }
}

// Assignment to reference proxies.
template <class charT, class Alloc>
__rope_charT_ref_proxy<charT, Alloc>&
__rope_charT_ref_proxy<charT, Alloc>::operator= (charT c) {
    RopeBase * old = root -> tree_ptr;
#   ifndef __GC
	// First check for the case in which everything is uniquely
	// referenced.  In that case we can do this destructively.
	charT * charT_ptr = my_rope::fetch_ptr(old, pos);
	if (0 != charT_ptr) {
	    *charT_ptr = c;
	    return *this;
	}
#   endif
    self_destruct_ptr left(my_rope::substring(old, 0, pos));
    self_destruct_ptr right(my_rope::substring(old, pos+1, old -> size));
    self_destruct_ptr result_left(my_rope::destr_concat_char_iter(left, &c, 1));
#   ifndef __GC
      __stl_assert(left == result_left || 1 == result_left -> refcount);
#   endif
    RopeBase * result =
		my_rope::concat(result_left, right);
#   ifndef __GC
      __stl_assert(1 <= result -> refcount);
      RopeBase::unref(old);
#   endif
    root -> tree_ptr = result;
    return *this;
}

template <class charT, class Alloc>
inline __rope_charT_ref_proxy<charT, Alloc>::operator charT () const
{
    if (current_valid) {
	return current;
    } else {
        return my_rope::fetch(root->tree_ptr, pos);
    }
}
template <class charT, class Alloc>
__rope_charT_ptr_proxy<charT, Alloc>
__rope_charT_ref_proxy<charT, Alloc>::operator& () const {
    return __rope_charT_ptr_proxy<charT, Alloc>(*this);
}

template <class charT, class Alloc>
rope<charT, Alloc>::rope(size_t n, charT c)
{
    rope result;
    const size_t exponentiate_threshold = 32;
    size_t exponent;
    size_t rest;
    charT *rest_buffer;
    RopeBase * remainder;
    rope remainder_rope;

    if (0 == n) { tree_ptr = 0; return; }
    exponent = n / exponentiate_threshold;
    rest = n % exponentiate_threshold;
    if (0 == rest) {
	remainder = 0;
    } else {
	rest_buffer = DataAlloc::allocate(rounded_up_size(rest));
	uninitialized_fill_n(rest_buffer, rest, c);
	__cond_store_eos(rest_buffer[rest]);
	__STL_TRY {
	    remainder = RopeLeaf_from_char_ptr(rest_buffer, rest);
        }
	__STL_UNWIND(RopeBase::free_string(rest_buffer, rest))
    }
    remainder_rope.tree_ptr = remainder;
    if (exponent != 0) {
	charT * base_buffer =
		DataAlloc::allocate(rounded_up_size(exponentiate_threshold));
	RopeLeaf * base_leaf;
	rope base_rope;
	uninitialized_fill_n(base_buffer, exponentiate_threshold, c);
	__cond_store_eos(base_buffer[exponentiate_threshold]);
	__STL_TRY {
          base_leaf = RopeLeaf_from_char_ptr(base_buffer,
                                             exponentiate_threshold);
        }
	__STL_UNWIND(RopeBase::free_string(base_buffer, exponentiate_threshold))
	base_rope.tree_ptr = base_leaf;
 	if (1 == exponent) {
	  result = base_rope;
#         ifndef __GC
	    __stl_assert(1 == result -> tree_ptr -> refcount);
#         endif
	} else {
	  result = power(base_rope, exponent, concat_fn());
	}
	if (0 != remainder) {
	  result += remainder_rope;
	}
    } else {
	result = remainder_rope;
    }
    tree_ptr = result.tree_ptr;
    tree_ptr -> ref_nonnil();
}

template<class charT, class Alloc> charT rope<charT,Alloc>::empty_c_str[1];

# ifdef __STL_PTHREADS
    template<class charT, class Alloc>
    pthread_mutex_t rope<charT,Alloc>::swap_lock = PTHREAD_MUTEX_INITIALIZER;
# endif

template<class charT, class Alloc>
const charT * rope<charT,Alloc>::c_str() const {
    if (0 == tree_ptr) {
        empty_c_str[0] = __eos((charT *)0);  // Possibly redundant,
					     // but probably fast.
        return empty_c_str;
    }
    __GC_CONST charT * old_c_string = tree_ptr -> c_string;
    if (0 != old_c_string) return(old_c_string);
    size_t s = size();
    charT * result = DataAlloc::allocate(s + 1);
    flatten(tree_ptr, result);
    result[s] = __eos((charT *)0);
#   ifdef __GC
	tree_ptr -> c_string = result;
#   else
      if ((old_c_string = atomic_swap(&(tree_ptr -> c_string), result)) != 0) {
	// It must have been added in the interim.  Hence it had to have been
	// separately allocated.  Deallocate the old copy, since we just
	// replaced it.
	destroy(old_c_string, old_c_string + s + 1);
	DataAlloc::deallocate(old_c_string, s + 1);
      }
#   endif
    return(result);
}

template<class charT, class Alloc>
const charT * rope<charT,Alloc>::replace_with_c_str() {
    if (0 == tree_ptr) {
        empty_c_str[0] = __eos((charT *)0);
        return empty_c_str;
    }
    __GC_CONST charT * old_c_string = tree_ptr -> c_string;
    if (RopeBase::leaf == tree_ptr -> tag && 0 != old_c_string) {
	return(old_c_string);
    }
    size_t s = size();
    charT * result = DataAlloc::allocate(rounded_up_size(s));
    flatten(tree_ptr, result);
    result[s] = __eos((charT *)0);
    tree_ptr -> unref_nonnil();
    tree_ptr = RopeLeaf_from_char_ptr(result, s);
    return(result);
}

// Algorithm specializations.  More should be added.

#ifndef _MSC_VER
// I couldn't get this to work with VC++
template<class charT,class Alloc>
void
__rope_rotate(__rope_iterator<charT,Alloc> first,
              __rope_iterator<charT,Alloc> middle,
              __rope_iterator<charT,Alloc> last) {
    __stl_assert(first.container() == middle.container()
                 && middle.container() == last.container());
    rope<charT,Alloc>& r(first.container());
    rope<charT,Alloc> prefix = r.substr(0, first.index());
    rope<charT,Alloc> suffix = r.substr(last.index(), r.size() - last.index());
    rope<charT,Alloc> part1 = r.substr(middle.index(),
                                       last.index() - middle.index());
    rope<charT,Alloc> part2 = r.substr(first.index(),
                                       middle.index() - first.index());
    r = prefix;
    r += part1;
    r += part2;
    r += suffix;
}

inline void rotate(__rope_iterator<char,__ALLOC> first,
                   __rope_iterator<char,__ALLOC> middle,
                   __rope_iterator<char,__ALLOC> last) {
    __rope_rotate(first, middle, last);
}

# if 0
// Probably not useful for several reasons:
// - for SGIs 7.1 compiler and probably some others,
//   this forces lots of rope<wchar_t, ...> instantiations, creating a
//   code bloat and compile time problem.  (Fixed in 7.2.)
// - wchar_t is 4 bytes wide on most UNIX platforms, making it unattractive
//   for unicode strings.  Unsigned short may be a better character
//   type.
inline void rotate(__rope_iterator<wchar_t,__ALLOC> first,
                   __rope_iterator<wchar_t,__ALLOC> middle,
                   __rope_iterator<wchar_t,__ALLOC> last) {
    __rope_rotate(first, middle, last);
}
# endif
#endif /* _MSC_VER */

#if defined(__sgi) && !defined(__GNUC__) && (_MIPS_SIM != _MIPS_SIM_ABI32)
#pragma reset woff 1174
#endif

__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996
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

// vector<bool> is replaced by bit_vector at present because partial 
// specialization is not yet implemented.  

#ifndef __SGI_STL_BVECTOR_H
#define __SGI_STL_BVECTOR_H

#include <stddef.h>
#include <algobase.h>
#include <alloc.h>


#define __WORD_BIT (int(CHAR_BIT*sizeof(unsigned int)))

class bit_vector {
public:
    typedef bool value_type;
    typedef size_t size_type;
    typedef ptrdiff_t difference_type; 

    class iterator;
    class const_iterator;

    class reference {
      friend class iterator;
      friend class const_iterator;
    protected:
	unsigned int* p;
	unsigned int mask;
	reference(unsigned int* x, unsigned int y) : p(x), mask(y) {}
    public:
	reference() : p(0), mask(0) {}
	operator bool() const { return !(!(*p & mask)); }
	reference& operator=(bool x) {
	    if (x)      
		*p |= mask;
	    else 
		*p &= ~mask;
	    return *this;
	}
	reference& operator=(const reference& x) { return *this = bool(x); }
	bool operator==(const reference& x) const {
	    return bool(*this) == bool(x);
	}
	bool operator<(const reference& x) const {
	    return bool(*this) < bool(x);
	}
	void flip() { *p ^= mask; }
    };

    typedef bool const_reference;

    typedef reference bit_reference;
    typedef const_reference bit_const_reference;

    class iterator : public random_access_iterator<bool, difference_type> {
      friend class bit_vector;
      friend class const_iterator;
    public:
      typedef bit_reference  reference;
      typedef bit_reference* pointer;
    protected:
	unsigned int* p;
	unsigned int offset;
	void bump_up() {
	    if (offset++ == __WORD_BIT - 1) {
		offset = 0;
		++p;
	    }
	}
    void bump_down() {
	if (offset-- == 0) {
	    offset = __WORD_BIT - 1;
	    --p;
	}
    }
    public:
	iterator() : p(0), offset(0) {}
	iterator(unsigned int* x, unsigned int y) : p(x), offset(y) {}
	reference operator*() const { return reference(p, 1U << offset); }
	iterator& operator++() {
	    bump_up();
	    return *this;
	}
	iterator operator++(int) {
	    iterator tmp = *this;
	    bump_up();
	    return tmp;
	}
	iterator& operator--() {
	    bump_down();
	    return *this;
	}
	iterator operator--(int) {
	    iterator tmp = *this;
	    bump_down();
	    return tmp;
	}
	iterator& operator+=(difference_type i) {
	    difference_type n = i + offset;
	    p += n / __WORD_BIT;
	    n = n % __WORD_BIT;
	    if (n < 0) {
		offset = n + __WORD_BIT;
		--p;
	    } else
		offset = n;
	    return *this;
	}
	iterator& operator-=(difference_type i) {
	    *this += -i;
	    return *this;
	}
	iterator operator+(difference_type i) const {
	    iterator tmp = *this;
	    return tmp += i;
	}
	iterator operator-(difference_type i) const {
	    iterator tmp = *this;
	    return tmp -= i;
	}
	difference_type operator-(iterator x) const {
	    return __WORD_BIT * (p - x.p) + offset - x.offset;
	}
	reference operator[](difference_type i) { return *(*this + i); }
	bool operator==(const iterator& x) const {
	    return p == x.p && offset == x.offset;
	}
	bool operator!=(const iterator& x) const {
	    return p != x.p || offset != x.offset;
	}
	bool operator<(iterator x) const {
	    return p < x.p || (p == x.p && offset < x.offset);
	}
    };

    class const_iterator : public random_access_iterator<bool, difference_type>
    {
      friend class bit_vector;
    public:
      typedef bit_const_reference reference;
      typedef const bool*         pointer;
    protected:
	unsigned int* p;
	unsigned int offset;
	void bump_up() {
	    if (offset++ == __WORD_BIT - 1) {
		offset = 0;
		++p;
	    }
	}
    void bump_down() {
	if (offset-- == 0) {
	    offset = __WORD_BIT - 1;
	    --p;
	}
    }
    public:
	const_iterator() : p(0), offset(0) {}
	const_iterator(unsigned int* x, unsigned int y) : p(x), offset(y) {}
	const_iterator(const iterator& x) : p(x.p), offset(x.offset) {}
	const_reference operator*() const {
	    return bit_vector::reference(p, 1U << offset);
	}
	const_iterator& operator++() {
	    bump_up();
	    return *this;
	}
	const_iterator operator++(int) {
	    const_iterator tmp = *this;
	    bump_up();
	    return tmp;
	}
	const_iterator& operator--() {
	    bump_down();
	    return *this;
	}
	const_iterator operator--(int) {
	    const_iterator tmp = *this;
	    bump_down();
	    return tmp;
	}
	const_iterator& operator+=(difference_type i) {
	    difference_type n = i + offset;
	    p += n / __WORD_BIT;
	    n = n % __WORD_BIT;
	    if (n < 0) {
		offset = n + __WORD_BIT;
		--p;
	    } else
		offset = n;
	    return *this;
	}
	const_iterator& operator-=(difference_type i) {
	    *this += -i;
	    return *this;
	}
	const_iterator operator+(difference_type i) const {
	    const_iterator tmp = *this;
	    return tmp += i;
	}
	const_iterator operator-(difference_type i) const {
	    const_iterator tmp = *this;
	    return tmp -= i;
	}
	difference_type operator-(const_iterator x) const {
	    return __WORD_BIT * (p - x.p) + offset - x.offset;
	}
	const_reference operator[](difference_type i) { 
	    return *(*this + i); 
	}
	bool operator==(const const_iterator& x) const {
	    return p == x.p && offset == x.offset;
	}
	bool operator!=(const const_iterator& x) const {
	    return p != x.p || offset != x.offset;
	}
	bool operator<(const_iterator x) const {
	    return p < x.p || (p == x.p && offset < x.offset);
	}
    };

#ifdef __STL_CLASS_PARTIAL_SPECIALIZATION
    typedef reverse_iterator<const_iterator> const_reverse_iterator;
    typedef reverse_iterator<iterator> reverse_iterator;
#else /* __STL_CLASS_PARTIAL_SPECIALIZATION */
    typedef reverse_iterator<const_iterator, value_type, const_reference, 
                             difference_type> const_reverse_iterator;
    typedef reverse_iterator<iterator, value_type, reference, difference_type>
        reverse_iterator;
#endif /* __STL_CLASS_PARTIAL_SPECIALIZATION */

protected:
    typedef simple_alloc<unsigned int, alloc> data_allocator;
    iterator start;
    iterator finish;
    unsigned int* end_of_storage;
    unsigned int* bit_alloc(size_type n) {
	return data_allocator::allocate((n + __WORD_BIT - 1)/__WORD_BIT);
    }
    void deallocate() {
      if (start.p)
        data_allocator::deallocate(start.p, end_of_storage - start.p);
    }
    void initialize(size_type n) {
	unsigned int* q = bit_alloc(n);
	end_of_storage = q + (n + __WORD_BIT - 1)/__WORD_BIT;
	start = iterator(q, 0);
	finish = start + n;
    }
    void insert_aux(iterator position, bool x) {
      if (finish.p != end_of_storage) {
	copy_backward(position, finish, finish + 1);
	*position = x;
	++finish;
      } else {
	size_type len = size() ? 2 * size() : __WORD_BIT;
	unsigned int* q = bit_alloc(len);
	iterator i = copy(begin(), position, iterator(q, 0));
	*i++ = x;
	finish = copy(position, end(), i);
	deallocate();
	end_of_storage = q + (len + __WORD_BIT - 1)/__WORD_BIT;
	start = iterator(q, 0);
      }
    }

#ifdef __STL_MEMBER_TEMPLATES
    template <class InputIterator>
    void initialize_range(InputIterator first, InputIterator last,
                          input_iterator_tag) {
      start = iterator();
      finish = iterator();
      end_of_storage = 0;
      for ( ; first != last; ++first) 
        push_back(*first);
    }

    template <class ForwardIterator>
    void initialize_range(ForwardIterator first, ForwardIterator last,
                          forward_iterator_tag) {
      size_type n = 0;
      distance(first, last, n);
      initialize(n);
      copy(first, last, start);
    }

    template <class InputIterator>
    void insert_range(iterator pos,
                      InputIterator first, InputIterator last,
                      input_iterator_tag) {
      for ( ; first != last; ++first) {
        pos = insert(pos, *first);
        ++pos;
      }
    }

    template <class ForwardIterator>
    void insert_range(iterator position,
                      ForwardIterator first, ForwardIterator last,
                      forward_iterator_tag) {
      if (first != last) {
        size_type n = 0;
        distance(first, last, n);
        if (capacity() - size() >= n) {
          copy_backward(position, end(), finish + n);
          copy(first, last, position);
          finish += n;
        }
        else {
          size_type len = size() + max(size(), n);
          unsigned int* q = bit_alloc(len);
          iterator i = copy(begin(), position, iterator(q, 0));
          i = copy(first, last, i);
          finish = copy(position, end(), i);
          deallocate();
          end_of_storage = q + (len + __WORD_BIT - 1)/__WORD_BIT;
          start = iterator(q, 0);
        }
      }
    }      

#endif /* __STL_MEMBER_TEMPLATES */

    typedef bit_vector self;
public:
    iterator begin() { return start; }
    const_iterator begin() const { return start; }
    iterator end() { return finish; }
    const_iterator end() const { return finish; }

    reverse_iterator rbegin() { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const { 
        return const_reverse_iterator(end()); 
    }
    reverse_iterator rend() { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const { 
        return const_reverse_iterator(begin()); 
    }

    size_type size() const { return size_type(end() - begin()); }
    size_type max_size() const { return size_type(-1); }
    size_type capacity() const {
	return size_type(const_iterator(end_of_storage, 0) - begin());
    }
    bool empty() const { return begin() == end(); }
    reference operator[](size_type n) { return *(begin() + n); }
    const_reference operator[](size_type n) const { return *(begin() + n); }
    bit_vector() : start(iterator()), finish(iterator()), end_of_storage(0) {}
    bit_vector(size_type n, bool value) {
        initialize(n);
        fill(start.p, end_of_storage, value ? ~0 : 0);
    }
    bit_vector(int n, bool value) {
        initialize(n);
        fill(start.p, end_of_storage, value ? ~0 : 0);
    }
    bit_vector(long n, bool value) {
        initialize(n);
        fill(start.p, end_of_storage, value ? ~0 : 0);
    }
    explicit bit_vector(size_type n) {
        initialize(n);
        fill(start.p, end_of_storage, 0);
    }
    bit_vector(const self& x) {
	initialize(x.size());
	copy(x.begin(), x.end(), start);
    }

#ifdef __STL_MEMBER_TEMPLATES
    template <class InputIterator>
    bit_vector(InputIterator first, InputIterator last) {
        initialize_range(first, last, iterator_category(first));
    }
#else /* __STL_MEMBER_TEMPLATES */
    bit_vector(const_iterator first, const_iterator last) {
	size_type n = 0;
	distance(first, last, n);
	initialize(n);
	copy(first, last, start);
    }
    bit_vector(const bool* first, const bool* last) {
	size_type n = 0;
	distance(first, last, n);
	initialize(n);
	copy(first, last, start);
    }
#endif /* __STL_MEMBER_TEMPLATES */

    ~bit_vector() { deallocate(); }
    self& operator=(const self& x) {
	if (&x == this) return *this;
	if (x.size() > capacity()) {
	    deallocate();
	    initialize(x.size());
	}
	copy(x.begin(), x.end(), begin());
	finish = begin() + x.size();
	return *this;
    }
    void reserve(size_type n) {
	if (capacity() < n) {
	    unsigned int* q = bit_alloc(n);
	    finish = copy(begin(), end(), iterator(q, 0));
	    deallocate();
	    start = iterator(q, 0);
	    end_of_storage = q + (n + __WORD_BIT - 1)/__WORD_BIT;
	}
    }
    reference front() { return *begin(); }
    const_reference front() const { return *begin(); }
    reference back() { return *(end() - 1); }
    const_reference back() const { return *(end() - 1); }
    void push_back(bool x) {
	if (finish.p != end_of_storage)
	    *finish++ = x;
	else
	    insert_aux(end(), x);
    }
    void swap(bit_vector& x) {
	::swap(start, x.start);
	::swap(finish, x.finish);
	::swap(end_of_storage, x.end_of_storage);
    }
    iterator insert(iterator position, bool x = bool()) {
	size_type n = position - begin();
	if (finish.p != end_of_storage && position == end())
	    *finish++ = x;
	else
	    insert_aux(position, x);
	return begin() + n;
    }

#ifdef __STL_MEMBER_TEMPLATES
    template <class InputIterator> void insert(iterator position,
                                               InputIterator first,
                                               InputIterator last) {
      insert_range(position, first, last, iterator_category(first));
    }
#else /* __STL_MEMBER_TEMPLATES */
    void insert(iterator position, const_iterator first, 
		const_iterator last) {
      if (first == last) return;
      size_type n = 0;
      distance(first, last, n);
      if (capacity() - size() >= n) {
	copy_backward(position, end(), finish + n);
	copy(first, last, position);
	finish += n;
      } else {
	size_type len = size() + max(size(), n);
	unsigned int* q = bit_alloc(len);
	iterator i = copy(begin(), position, iterator(q, 0));
	i = copy(first, last, i);
	finish = copy(position, end(), i);
	deallocate();
	end_of_storage = q + (len + __WORD_BIT - 1)/__WORD_BIT;
	start = iterator(q, 0);
      }
    }

    void insert(iterator position, const bool* first, const bool* last) {
      if (first == last) return;
      size_type n = 0;
      distance(first, last, n);
      if (capacity() - size() >= n) {
	copy_backward(position, end(), finish + n);
	copy(first, last, position);
	finish += n;
      } else {
	size_type len = size() + max(size(), n);
	unsigned int* q = bit_alloc(len);
	iterator i = copy(begin(), position, iterator(q, 0));
	i = copy(first, last, i);
	finish = copy(position, end(), i);
	deallocate();
	end_of_storage = q + (len + __WORD_BIT - 1)/__WORD_BIT;
	start = iterator(q, 0);
      }
    }
#endif /* __STL_MEMBER_TEMPLATES */
  
    void insert(iterator position, size_type n, bool x) {
      if (n == 0) return;
      if (capacity() - size() >= n) {
	copy_backward(position, end(), finish + n);
	fill(position, position + n, x);
	finish += n;
      } else {
	size_type len = size() + max(size(), n);
	unsigned int* q = bit_alloc(len);
	iterator i = copy(begin(), position, iterator(q, 0));
	fill_n(i, n, x);
	finish = copy(position, end(), i + n);
	deallocate();
	end_of_storage = q + (len + __WORD_BIT - 1)/__WORD_BIT;
	start = iterator(q, 0);
      }
    }

    void insert(iterator pos, int n, bool x)  { insert(pos, (size_type)n, x); }
    void insert(iterator pos, long n, bool x) { insert(pos, (size_type)n, x); }

    void pop_back() { --finish; }
    void erase(iterator position) {
	if (position + 1 != end())
	    copy(position + 1, end(), position);
	--finish;
    }
    void erase(iterator first, iterator last) {
	finish = copy(last, end(), first);
    }
    void resize(size_type new_size, bool x = bool()) {
      if (new_size < size()) 
        erase(begin() + new_size, end());
      else
        insert(end(), new_size - size(), x);
    }
    void clear() { erase(begin(), end()); }
};

inline bool operator==(const bit_vector& x, const bit_vector& y) {
    return x.size() == y.size() && equal(x.begin(), x.end(), y.begin());
}

inline bool operator<(const bit_vector& x, const bit_vector& y) {
    return lexicographical_compare(x.begin(), x.end(), y.begin(), y.end());
}

inline void swap(bit_vector::reference x, bit_vector::reference y) {
    bool tmp = x;
    x = y;
    y = tmp;
}

#undef __WORD_BIT

#endif /* __SGI_STL_BVECTOR_H */

// Internal policy header for TR1 unordered_set and unordered_map -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/** @file 
 *  This is a TR1 C++ Library header. 
 */

#ifndef _TR1_HASHTABLE_POLICY_H
#define _TR1_HASHTABLE_POLICY_H 1

namespace std
{ 
_GLIBCXX_BEGIN_NAMESPACE(tr1)
namespace detail
{
namespace 
{
  // General utilities.
  template<bool Flag, typename IfTrue, typename IfFalse>
    struct IF;

  template<typename IfTrue, typename IfFalse>
    struct IF<true, IfTrue, IfFalse>
    { typedef IfTrue type; };
 
  template <typename IfTrue, typename IfFalse>
    struct IF<false, IfTrue, IfFalse>
    { typedef IfFalse type; };

  // Helper function: return distance(first, last) for forward
  // iterators, or 0 for input iterators.
  template<class Iterator>
    inline typename std::iterator_traits<Iterator>::difference_type
    distance_fw(Iterator first, Iterator last, std::input_iterator_tag)
    { return 0; }

  template<class Iterator>
    inline typename std::iterator_traits<Iterator>::difference_type
    distance_fw(Iterator first, Iterator last, std::forward_iterator_tag)
    { return std::distance(first, last); }

  template<class Iterator>
    inline typename std::iterator_traits<Iterator>::difference_type
    distance_fw(Iterator first, Iterator last)
    {
      typedef typename std::iterator_traits<Iterator>::iterator_category tag;
      return distance_fw(first, last, tag());
    }

  // XXX This is a hack.  prime_rehash_policy's member functions, and
  // certainly the list of primes, should be defined in a .cc file.
  // We're temporarily putting them in a header because we don't have a
  // place to put TR1 .cc files yet.  There's no good reason for any of
  // prime_rehash_policy's member functions to be inline, and there's
  // certainly no good reason for X<> to exist at all.  
  struct lt
  {
    template<typename X, typename Y>
      bool
      operator()(X x, Y y)
      { return x < y; }
  };

  template<int ulongsize = sizeof(unsigned long)>
    struct X
    {
      static const int n_primes = ulongsize != 8 ? 256 : 256 + 48;
      static const unsigned long primes[256 + 48 + 1];
    };

  template<int ulongsize>
    const int X<ulongsize>::n_primes;

  template<int ulongsize>
    const unsigned long X<ulongsize>::primes[256 + 48 + 1] =
    {
      2ul, 3ul, 5ul, 7ul, 11ul, 13ul, 17ul, 19ul, 23ul, 29ul, 31ul,
      37ul, 41ul, 43ul, 47ul, 53ul, 59ul, 61ul, 67ul, 71ul, 73ul, 79ul,
      83ul, 89ul, 97ul, 103ul, 109ul, 113ul, 127ul, 137ul, 139ul, 149ul,
      157ul, 167ul, 179ul, 193ul, 199ul, 211ul, 227ul, 241ul, 257ul,
      277ul, 293ul, 313ul, 337ul, 359ul, 383ul, 409ul, 439ul, 467ul,
      503ul, 541ul, 577ul, 619ul, 661ul, 709ul, 761ul, 823ul, 887ul,
      953ul, 1031ul, 1109ul, 1193ul, 1289ul, 1381ul, 1493ul, 1613ul,
      1741ul, 1879ul, 2029ul, 2179ul, 2357ul, 2549ul, 2753ul, 2971ul,
      3209ul, 3469ul, 3739ul, 4027ul, 4349ul, 4703ul, 5087ul, 5503ul,
      5953ul, 6427ul, 6949ul, 7517ul, 8123ul, 8783ul, 9497ul, 10273ul,
      11113ul, 12011ul, 12983ul, 14033ul, 15173ul, 16411ul, 17749ul,
      19183ul, 20753ul, 22447ul, 24281ul, 26267ul, 28411ul, 30727ul,
      33223ul, 35933ul, 38873ul, 42043ul, 45481ul, 49201ul, 53201ul,
      57557ul, 62233ul, 67307ul, 72817ul, 78779ul, 85229ul, 92203ul,
      99733ul, 107897ul, 116731ul, 126271ul, 136607ul, 147793ul,
      159871ul, 172933ul, 187091ul, 202409ul, 218971ul, 236897ul,
      256279ul, 277261ul, 299951ul, 324503ul, 351061ul, 379787ul,
      410857ul, 444487ul, 480881ul, 520241ul, 562841ul, 608903ul,
      658753ul, 712697ul, 771049ul, 834181ul, 902483ul, 976369ul,
      1056323ul, 1142821ul, 1236397ul, 1337629ul, 1447153ul, 1565659ul,
      1693859ul, 1832561ul, 1982627ul, 2144977ul, 2320627ul, 2510653ul,
      2716249ul, 2938679ul, 3179303ul, 3439651ul, 3721303ul, 4026031ul,
      4355707ul, 4712381ul, 5098259ul, 5515729ul, 5967347ul, 6456007ul,
      6984629ul, 7556579ul, 8175383ul, 8844859ul, 9569143ul, 10352717ul,
      11200489ul, 12117689ul, 13109983ul, 14183539ul, 15345007ul,
      16601593ul, 17961079ul, 19431899ul, 21023161ul, 22744717ul,
      24607243ul, 26622317ul, 28802401ul, 31160981ul, 33712729ul,
      36473443ul, 39460231ul, 42691603ul, 46187573ul, 49969847ul,
      54061849ul, 58488943ul, 63278561ul, 68460391ul, 74066549ul,
      80131819ul, 86693767ul, 93793069ul, 101473717ul, 109783337ul,
      118773397ul, 128499677ul, 139022417ul, 150406843ul, 162723577ul,
      176048909ul, 190465427ul, 206062531ul, 222936881ul, 241193053ul,
      260944219ul, 282312799ul, 305431229ul, 330442829ul, 357502601ul,
      386778277ul, 418451333ul, 452718089ul, 489790921ul, 529899637ul,
      573292817ul, 620239453ul, 671030513ul, 725980837ul, 785430967ul,
      849749479ul, 919334987ul, 994618837ul, 1076067617ul, 1164186217ul,
      1259520799ul, 1362662261ul, 1474249943ul, 1594975441ul,
      1725587117ul, 1866894511ul, 2019773507ul, 2185171673ul,
      2364114217ul, 2557710269ul, 2767159799ul, 2993761039ul,
      3238918481ul, 3504151727ul, 3791104843ul, 4101556399ul,
      4294967291ul,
      // Sentinel, so we don't have to test the result of lower_bound,
      // or, on 64-bit machines, rest of the table.
      ulongsize != 8 ? 4294967291ul : (unsigned long)6442450933ull,
      (unsigned long)8589934583ull,
      (unsigned long)12884901857ull, (unsigned long)17179869143ull,
      (unsigned long)25769803693ull, (unsigned long)34359738337ull,
      (unsigned long)51539607367ull, (unsigned long)68719476731ull,
      (unsigned long)103079215087ull, (unsigned long)137438953447ull,
      (unsigned long)206158430123ull, (unsigned long)274877906899ull,
      (unsigned long)412316860387ull, (unsigned long)549755813881ull,
      (unsigned long)824633720731ull, (unsigned long)1099511627689ull,
      (unsigned long)1649267441579ull, (unsigned long)2199023255531ull,
      (unsigned long)3298534883309ull, (unsigned long)4398046511093ull,
      (unsigned long)6597069766607ull, (unsigned long)8796093022151ull,
      (unsigned long)13194139533241ull, (unsigned long)17592186044399ull,
      (unsigned long)26388279066581ull, (unsigned long)35184372088777ull,
      (unsigned long)52776558133177ull, (unsigned long)70368744177643ull,
      (unsigned long)105553116266399ull, (unsigned long)140737488355213ull,
      (unsigned long)211106232532861ull, (unsigned long)281474976710597ull,
      (unsigned long)562949953421231ull, (unsigned long)1125899906842597ull,
      (unsigned long)2251799813685119ull, (unsigned long)4503599627370449ull,
      (unsigned long)9007199254740881ull, (unsigned long)18014398509481951ull,
      (unsigned long)36028797018963913ull, (unsigned long)72057594037927931ull,
      (unsigned long)144115188075855859ull,
      (unsigned long)288230376151711717ull,
      (unsigned long)576460752303423433ull,
      (unsigned long)1152921504606846883ull,
      (unsigned long)2305843009213693951ull,
      (unsigned long)4611686018427387847ull,
      (unsigned long)9223372036854775783ull,
      (unsigned long)18446744073709551557ull,
      (unsigned long)18446744073709551557ull
    };
} // anonymous namespace

  // Auxiliary types used for all instantiations of hashtable: nodes
  // and iterators.
  
  // Nodes, used to wrap elements stored in the hash table.  A policy
  // template parameter of class template hashtable controls whether
  // nodes also store a hash code. In some cases (e.g. strings) this
  // may be a performance win.
  template<typename Value, bool cache_hash_code>
    struct hash_node;

  template<typename Value>
    struct hash_node<Value, true>
    {
      Value       m_v;
      std::size_t hash_code;
      hash_node*  m_next;
    };

  template<typename Value>
    struct hash_node<Value, false>
    {
      Value       m_v;
      hash_node*  m_next;
    };

  // Local iterators, used to iterate within a bucket but not between
  // buckets.
  template<typename Value, bool cache>
    struct node_iterator_base
    {
      node_iterator_base(hash_node<Value, cache>* p)
      : m_cur(p) { }
      
      void
      incr()
      { m_cur = m_cur->m_next; }

      hash_node<Value, cache>* m_cur;
    };

  template<typename Value, bool cache>
    inline bool
    operator==(const node_iterator_base<Value, cache>& x,
	       const node_iterator_base<Value, cache>& y)
    { return x.m_cur == y.m_cur; }

  template<typename Value, bool cache>
    inline bool
    operator!=(const node_iterator_base<Value, cache>& x,
	       const node_iterator_base<Value, cache>& y)
    { return x.m_cur != y.m_cur; }

  template<typename Value, bool constant_iterators, bool cache>
    struct node_iterator
    : public node_iterator_base<Value, cache>
    {
      typedef Value                                    value_type;
      typedef typename IF<constant_iterators, const Value*, Value*>::type
                                                       pointer;
      typedef typename IF<constant_iterators, const Value&, Value&>::type
                                                       reference;
      typedef std::ptrdiff_t                           difference_type;
      typedef std::forward_iterator_tag                iterator_category;

      node_iterator()
      : node_iterator_base<Value, cache>(0) { }

      explicit
      node_iterator(hash_node<Value, cache>* p)
      : node_iterator_base<Value, cache>(p) { }

      reference
      operator*() const
      { return this->m_cur->m_v; }
  
      pointer
      operator->() const
      { return &this->m_cur->m_v; }

      node_iterator&
      operator++()
      { 
	this->incr(); 
	return *this; 
      }
  
      node_iterator
      operator++(int)
      { 
	node_iterator tmp(*this);
	this->incr();
	return tmp;
      }
    };

  template<typename Value, bool constant_iterators, bool cache>
    struct node_const_iterator
    : public node_iterator_base<Value, cache>
    {
      typedef Value                                    value_type;
      typedef const Value*                             pointer;
      typedef const Value&                             reference;
      typedef std::ptrdiff_t                           difference_type;
      typedef std::forward_iterator_tag                iterator_category;

      node_const_iterator()
      : node_iterator_base<Value, cache>(0) { }

      explicit
      node_const_iterator(hash_node<Value, cache>* p)
      : node_iterator_base<Value, cache>(p) { }

      node_const_iterator(const node_iterator<Value, constant_iterators,
			  cache>& x)
      : node_iterator_base<Value, cache>(x.m_cur) { }

      reference
      operator*() const
      { return this->m_cur->m_v; }
  
      pointer
      operator->() const
      { return &this->m_cur->m_v; }

      node_const_iterator&
      operator++()
      { 
	this->incr(); 
	return *this; 
      }
  
      node_const_iterator
      operator++(int)
      { 
	node_const_iterator tmp(*this);
	this->incr();
	return tmp;
      }
    };

  template<typename Value, bool cache>
    struct hashtable_iterator_base
    {
      hashtable_iterator_base(hash_node<Value, cache>* node,
			      hash_node<Value, cache>** bucket)
      : m_cur_node(node), m_cur_bucket(bucket) { }

      void
      incr()
      {
	m_cur_node = m_cur_node->m_next;
	if (!m_cur_node)
	  m_incr_bucket();
      }

      void
      m_incr_bucket();

      hash_node<Value, cache>*  m_cur_node;
      hash_node<Value, cache>** m_cur_bucket;
    };

  // Global iterators, used for arbitrary iteration within a hash
  // table.  Larger and more expensive than local iterators.
  template<typename Value, bool cache>
    void
    hashtable_iterator_base<Value, cache>::
    m_incr_bucket()
    {
      ++m_cur_bucket;

      // This loop requires the bucket array to have a non-null sentinel.
      while (!*m_cur_bucket)
	++m_cur_bucket;
      m_cur_node = *m_cur_bucket;
    }

  template<typename Value, bool cache>
    inline bool
    operator==(const hashtable_iterator_base<Value, cache>& x,
	       const hashtable_iterator_base<Value, cache>& y)
    { return x.m_cur_node == y.m_cur_node; }

  template<typename Value, bool cache>
    inline bool
    operator!=(const hashtable_iterator_base<Value, cache>& x,
	       const hashtable_iterator_base<Value, cache>& y)
    { return x.m_cur_node != y.m_cur_node; }

  template<typename Value, bool constant_iterators, bool cache>
    struct hashtable_iterator
    : public hashtable_iterator_base<Value, cache>
    {
      typedef Value                                    value_type;
      typedef typename IF<constant_iterators, const Value*, Value*>::type
                                                       pointer;
      typedef typename IF<constant_iterators, const Value&, Value&>::type
                                                       reference;
      typedef std::ptrdiff_t                           difference_type;
      typedef std::forward_iterator_tag                iterator_category;

      hashtable_iterator()
      : hashtable_iterator_base<Value, cache>(0, 0) { }

      hashtable_iterator(hash_node<Value, cache>* p,
			 hash_node<Value, cache>** b)
      : hashtable_iterator_base<Value, cache>(p, b) { }

      explicit
      hashtable_iterator(hash_node<Value, cache>** b)
      : hashtable_iterator_base<Value, cache>(*b, b) { }

      reference
      operator*() const
      { return this->m_cur_node->m_v; }
  
      pointer
      operator->() const
      { return &this->m_cur_node->m_v; }

      hashtable_iterator&
      operator++()
      { 
	this->incr();
	return *this;
      }
  
      hashtable_iterator
      operator++(int)
      { 
	hashtable_iterator tmp(*this);
	this->incr();
	return tmp;
      }
    };

  template<typename Value, bool constant_iterators, bool cache>
    struct hashtable_const_iterator
    : public hashtable_iterator_base<Value, cache>
    {
      typedef Value                                    value_type;
      typedef const Value*                             pointer;
      typedef const Value&                             reference;
      typedef std::ptrdiff_t                           difference_type;
      typedef std::forward_iterator_tag                iterator_category;

      hashtable_const_iterator()
      : hashtable_iterator_base<Value, cache>(0, 0) { }

      hashtable_const_iterator(hash_node<Value, cache>* p,
			       hash_node<Value, cache>** b)
      : hashtable_iterator_base<Value, cache>(p, b) { }

      explicit
      hashtable_const_iterator(hash_node<Value, cache>** b)
      : hashtable_iterator_base<Value, cache>(*b, b) { }

      hashtable_const_iterator(const hashtable_iterator<Value,
			       constant_iterators, cache>& x)
      : hashtable_iterator_base<Value, cache>(x.m_cur_node, x.m_cur_bucket) { }

      reference
      operator*() const
      { return this->m_cur_node->m_v; }
  
      pointer
      operator->() const
      { return &this->m_cur_node->m_v; }

      hashtable_const_iterator&
      operator++()
      { 
	this->incr();
	return *this;
      }
  
      hashtable_const_iterator
      operator++(int)
      { 
	hashtable_const_iterator tmp(*this);
	this->incr();
	return tmp;
      }
    };


  // Many of class template hashtable's template parameters are policy
  // classes.  These are defaults for the policies.

  // The two key extraction policies used by the *set and *map variants.
  // XXX pb_ds::type_to_type
  template<typename T>
    struct identity
    {
      const T&
      operator()(const T& t) const
      { return t; }
    };

  // XXX use std::_Select1st?
  template<typename Pair>
    struct extract1st
    {
      const typename Pair::first_type&
      operator()(const Pair& p) const
      { return p.first; }
    };

  // Default range hashing function: use division to fold a large number
  // into the range [0, N).
  struct mod_range_hashing
  {
    typedef std::size_t first_argument_type;
    typedef std::size_t second_argument_type;
    typedef std::size_t result_type;

    result_type
    operator()(first_argument_type r, second_argument_type N) const
    { return r % N; }
  };

  // Default ranged hash function H.  In principle it should be a
  // function object composed from objects of type H1 and H2 such that
  // h(k, N) = h2(h1(k), N), but that would mean making extra copies of
  // h1 and h2.  So instead we'll just use a tag to tell class template
  // hashtable to do that composition.
  struct default_ranged_hash { };

  // Default value for rehash policy.  Bucket size is (usually) the
  // smallest prime that keeps the load factor small enough.
  struct prime_rehash_policy
  {
    prime_rehash_policy(float z = 1.0);
    
    float
    max_load_factor() const;

    // Return a bucket size no smaller than n.
    std::size_t
    next_bkt(std::size_t n) const;
    
    // Return a bucket count appropriate for n elements
    std::size_t
    bkt_for_elements(std::size_t n) const;
    
    // n_bkt is current bucket count, n_elt is current element count,
    // and n_ins is number of elements to be inserted.  Do we need to
    // increase bucket count?  If so, return make_pair(true, n), where n
    // is the new bucket count.  If not, return make_pair(false, 0).
    std::pair<bool, std::size_t>
    need_rehash(std::size_t n_bkt, std::size_t n_elt, std::size_t n_ins) const;
    
    float               m_max_load_factor;
    float               m_growth_factor;
    mutable std::size_t m_next_resize;
  };

  inline
  prime_rehash_policy::
  prime_rehash_policy(float z)
  : m_max_load_factor(z), m_growth_factor(2.f), m_next_resize(0)
  { }

  inline float
  prime_rehash_policy::
  max_load_factor() const
  { return m_max_load_factor; }

  // Return a prime no smaller than n.
  inline std::size_t
  prime_rehash_policy::
  next_bkt(std::size_t n) const
  {
    const unsigned long* const last = X<>::primes + X<>::n_primes;
    const unsigned long* p = std::lower_bound(X<>::primes, last, n);
    m_next_resize = static_cast<std::size_t>(std::ceil(*p * m_max_load_factor));
    return *p;
  }

  // Return the smallest prime p such that alpha p >= n, where alpha
  // is the load factor.
  inline std::size_t
  prime_rehash_policy::
  bkt_for_elements(std::size_t n) const
  {
    const unsigned long* const last = X<>::primes + X<>::n_primes;
    const float min_bkts = n / m_max_load_factor;
    const unsigned long* p = std::lower_bound(X<>::primes, last,
					      min_bkts, lt());
    m_next_resize = static_cast<std::size_t>(std::ceil(*p * m_max_load_factor));
    return *p;
  }

  // Finds the smallest prime p such that alpha p > n_elt + n_ins.
  // If p > n_bkt, return make_pair(true, p); otherwise return
  // make_pair(false, 0).  In principle this isn't very different from 
  // bkt_for_elements.
  
  // The only tricky part is that we're caching the element count at
  // which we need to rehash, so we don't have to do a floating-point
  // multiply for every insertion.
  
  inline std::pair<bool, std::size_t>
  prime_rehash_policy::
  need_rehash(std::size_t n_bkt, std::size_t n_elt, std::size_t n_ins) const
  {
    if (n_elt + n_ins > m_next_resize)
      {
	float min_bkts = (float(n_ins) + float(n_elt)) / m_max_load_factor;
	if (min_bkts > n_bkt)
	  {
	    min_bkts = std::max(min_bkts, m_growth_factor * n_bkt);
	    const unsigned long* const last = X<>::primes + X<>::n_primes;
	    const unsigned long* p = std::lower_bound(X<>::primes, last,
						      min_bkts, lt());
	    m_next_resize = 
	      static_cast<std::size_t>(std::ceil(*p * m_max_load_factor));
	    return std::make_pair(true, *p);
	  }
	else 
	  {
	    m_next_resize = 
	      static_cast<std::size_t>(std::ceil(n_bkt * m_max_load_factor));
	    return std::make_pair(false, 0);
	  }
      }
    else
      return std::make_pair(false, 0);
  }

  // Base classes for std::tr1::hashtable.  We define these base
  // classes because in some cases we want to do different things
  // depending on the value of a policy class.  In some cases the
  // policy class affects which member functions and nested typedefs
  // are defined; we handle that by specializing base class templates.
  // Several of the base class templates need to access other members
  // of class template hashtable, so we use the "curiously recurring
  // template pattern" for them.

  // class template map_base.  If the hashtable has a value type of the
  // form pair<T1, T2> and a key extraction policy that returns the
  // first part of the pair, the hashtable gets a mapped_type typedef.
  // If it satisfies those criteria and also has unique keys, then it
  // also gets an operator[].  
  template<typename K, typename V, typename Ex, bool unique, typename Hashtable>
    struct map_base { };
	  
  template<typename K, typename Pair, typename Hashtable>
    struct map_base<K, Pair, extract1st<Pair>, false, Hashtable>
    {
      typedef typename Pair::second_type mapped_type;
    };

  template<typename K, typename Pair, typename Hashtable>
    struct map_base<K, Pair, extract1st<Pair>, true, Hashtable>
    {
      typedef typename Pair::second_type mapped_type;
      
      mapped_type&
      operator[](const K& k);
    };

  template<typename K, typename Pair, typename Hashtable>
    typename map_base<K, Pair, extract1st<Pair>, true, Hashtable>::mapped_type&
    map_base<K, Pair, extract1st<Pair>, true, Hashtable>::
    operator[](const K& k)
    {
      Hashtable* h = static_cast<Hashtable*>(this);
      typename Hashtable::hash_code_t code = h->m_hash_code(k);
      std::size_t n = h->bucket_index(k, code, h->bucket_count());

      typename Hashtable::node* p = h->m_find_node(h->m_buckets[n], k, code);
      if (!p)
	return h->m_insert_bucket(std::make_pair(k, mapped_type()),
				  n, code)->second;
      return (p->m_v).second;
    }

  // class template rehash_base.  Give hashtable the max_load_factor
  // functions iff the rehash policy is prime_rehash_policy.
  template<typename RehashPolicy, typename Hashtable>
    struct rehash_base { };

  template<typename Hashtable>
    struct rehash_base<prime_rehash_policy, Hashtable>
    {
      float
      max_load_factor() const
      {
	const Hashtable* This = static_cast<const Hashtable*>(this);
	return This->rehash_policy().max_load_factor();
      }

      void
      max_load_factor(float z)
      {
	Hashtable* This = static_cast<Hashtable*>(this);
	This->rehash_policy(prime_rehash_policy(z));    
      }
    };

  // Class template hash_code_base.  Encapsulates two policy issues that
  // aren't quite orthogonal.
  //   (1) the difference between using a ranged hash function and using
  //       the combination of a hash function and a range-hashing function.
  //       In the former case we don't have such things as hash codes, so
  //       we have a dummy type as placeholder.
  //   (2) Whether or not we cache hash codes.  Caching hash codes is
  //       meaningless if we have a ranged hash function.
  // We also put the key extraction and equality comparison function 
  // objects here, for convenience.
  
  // Primary template: unused except as a hook for specializations.  
  template<typename Key, typename Value,
	   typename ExtractKey, typename Equal,
	   typename H1, typename H2, typename H,
	   bool cache_hash_code>
    struct hash_code_base;

  // Specialization: ranged hash function, no caching hash codes.  H1
  // and H2 are provided but ignored.  We define a dummy hash code type.
  template<typename Key, typename Value,
	   typename ExtractKey, typename Equal,
	   typename H1, typename H2, typename H>
    struct hash_code_base<Key, Value, ExtractKey, Equal, H1, H2, H, false>
    {
    protected:
      hash_code_base(const ExtractKey& ex, const Equal& eq,
		     const H1&, const H2&, const H& h)
      : m_extract(ex), m_eq(eq), m_ranged_hash(h) { }

      typedef void* hash_code_t;
  
      hash_code_t
      m_hash_code(const Key& k) const
      { return 0; }
  
      std::size_t
      bucket_index(const Key& k, hash_code_t, std::size_t N) const
      { return m_ranged_hash(k, N); }

      std::size_t
      bucket_index(const hash_node<Value, false>* p, std::size_t N) const
      { return m_ranged_hash(m_extract(p->m_v), N); }
  
      bool
      compare(const Key& k, hash_code_t, hash_node<Value, false>* n) const
      { return m_eq(k, m_extract(n->m_v)); }

      void
      store_code(hash_node<Value, false>*, hash_code_t) const
      { }

      void
      copy_code(hash_node<Value, false>*, const hash_node<Value, false>*) const
      { }
      
      void
      m_swap(hash_code_base& x)
      {
	std::swap(m_extract, x.m_extract);
	std::swap(m_eq, x.m_eq);
	std::swap(m_ranged_hash, x.m_ranged_hash);
      }

    protected:
      ExtractKey m_extract;
      Equal      m_eq;
      H          m_ranged_hash;
    };


  // No specialization for ranged hash function while caching hash codes.
  // That combination is meaningless, and trying to do it is an error.
  
  
  // Specialization: ranged hash function, cache hash codes.  This
  // combination is meaningless, so we provide only a declaration
  // and no definition.  
  template<typename Key, typename Value,
	    typename ExtractKey, typename Equal,
	    typename H1, typename H2, typename H>
    struct hash_code_base<Key, Value, ExtractKey, Equal, H1, H2, H, true>;


  // Specialization: hash function and range-hashing function, no
  // caching of hash codes.  H is provided but ignored.  Provides
  // typedef and accessor required by TR1.  
  template<typename Key, typename Value,
	   typename ExtractKey, typename Equal,
	   typename H1, typename H2>
    struct hash_code_base<Key, Value, ExtractKey, Equal, H1, H2,
			  default_ranged_hash, false>
    {
      typedef H1 hasher;
      
      hasher
      hash_function() const
      { return m_h1; }

    protected:
      hash_code_base(const ExtractKey& ex, const Equal& eq,
		     const H1& h1, const H2& h2, const default_ranged_hash&)
      : m_extract(ex), m_eq(eq), m_h1(h1), m_h2(h2) { }

      typedef std::size_t hash_code_t;
      
      hash_code_t
      m_hash_code(const Key& k) const
      { return m_h1(k); }
      
      std::size_t
      bucket_index(const Key&, hash_code_t c, std::size_t N) const
      { return m_h2(c, N); }

      std::size_t
      bucket_index(const hash_node<Value, false>* p, std::size_t N) const
      { return m_h2(m_h1(m_extract(p->m_v)), N); }

      bool
      compare(const Key& k, hash_code_t, hash_node<Value, false>* n) const
      { return m_eq(k, m_extract(n->m_v)); }

      void
      store_code(hash_node<Value, false>*, hash_code_t) const
      { }

      void
      copy_code(hash_node<Value, false>*, const hash_node<Value, false>*) const
      { }

      void
      m_swap(hash_code_base& x)
      {
	std::swap(m_extract, x.m_extract);
	std::swap(m_eq, x.m_eq);
	std::swap(m_h1, x.m_h1);
	std::swap(m_h2, x.m_h2);
      }

    protected:
      ExtractKey m_extract;
      Equal      m_eq;
      H1         m_h1;
      H2         m_h2;
    };

  // Specialization: hash function and range-hashing function, 
  // caching hash codes.  H is provided but ignored.  Provides
  // typedef and accessor required by TR1.
  template<typename Key, typename Value,
	   typename ExtractKey, typename Equal,
	   typename H1, typename H2>
    struct hash_code_base<Key, Value, ExtractKey, Equal, H1, H2,
			  default_ranged_hash, true>
    {
      typedef H1 hasher;
      
      hasher
      hash_function() const
      { return m_h1; }

    protected:
      hash_code_base(const ExtractKey& ex, const Equal& eq,
		     const H1& h1, const H2& h2, const default_ranged_hash&)
      : m_extract(ex), m_eq(eq), m_h1(h1), m_h2(h2) { }

      typedef std::size_t hash_code_t;
  
      hash_code_t
      m_hash_code(const Key& k) const
      { return m_h1(k); }
  
      std::size_t
      bucket_index(const Key&, hash_code_t c, std::size_t N) const
      { return m_h2(c, N); }

      std::size_t
      bucket_index(const hash_node<Value, true>* p, std::size_t N) const
      { return m_h2(p->hash_code, N); }

      bool
      compare(const Key& k, hash_code_t c, hash_node<Value, true>* n) const
      { return c == n->hash_code && m_eq(k, m_extract(n->m_v)); }

      void
      store_code(hash_node<Value, true>* n, hash_code_t c) const
      { n->hash_code = c; }

      void
      copy_code(hash_node<Value, true>* to,
		const hash_node<Value, true>* from) const
      { to->hash_code = from->hash_code; }

      void
      m_swap(hash_code_base& x)
      {
	std::swap(m_extract, x.m_extract);
	std::swap(m_eq, x.m_eq);
	std::swap(m_h1, x.m_h1);
	std::swap(m_h2, x.m_h2);
      }
      
    protected:
      ExtractKey m_extract;
      Equal      m_eq;
      H1         m_h1;
      H2         m_h2;
    };
} // namespace detail
_GLIBCXX_END_NAMESPACE
} // namespace std::tr1

#endif // _TR1_HASHTABLE_POLICY_H


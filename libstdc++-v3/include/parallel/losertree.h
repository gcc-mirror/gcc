// -*- C++ -*-

// Copyright (C) 2007, 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

/** @file parallel/losertree.h
*  @brief Many generic loser tree variants.
*  This file is a GNU parallel extension to the Standard C++ Library.
*/

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_LOSERTREE_H
#define _GLIBCXX_PARALLEL_LOSERTREE_H 1

#include <functional>

#include <bits/stl_algobase.h>
#include <parallel/features.h>
#include <parallel/base.h>

namespace __gnu_parallel
{

#if _GLIBCXX_LOSER_TREE_EXPLICIT

/** @brief Guarded loser tree, copying the whole element into the
* tree structure.
*
*  Guarding is done explicitly through two flags per element, inf
*  and sup This is a quite slow variant.
*/
template<typename T, typename Comparator = std::less<T> >
  class LoserTreeExplicit
  {
  private:
    struct Loser
    {
      // The relevant element.
      T key;

      // Is this an infimum or supremum element?
      bool inf, sup;

      // Number of the sequence the element comes from.
      int source;
    };

    unsigned int size, offset;
    Loser* losers;
    Comparator comp;

  public:
    LoserTreeExplicit(unsigned int _size, Comparator _comp = std::less<T>())
    : comp(_comp)
    {
      size = _size;
      offset = size;
      losers = new Loser[size];
      for (unsigned int l = 0; l < size; ++l)
        {
          //losers[l].key = ... 	stays unset
          losers[l].inf = true;
          losers[l].sup = false;
          //losers[l].source = -1;	//sentinel
        }
    }

    ~LoserTreeExplicit()
    { delete[] losers; }

    int
    get_min_source()
    { return losers[0].source; }

    void
    insert_start(T key, int source, bool sup)
    {
      bool inf = false;
      for (unsigned int pos = (offset + source) / 2; pos > 0; pos /= 2)
        {
          if ((!inf && !losers[pos].inf && !sup && !losers[pos].sup
               && comp(losers[pos].key, key)) || losers[pos].inf || sup)
            {
              // The other one is smaller.
              std::swap(losers[pos].key, key);
              std::swap(losers[pos].inf, inf);
              std::swap(losers[pos].sup, sup);
              std::swap(losers[pos].source, source);
            }
        }

      losers[0].key = key;
      losers[0].inf = inf;
      losers[0].sup = sup;
      losers[0].source = source;
    }

    void
    init() { }

    void
    delete_min_insert(T key, bool sup)
    {
      bool inf = false;
      int source = losers[0].source;
      for (unsigned int pos = (offset + source) / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted.
          if ((!inf && !losers[pos].inf && !sup && !losers[pos].sup
              && comp(losers[pos].key, key))
              || losers[pos].inf || sup)
            {
              // The other one is smaller.
              std::swap(losers[pos].key, key);
              std::swap(losers[pos].inf, inf);
              std::swap(losers[pos].sup, sup);
              std::swap(losers[pos].source, source);
            }
        }

      losers[0].key = key;
      losers[0].inf = inf;
      losers[0].sup = sup;
      losers[0].source = source;
    }

    void
    insert_start_stable(T key, int source, bool sup)
    {
      bool inf = false;
      for (unsigned int pos = (offset + source) / 2; pos > 0; pos /= 2)
        {
          if ((!inf && !losers[pos].inf && !sup && !losers[pos].sup
	       && ((comp(losers[pos].key, key))
		   || (!comp(key, losers[pos].key)
		       && losers[pos].source < source)))
              || losers[pos].inf || sup)
            {
              // Take next key.
              std::swap(losers[pos].key, key);
              std::swap(losers[pos].inf, inf);
              std::swap(losers[pos].sup, sup);
              std::swap(losers[pos].source, source);
            }
        }

      losers[0].key = key;
      losers[0].inf = inf;
      losers[0].sup = sup;
      losers[0].source = source;
    }

    void
    init_stable() { }

    void
    delete_min_insert_stable(T key, bool sup)
    {
      bool inf = false;
      int source = losers[0].source;
      for (unsigned int pos = (offset + source) / 2; pos > 0; pos /= 2)
        {
          if ((!inf && !losers[pos].inf && !sup && !losers[pos].sup
              && ((comp(losers[pos].key, key))
		  || (!comp(key, losers[pos].key)
		      && losers[pos].source < source)))
              || losers[pos].inf || sup)
            {
              std::swap(losers[pos].key, key);
              std::swap(losers[pos].inf, inf);
              std::swap(losers[pos].sup, sup);
              std::swap(losers[pos].source, source);
            }
        }

      losers[0].key = key;
      losers[0].inf = inf;
      losers[0].sup = sup;
      losers[0].source = source;
    }
  };

#endif

#if _GLIBCXX_LOSER_TREE

/** @brief Guarded loser tree, either copying the whole element into
* the tree structure, or looking up the element via the index.
*
*  Guarding is done explicitly through one flag sup per element,
*  inf is not needed due to a better initialization routine.  This
*  is a well-performing variant.
*/
template<typename T, typename Comparator = std::less<T> >
  class LoserTree
  {
  private:
    struct Loser
    {
      bool sup;
      int source;
      T key;
    };

    unsigned int ik, k, offset;
    Loser* losers;
    Comparator comp;
    bool first_insert;

  public:
    LoserTree(unsigned int _k, Comparator _comp = std::less<T>())
    : comp(_comp)
    {
      ik = _k;

      // Next greater power of 2.
      k = 1 << (log2(ik - 1) + 1);
      offset = k;
      // Avoid default-constructing losers[].key
      losers = static_cast<Loser*>(::operator new(2 * k * sizeof(Loser)));
      for (unsigned int i = ik - 1; i < k; ++i)
        losers[i + k].sup = true;

      first_insert = true;
    }

    ~LoserTree()
    { ::operator delete(losers); }

    int
    get_min_source()
    { return losers[0].source; }

    void
    insert_start(const T& key, int source, bool sup)
    {
      unsigned int pos = k + source;

      if(first_insert)
        {
          // Construct all keys, so we can easily deconstruct them.
          for (unsigned int i = 0; i < (2 * k); ++i)
            ::new(&(losers[i].key)) T(key);
          first_insert = false;
        }
      else
        ::new(&(losers[pos].key)) T(key);

      losers[pos].sup = sup;
      losers[pos].source = source;
    }

    unsigned int
    init_winner (unsigned int root)
    {
      if (root >= k)
        {
          return root;
        }
      else
        {
          unsigned int left = init_winner (2 * root);
          unsigned int right = init_winner (2 * root + 1);
          if (losers[right].sup
	      || (!losers[left].sup
		  && !comp(losers[right].key, losers[left].key)))
            {
              // Left one is less or equal.
              losers[root] = losers[right];
              return left;
            }
          else
            {
              // Right one is less.
              losers[root] = losers[left];
              return right;
            }
        }
    }

    void
    init()
    { losers[0] = losers[init_winner(1)]; }

    // Do not pass const reference since key will be used as local variable.
    void
    delete_min_insert(T key, bool sup)
    {
      int source = losers[0].source;
      for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted.
          if (sup || (!losers[pos].sup && comp(losers[pos].key, key)))
            {
              // The other one is smaller.
              std::swap(losers[pos].sup, sup);
              std::swap(losers[pos].source, source);
              std::swap(losers[pos].key, key);
            }
        }

      losers[0].sup = sup;
      losers[0].source = source;
      losers[0].key = key;
    }

    void
    insert_start_stable(const T& key, int source, bool sup)
    { return insert_start(key, source, sup); }

    unsigned int
    init_winner_stable (unsigned int root)
    {
      if (root >= k)
        {
          return root;
        }
      else
        {
          unsigned int left = init_winner (2 * root);
          unsigned int right = init_winner (2 * root + 1);
          if (losers[right].sup
              || (!losers[left].sup
		  && !comp(losers[right].key, losers[left].key)))
            {
              // Left one is less or equal.
              losers[root] = losers[right];
              return left;
            }
          else
            {
              // Right one is less.
              losers[root] = losers[left];
              return right;
            }
        }
    }

    void
    init_stable()
    { losers[0] = losers[init_winner_stable(1)]; }

    // Do not pass const reference since key will be used as local variable.
    void
    delete_min_insert_stable(T key, bool sup)
    {
      int source = losers[0].source;
      for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted, ties are broken by source.
          if (	(sup && (!losers[pos].sup || losers[pos].source < source))
                || (!sup && !losers[pos].sup
                  && ((comp(losers[pos].key, key))
                    || (!comp(key, losers[pos].key)
                      && losers[pos].source < source))))
            {
              // The other one is smaller.
              std::swap(losers[pos].sup, sup);
              std::swap(losers[pos].source, source);
              std::swap(losers[pos].key, key);
            }
        }

      losers[0].sup = sup;
      losers[0].source = source;
      losers[0].key = key;
    }
  };

#endif

#if _GLIBCXX_LOSER_TREE_REFERENCE

/** @brief Guarded loser tree, either copying the whole element into
* the tree structure, or looking up the element via the index.
*
*  Guarding is done explicitly through one flag sup per element,
*  inf is not needed due to a better initialization routine.  This
*  is a well-performing variant.
*/
template<typename T, typename Comparator = std::less<T> >
  class LoserTreeReference
  {
#undef COPY
#ifdef COPY
#define KEY(i) losers[i].key
#define KEY_SOURCE(i) key
#else
#define KEY(i) keys[losers[i].source]
#define KEY_SOURCE(i) keys[i]
#endif
  private:
    struct Loser
    {
      bool sup;
      int source;
#ifdef COPY
      T key;
#endif
    };

    unsigned int ik, k, offset;
    Loser* losers;
#ifndef COPY
    T* keys;
#endif
    Comparator comp;

  public:
    LoserTreeReference(unsigned int _k, Comparator _comp = std::less<T>())
    : comp(_comp)
    {
      ik = _k;

      // Next greater power of 2.
      k = 1 << (log2(ik - 1) + 1);
      offset = k;
      losers = new Loser[k * 2];
#ifndef COPY
      keys = new T[ik];
#endif
      for (unsigned int i = ik - 1; i < k; ++i)
        losers[i + k].sup = true;
    }

    ~LoserTreeReference()
    {
      delete[] losers;
#ifndef COPY
      delete[] keys;
#endif
    }

    int
    get_min_source()
    { return losers[0].source; }

    void
    insert_start(T key, int source, bool sup)
    {
      unsigned int pos = k + source;

      losers[pos].sup = sup;
      losers[pos].source = source;
      KEY(pos) = key;
    }

    unsigned int
    init_winner(unsigned int root)
    {
      if (root >= k)
        {
          return root;
        }
      else
        {
          unsigned int left = init_winner (2 * root);
          unsigned int right = init_winner (2 * root + 1);
          if (	losers[right].sup ||
                (!losers[left].sup && !comp(KEY(right), KEY(left))))
            {
              // Left one is less or equal.
              losers[root] = losers[right];
              return left;
            }
          else
            {
              // Right one is less.
              losers[root] = losers[left];
              return right;
            }
        }
    }

    void
    init()
    {
      losers[0] = losers[init_winner(1)];
    }

    void
    delete_min_insert(T key, bool sup)
    {
      int source = losers[0].source;
      for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted.
          if (sup || (!losers[pos].sup && comp(KEY(pos), KEY_SOURCE(source))))
            {
              // The other one is smaller.
              std::swap(losers[pos].sup, sup);
              std::swap(losers[pos].source, source);
#ifdef COPY
              std::swap(KEY(pos), KEY_SOURCE(source));
#endif
            }
        }

      losers[0].sup = sup;
      losers[0].source = source;
#ifdef COPY
      KEY(0) = KEY_SOURCE(source);
#endif
    }

    void
    insert_start_stable(T key, int source, bool sup)
    { return insert_start(key, source, sup); }

    unsigned int
    init_winner_stable(unsigned int root)
    {
      if (root >= k)
        {
          return root;
        }
      else
        {
          unsigned int left = init_winner (2 * root);
          unsigned int right = init_winner (2 * root + 1);
          if (losers[right].sup
              || (!losers[left].sup && !comp(KEY(right), KEY(left))))
            {
              // Left one is less or equal.
              losers[root] = losers[right];
              return left;
            }
          else
            {
              // Right one is less.
              losers[root] = losers[left];
              return right;
            }
        }
    }

    void
    init_stable()
    { losers[0] = losers[init_winner_stable(1)]; }

    void
    delete_min_insert_stable(T key, bool sup)
    {
      int source = losers[0].source;
      for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted, ties are broken by source.
          if ((sup && (!losers[pos].sup || losers[pos].source < source))
	      || (!sup && !losers[pos].sup
		  && ((comp(KEY(pos), KEY_SOURCE(source)))
		      || (!comp(KEY_SOURCE(source), KEY(pos))
			  && losers[pos].source < source))))
            {
              // The other one is smaller.
              std::swap(losers[pos].sup, sup);
              std::swap(losers[pos].source, source);
#ifdef COPY
              std::swap(KEY(pos), KEY_SOURCE(source));
#endif
            }
        }

      losers[0].sup = sup;
      losers[0].source = source;
#ifdef COPY
      KEY(0) = KEY_SOURCE(source);
#endif
    }
  };
#undef KEY
#undef KEY_SOURCE

#endif

#if _GLIBCXX_LOSER_TREE_POINTER

/** @brief Guarded loser tree, either copying the whole element into
    the tree structure, or looking up the element via the index.
*  Guarding is done explicitly through one flag sup per element,
*  inf is not needed due to a better initialization routine.
*  This is a well-performing variant.
*/
template<typename T, typename Comparator = std::less<T> >
  class LoserTreePointer
  {
  private:
    struct Loser
    {
      bool sup;
      int source;
      const T* keyp;
    };

    unsigned int ik, k, offset;
    Loser* losers;
    Comparator comp;

  public:
    LoserTreePointer(unsigned int _k, Comparator _comp = std::less<T>())
      : comp(_comp)
    {
      ik = _k;

      // Next greater power of 2.
      k = 1 << (log2(ik - 1) + 1);
      offset = k;
      losers = new Loser[k * 2];
      for (unsigned int i = ik - 1; i < k; ++i)
        losers[i + k].sup = true;
    }

    ~LoserTreePointer()
    { delete[] losers; }

    int
    get_min_source()
    { return losers[0].source; }

    void
    insert_start(const T& key, int source, bool sup)
    {
      unsigned int pos = k + source;

      losers[pos].sup = sup;
      losers[pos].source = source;
      losers[pos].keyp = &key;
    }

    unsigned int
    init_winner(unsigned int root)
    {
      if (root >= k)
	return root;
      else
        {
          unsigned int left = init_winner (2 * root);
          unsigned int right = init_winner (2 * root + 1);
          if (losers[right].sup
                || (!losers[left].sup
                  && !comp(*losers[right].keyp, *losers[left].keyp)))
            {
              // Left one is less or equal.
              losers[root] = losers[right];
              return left;
            }
          else
            {
              // Right one is less.
              losers[root] = losers[left];
              return right;
            }
        }
    }

    void
    init()
    { losers[0] = losers[init_winner(1)]; }

    void
    delete_min_insert(const T& key, bool sup)
    {
      const T* keyp = &key;
      int source = losers[0].source;
      for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted.
          if (sup || (!losers[pos].sup && comp(*losers[pos].keyp, *keyp)))
            {
              // The other one is smaller.
              std::swap(losers[pos].sup, sup);
              std::swap(losers[pos].source, source);
              std::swap(losers[pos].keyp, keyp);
            }
        }

      losers[0].sup = sup;
      losers[0].source = source;
      losers[0].keyp = keyp;
    }

    void
    insert_start_stable(const T& key, int source, bool sup)
    { return insert_start(key, source, sup); }

    unsigned int
    init_winner_stable(unsigned int root)
    {
      if (root >= k)
        {
          return root;
        }
      else
        {
          unsigned int left = init_winner (2 * root);
          unsigned int right = init_winner (2 * root + 1);
          if (losers[right].sup
              || (!losers[left].sup && !comp(*losers[right].keyp,
                                            *losers[left].keyp)))
            {
              // Left one is less or equal.
              losers[root] = losers[right];
              return left;
            }
          else
            {
              // Right one is less.
              losers[root] = losers[left];
              return right;
            }
        }
    }

    void
    init_stable()
    { losers[0] = losers[init_winner_stable(1)]; }

    void
    delete_min_insert_stable(const T& key, bool sup)
    {
      const T* keyp = &key;
      int source = losers[0].source;
      for (unsigned int pos = (k + source) / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted, ties are broken by source.
          if (	(sup && (!losers[pos].sup || losers[pos].source < source))
		|| (!sup && !losers[pos].sup &&
		    ((comp(*losers[pos].keyp, *keyp))
		     || (!comp(*keyp, *losers[pos].keyp)
			 && losers[pos].source < source))))
            {
              // The other one is smaller.
              std::swap(losers[pos].sup, sup);
              std::swap(losers[pos].source, source);
              std::swap(losers[pos].keyp, keyp);
            }
        }

      losers[0].sup = sup;
      losers[0].source = source;
      losers[0].keyp = keyp;
    }
  };

#endif

#if _GLIBCXX_LOSER_TREE_UNGUARDED

/** @brief Unguarded loser tree, copying the whole element into the
* tree structure.
*
*  No guarding is done, therefore not a single input sequence must
*  run empty.  This is a very fast variant.
*/
template<typename T, typename Comparator = std::less<T> >
  class LoserTreeUnguarded
  {
  private:
    struct Loser
    {
      int source;
      T key;
    };

    unsigned int ik, k, offset;
    unsigned int* mapping;
    Loser* losers;
    Comparator comp;

    void
    map(unsigned int root, unsigned int begin, unsigned int end)
    {
      if (begin + 1 == end)
        mapping[begin] = root;
      else
        {
          // Next greater or equal power of 2.
          unsigned int left = 1 << (log2(end - begin - 1));
          map(root * 2, begin, begin + left);
          map(root * 2 + 1, begin + left, end);
        }
    }

  public:
    LoserTreeUnguarded(unsigned int _k, Comparator _comp = std::less<T>())
    : comp(_comp)
    {
      ik = _k;
      // Next greater or equal power of 2.
      k = 1 << (log2(ik - 1) + 1);
      offset = k;
      losers = new Loser[k + ik];
      mapping = new unsigned int[ik];
      map(1, 0, ik);
    }

    ~LoserTreeUnguarded()
    {
      delete[] losers;
      delete[] mapping;
    }

    int
    get_min_source()
    { return losers[0].source; }

    void
    insert_start(const T& key, int source, bool)
    {
      unsigned int pos = mapping[source];
      losers[pos].source = source;
      losers[pos].key = key;
    }

    unsigned int
    init_winner(unsigned int root, unsigned int begin, unsigned int end)
    {
      if (begin + 1 == end)
        return mapping[begin];
      else
        {
          // Next greater or equal power of 2.
          unsigned int division = 1 << (log2(end - begin - 1));
          unsigned int left = init_winner(2 * root, begin, begin + division);
          unsigned int right =
                          init_winner(2 * root + 1, begin + division, end);
          if (!comp(losers[right].key, losers[left].key))
            {
              // Left one is less or equal.
              losers[root] = losers[right];
              return left;
            }
          else
            {
              // Right one is less.
              losers[root] = losers[left];
              return right;
            }
        }
    }

    void
    init()
    { losers[0] = losers[init_winner(1, 0, ik)]; }

    // Do not pass const reference since key will be used as local variable.
    void
    delete_min_insert(const T& key, bool)
    {
      losers[0].key = key;
      T& keyr = losers[0].key;
      int& source = losers[0].source;
      for (int pos = mapping[source] / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted.
          if (comp(losers[pos].key, keyr))
            {
              // The other one is smaller.
              std::swap(losers[pos].source, source);
              std::swap(losers[pos].key, keyr);
            }
        }
    }

    void
    insert_start_stable(const T& key, int source, bool)
    { return insert_start(key, source, false); }

    void
    init_stable()
    { init(); }

    void
    delete_min_insert_stable(const T& key, bool)
    {
      losers[0].key = key;
      T& keyr = losers[0].key;
      int& source = losers[0].source;
      for (int pos = mapping[source] / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted, ties are broken by source.
          if (comp(losers[pos].key, keyr)
              || (!comp(keyr, losers[pos].key)
                && losers[pos].source < source))
            {
              // The other one is smaller.
              std::swap(losers[pos].source, source);
              std::swap(losers[pos].key, keyr);
            }
        }
    }
  };

#endif

#if _GLIBCXX_LOSER_TREE_POINTER_UNGUARDED

/** @brief Unguarded loser tree, keeping only pointers to the
* elements in the tree structure.
*
*  No guarding is done, therefore not a single input sequence must
*  run empty.  This is a very fast variant.
*/
template<typename T, typename Comparator = std::less<T> >
  class LoserTreePointerUnguarded
  {
  private:
    struct Loser
    {
      int source;
      const T* keyp;
    };

    unsigned int ik, k, offset;
    unsigned int* mapping;
    Loser* losers;
    Comparator comp;

    void map(unsigned int root, unsigned int begin, unsigned int end)
    {
      if (begin + 1 == end)
        mapping[begin] = root;
      else
        {
          // Next greater or equal power of 2.
          unsigned int left = 1 << (log2(end - begin - 1));
          map(root * 2, begin, begin + left);
          map(root * 2 + 1, begin + left, end);
        }
    }

  public:
    LoserTreePointerUnguarded(unsigned int _k,
                              Comparator _comp = std::less<T>())
    : comp(_comp)
    {
      ik = _k;

      // Next greater power of 2.
      k = 1 << (log2(ik - 1) + 1);
      offset = k;
      losers = new Loser[k + ik];
      mapping = new unsigned int[ik];
      map(1, 0, ik);
    }

    ~LoserTreePointerUnguarded()
    {
      delete[] losers;
      delete[] mapping;
    }

    int
    get_min_source()
    { return losers[0].source; }

    void
    insert_start(const T& key, int source, bool)
    {
      unsigned int pos = mapping[source];
      losers[pos].source = source;
      losers[pos].keyp = &key;
    }

    unsigned int
    init_winner(unsigned int root, unsigned int begin, unsigned int end)
    {
      if (begin + 1 == end)
        return mapping[begin];
      else
        {
          // Next greater or equal power of 2.
          unsigned int division = 1 << (log2(end - begin - 1));
          unsigned int left = init_winner(2 * root, begin, begin + division);
          unsigned int right = init_winner(2 * root + 1,
					   begin + division, end);
          if (!comp(*losers[right].keyp, *losers[left].keyp))
            {
              // Left one is less or equal.
              losers[root] = losers[right];
              return left;
            }
          else
            {
              // Right one is less.
              losers[root] = losers[left];
              return right;
            }
        }
    }

    void
    init()
    { losers[0] = losers[init_winner(1, 0, ik)]; }

    void
    delete_min_insert(const T& key, bool)
    {
      const T* keyp = &key;
      int& source = losers[0].source;
      for (int pos = mapping[source] / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted.
          if (comp(*losers[pos].keyp, *keyp))
            {
              // The other one is smaller.
              std::swap(losers[pos].source, source);
              std::swap(losers[pos].keyp, keyp);
            }
        }

      losers[0].keyp = keyp;
    }

    void
    insert_start_stable(const T& key, int source, bool)
    { return insert_start(key, source, false); }

    void
    init_stable()
    { init(); }

    void
    delete_min_insert_stable(const T& key, bool)
    {
      int& source = losers[0].source;
      const T* keyp = &key;
      for (int pos = mapping[source] / 2; pos > 0; pos /= 2)
        {
          // The smaller one gets promoted, ties are broken by source.
          if (comp(*losers[pos].keyp, *keyp)
              || (!comp(*keyp, *losers[pos].keyp)
                  && losers[pos].source < source))
            {
              // The other one is smaller.
              std::swap(losers[pos].source, source);
              std::swap(losers[pos].keyp, keyp);
            }
        }
      losers[0].keyp = keyp;
    }
  };
#endif

template<typename _ValueTp, class Comparator>
  struct loser_tree_traits
  {
#if _GLIBCXX_LOSER_TREE
    typedef LoserTree<_ValueTp, Comparator> LT;
#else
#  if _GLIBCXX_LOSER_TREE_POINTER
    typedef LoserTreePointer<_ValueTp, Comparator> LT;
#  else
#    error Must define some type in losertree.h.
#  endif
#endif
  };

template<typename _ValueTp, class Comparator>
  struct loser_tree_unguarded_traits
  {
#if _GLIBCXX_LOSER_TREE_UNGUARDED
    typedef LoserTreeUnguarded<_ValueTp, Comparator> LT;
#else
#  if _GLIBCXX_LOSER_TREE_POINTER_UNGUARDED
    typedef LoserTreePointerUnguarded<_ValueTp, Comparator> LT;
#  else
#    error Must define some unguarded type in losertree.h.
#  endif
#endif
  };

}

#endif

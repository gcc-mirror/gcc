// MT-optimized allocator -*- C++ -*-

// Copyright (C) 2003 Free Software Foundation, Inc.
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

/** @file ext/mt_allocator.h
 *  This file is a GNU extension to the Standard C++ Library.
 *  You should only include this header if you are using GCC 3 or later.
 */

#ifndef _MT_ALLOCATOR_H
#define _MT_ALLOCATOR_H 1

#include <new>
#include <memory>
#include <cstdlib>
#include <bits/functexcept.h>
#include <bits/gthr.h>
#include <bits/atomicity.h>

namespace __gnu_cxx
{
  /**
   *  This is a fixed size (power of 2) allocator which - when
   *  compiled with thread support - will maintain one freelist per
   *  size per thread plus a "global" one. Steps are taken to limit
   *  the per thread freelist sizes (by returning excess back to
   *  "global").
   *
   *  Usage examples:
   *  @code
   *    vector<int, __gnu_cxx::__mt_alloc<int> > v1;
   *
   *    typedef __gnu_cxx::__mt_alloc<char> > string_allocator;
   *    std::basic_string<char, std::char_traits<char>, string_allocator> s1;
   *  @endcode
   */
  template<typename _Tp>
    class __mt_alloc
    {
    public:
      typedef size_t     size_type;
      typedef ptrdiff_t  difference_type;
      typedef _Tp*       pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp&       reference;
      typedef const _Tp& const_reference;
      typedef _Tp        value_type;

      template<typename _Tp1>
        struct rebind
        { typedef __mt_alloc<_Tp1> other; };

      __mt_alloc() throw() 
      {
	// XXX
      }

      __mt_alloc(const __mt_alloc&) throw()
      {
	// XXX
      }

      template<typename _Tp1>
        __mt_alloc(const __mt_alloc<_Tp1>&) throw()
        {
	  // XXX
	}

      ~__mt_alloc() throw() { }

      pointer
      address(reference __x) const { return &__x; }

      const_pointer
      address(const_reference __x) const { return &__x; }

      size_type
      max_size() const throw() 
      { return size_t(-1) / sizeof(_Tp); }

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 402. wrong new expression in [some_] allocator::construct
      void 
      construct(pointer __p, const _Tp& __val) 
      { ::new(__p) _Tp(__val); }

      void 
      destroy(pointer __p) { __p->~_Tp(); }

    private:
      /*
       * We need to create the initial lists and set up some variables
       * before we can answer to the first request for memory.
       * The initialization of these variables is done at file scope
       * below class declaration.
       */
#ifdef __GTHREADS
      static __gthread_once_t _S_once_mt;
#endif
      static bool _S_initialized;

      /*
       * Using short int as type for the binmap implies we are never caching
       * blocks larger than 65535 with this allocator
       */
      typedef unsigned short int binmap_type;
      static binmap_type* _S_binmap;

      static void _S_init();

      /*
       * Variables used to "tune" the behavior of the allocator, assigned
       * and explained in detail below.
       */
      static size_t _S_max_bytes;
      static size_t _S_chunk_size;
      static size_t _S_max_threads;
      static size_t _S_no_of_bins;
      static size_t _S_freelist_headroom;

      /*
       * Each requesting thread is assigned an id ranging from 1 to
       * _S_max_threads. Thread id 0 is used as a global memory pool.
       * In order to get constant performance on the thread assignment
       * routine, we keep a list of free ids. When a thread first requests
       * memory we remove the first record in this list and stores the address
       * in a __gthread_key. When initializing the __gthread_key
       * we specify a destructor. When this destructor (i.e. the thread dies)
       * is called, we return the thread id to the back of this list.
       */
#ifdef __GTHREADS
      struct thread_record
      {
        /*
         * Points to next free thread id record. NULL if last record in list.
         */
        thread_record* next;

        /*
         * Thread id ranging from 1 to _S_max_threads.
         */
        size_t id;
      };

      static thread_record* _S_thread_freelist_first;
      static thread_record* _S_thread_freelist_last;
      static __gthread_mutex_t _S_thread_freelist_mutex;
      static void _S_thread_key_destr(void* freelist_pos);
      static __gthread_key_t _S_thread_key;
      static size_t _S_get_thread_id();
#endif

      struct block_record
      {
        /*
         * Points to the next block_record for its thread_id.
         */
        block_record* next;

        /*
         * The thread id of the thread which has requested this block.
         * All blocks are initially "owned" by global pool thread id 0.
         */
        size_t thread_id;
      };

      struct bin_record
      {
        /*
         * An "array" of pointers to the first/last free block for each
         * thread id. Memory to these "arrays" is allocated in _S_init()
         * for _S_max_threads + global pool 0.
         */
        block_record** first;
        block_record** last;

        /*
         * An "array" of counters used to keep track of the amount of blocks
         * that are on the freelist/used for each thread id.
         * Memory to these "arrays" is allocated in _S_init()
         * for _S_max_threads + global pool 0.
         */
        size_t* free;
        size_t* used;

        /*
         * Each bin has its own mutex which is used to ensure data integrity
         * while changing "ownership" on a block.
         * The mutex is initialized in _S_init().
         */
#ifdef __GTHREADS
        __gthread_mutex_t* mutex;
#endif
      };

      /*
       * An "array" of bin_records each of which represents a specific
       * power of 2 size. Memory to this "array" is allocated in _S_init().
       */
      static bin_record* _S_bin;

    public:
      pointer
      allocate(size_t __n, std::allocator<void>::const_pointer __h = 0)
      {
        /*
         * Requests larger than _S_max_bytes are handled by
         * new/delete directly
         */
        if (__n > _S_max_bytes)
          {
            void* __ret = malloc(__n * sizeof(_Tp));
            if (!__ret)
              __throw_bad_alloc();
            return static_cast<_Tp*>(__ret);
          }
	
        /*
         * Although the test in __gthread_once() would suffice, we
         * wrap test of the once condition in our own unlocked
         * check. This saves one function call to pthread_once()
         * (which itself only tests for the once value unlocked anyway
         * and immediately returns if set)
         */
        if (!_S_initialized)
          {
#ifdef __GTHREADS
            if (__gthread_active_p())
              __gthread_once(&_S_once_mt, _S_init);
            else
#endif
              {
                _S_max_threads = 0;
                _S_init();
              }
          }

        /*
         * Round up to power of 2 and figure out which bin to use
         */
        size_t bin = _S_binmap[__n];

#ifdef __GTHREADS
        size_t thread_id = _S_get_thread_id();
#else
        size_t thread_id = 0;
#endif

        block_record* block;

        /*
         * Find out if we have blocks on our freelist.
         * If so, go ahead and use them directly without
         * having to lock anything.
         */
        if (_S_bin[bin].first[thread_id] == NULL)
          {
            /*
             * Are we using threads?
             * - Yes, lock and check if there are free blocks on the global
             *   list (and if not add new ones), get the first one
             *   and change owner.
             * - No, all operations are made directly to global pool 0
             *   no need to lock or change ownership but check for free
             *   blocks on global list (and if not add new ones) and
             *   get the first one.
             */
#ifdef __GTHREADS
            if (__gthread_active_p())
              {
                __gthread_mutex_lock(_S_bin[bin].mutex);

                if (_S_bin[bin].first[0] == NULL)
                  {
                    _S_bin[bin].first[0] =
                      (block_record*)malloc(_S_chunk_size);

                    if (!_S_bin[bin].first[0])
                      {
                        __gthread_mutex_unlock(_S_bin[bin].mutex);
                        __throw_bad_alloc();
                      }

                    size_t bin_t = 1 << bin;
                    size_t block_count =
                      _S_chunk_size /(bin_t + sizeof(block_record));

                    _S_bin[bin].free[0] = block_count;

                    block_count--;
                    block = _S_bin[bin].first[0];

                    while (block_count > 0)
                      {
                        block->next = (block_record*)((char*)block +
                                      (bin_t + sizeof(block_record)));
                        block = block->next;
                        block_count--;
                      }

                    block->next = NULL;
                    _S_bin[bin].last[0] = block;
                  }

                block = _S_bin[bin].first[0];

                /*
                 * Remove from list and count down the available counter on
                 * global pool 0.
                 */
                _S_bin[bin].first[0] = _S_bin[bin].first[0]->next;
                _S_bin[bin].free[0]--;

                __gthread_mutex_unlock(_S_bin[bin].mutex);

                /*
                 * Now that we have removed the block from the global
                 * freelist we can change owner and update the used
                 * counter for this thread without locking.
                 */
                block->thread_id = thread_id;
                _S_bin[bin].used[thread_id]++;
              }
            else
#endif
              {
                _S_bin[bin].first[0] = (block_record*)malloc(_S_chunk_size);

                if (!_S_bin[bin].first[0])
                  __throw_bad_alloc();

                size_t bin_t = 1 << bin;
                size_t block_count = 
		  _S_chunk_size / (bin_t + sizeof(block_record));

                _S_bin[bin].free[0] = block_count;

                block_count--;
                block = _S_bin[bin].first[0];

                while (block_count > 0)
                  {
                    block->next = (block_record*)((char*)block +
                                  (bin_t + sizeof(block_record)));
                    block = block->next;
                    block_count--;
                  }

                block->next = NULL;
                _S_bin[bin].last[0] = block;

                block = _S_bin[bin].first[0];

                /*
                 * Remove from list and count down the available counter on
                 * global pool 0 and increase it's used counter.
                 */
                _S_bin[bin].first[0] = _S_bin[bin].first[0]->next;
                _S_bin[bin].free[0]--;
                _S_bin[bin].used[0]++;
              }
          }
        else
          {
            /*
             * "Default" operation - we have blocks on our own freelist
             * grab the first record and update the counters.
             */
            block = _S_bin[bin].first[thread_id];

            _S_bin[bin].first[thread_id] = _S_bin[bin].first[thread_id]->next;
            _S_bin[bin].free[thread_id]--;
            _S_bin[bin].used[thread_id]++;
          }

        return static_cast<_Tp*>(static_cast<void*>((char*)block + sizeof(block_record)));
      }

      void
      deallocate(pointer __p, size_type __n)
      {
        /*
         * Requests larger than _S_max_bytes are handled by
         * malloc/free directly
         */
        if (__n > _S_max_bytes)
          {
            free(__p);
            return;
           }

        /*
         * Round up to power of 2 and figure out which bin to use
         */
        size_t bin = _S_binmap[__n];

#ifdef __GTHREADS
        size_t thread_id = _S_get_thread_id();
#else
        size_t thread_id = 0;
#endif

        block_record* block = (block_record*)((char*)__p
					      - sizeof(block_record));

        /*
         * This block will always be at the back of a list and thus
         * we set its next pointer to NULL.
         */
        block->next = NULL;

#ifdef __GTHREADS
        if (__gthread_active_p())
          {
            /*
             * Calculate the number of records to remove from our freelist
             */
            int remove = _S_bin[bin].free[thread_id] -
                         (_S_bin[bin].used[thread_id] / _S_freelist_headroom);

            /*
             * The calculation above will almost always tell us to
             * remove one or two records at a time, but this creates
             * too much contention when locking and therefore we
             * wait until the number of records is "high enough".
             */
            if (remove > (int)(100 * (_S_no_of_bins - bin)) &&
                remove > (int)(_S_bin[bin].free[thread_id] /
                               _S_freelist_headroom))
              {
                __gthread_mutex_lock(_S_bin[bin].mutex);

                while (remove > 0)
                  {
                    if (_S_bin[bin].first[0] == NULL)
                      _S_bin[bin].first[0] = _S_bin[bin].first[thread_id];
                    else
                      _S_bin[bin].last[0]->next = _S_bin[bin].first[thread_id];

                    _S_bin[bin].last[0] = _S_bin[bin].first[thread_id];

                    _S_bin[bin].first[thread_id] =
                      _S_bin[bin].first[thread_id]->next;

                    _S_bin[bin].free[0]++;
                    _S_bin[bin].free[thread_id]--;

                    remove--;
                  }

                _S_bin[bin].last[0]->next = NULL;

                __gthread_mutex_unlock(_S_bin[bin].mutex);
              }

            /*
             * Did we allocate this block?
             * - Yes, return it to our freelist
             * - No, return it to global pool
             */
            if (thread_id == block->thread_id)
              {
                if (_S_bin[bin].first[thread_id] == NULL)
                  _S_bin[bin].first[thread_id] = block;
                else
                  _S_bin[bin].last[thread_id]->next = block;

                _S_bin[bin].last[thread_id] = block;

                _S_bin[bin].free[thread_id]++;
                _S_bin[bin].used[thread_id]--;
              }
            else
              {
                __gthread_mutex_lock(_S_bin[bin].mutex);

                if (_S_bin[bin].first[0] == NULL)
                  _S_bin[bin].first[0] = block;
                else
                  _S_bin[bin].last[0]->next = block;

                _S_bin[bin].last[0] = block;

                _S_bin[bin].free[0]++;
                _S_bin[bin].used[block->thread_id]--;

                __gthread_mutex_unlock(_S_bin[bin].mutex);
              }
          }
        else
#endif
          {
            /*
             * Single threaded application - return to global pool
             */
            if (_S_bin[bin].first[0] == NULL)
              _S_bin[bin].first[0] = block;
            else
              _S_bin[bin].last[0]->next = block;

            _S_bin[bin].last[0] = block;

            _S_bin[bin].free[0]++;
            _S_bin[bin].used[0]--;
          }
      }
    };

  template<typename _Tp>
    void
    __mt_alloc<_Tp>::
    _S_init()
    {
      /*
       * Calculate the number of bins required based on _S_max_bytes,
       * _S_no_of_bins is initialized to 1 below.
       */
      {
        size_t bin_t = 1;
        while (_S_max_bytes > bin_t)
          {
            bin_t = bin_t << 1;
            _S_no_of_bins++;
          }
      }

      /*
       * Setup the bin map for quick lookup of the relevant bin
       */
      _S_binmap = (binmap_type*)
        malloc ((_S_max_bytes + 1) * sizeof(binmap_type));

      if (!_S_binmap)
        __throw_bad_alloc();

      binmap_type* bp_t = _S_binmap;
      binmap_type bin_max_t = 1;
      binmap_type bin_t = 0;
      for (binmap_type ct = 0; ct <= _S_max_bytes; ct++)
        {
          if (ct > bin_max_t)
            {
              bin_max_t <<= 1;
              bin_t++;
            }
          *bp_t++ = bin_t;
        }

      /*
       * If __gthread_active_p() create and initialize the list of
       * free thread ids. Single threaded applications use thread id 0
       * directly and have no need for this.
       */
#ifdef __GTHREADS
      if (__gthread_active_p())
        {
	  _S_thread_freelist_first =
            (thread_record*)malloc(sizeof(thread_record) * _S_max_threads);

          if (!_S_thread_freelist_first)
            __throw_bad_alloc();

          /*
           * NOTE! The first assignable thread id is 1 since the global
           * pool uses id 0
           */
          size_t i;
          for (i = 1; i < _S_max_threads; i++)
            {
              _S_thread_freelist_first[i - 1].next = 
		&_S_thread_freelist_first[i];

              _S_thread_freelist_first[i - 1].id = i;
            }

          /*
           * Set last record and pointer to this
           */
          _S_thread_freelist_first[i - 1].next = NULL;
          _S_thread_freelist_first[i - 1].id = i;
          _S_thread_freelist_last = &_S_thread_freelist_first[i - 1];

          /*
           * Initialize per thread key to hold pointer to
           * _S_thread_freelist NOTE! Here's an ugly workaround - if
           * _S_thread_key_destr is not explicitly called at least
           * once it won't be linked into the application. This is the
           * behavior of template methods and __gthread_key_create()
           * takes only a pointer to the function and does not cause
           * the compiler to create an instance.
           */
          _S_thread_key_destr(NULL);
          __gthread_key_create(&_S_thread_key, _S_thread_key_destr);
        }
#endif

      /*
       * Initialize _S_bin and its members
       */
      _S_bin = (bin_record*)malloc(sizeof(bin_record) * _S_no_of_bins);

      if (!_S_bin)
        __throw_bad_alloc();

       for (size_t bin = 0; bin < _S_no_of_bins; bin++)
        {
	  std::size_t __n = _S_max_threads + 1;

          _S_bin[bin].first = (block_record**) 
	    malloc(sizeof(block_record*) * __n);

          if (!_S_bin[bin].first)
            __throw_bad_alloc();

          _S_bin[bin].last = (block_record**) 
	    malloc(sizeof(block_record*) * __n);

          if (!_S_bin[bin].last)
            __throw_bad_alloc();

          _S_bin[bin].free = (size_t*) malloc(sizeof(size_t) * __n);

          if (!_S_bin[bin].free)
            __throw_bad_alloc();

          _S_bin[bin].used = (size_t*) malloc(sizeof(size_t) * __n);

          if (!_S_bin[bin].used)
            __throw_bad_alloc();

#ifdef __GTHREADS
          _S_bin[bin].mutex =(__gthread_mutex_t*)  malloc(sizeof(__gthread_mutex_t));

#ifdef __GTHREAD_MUTEX_INIT
	  {
	    // Do not copy a POSIX/gthr mutex once in use.
	    __gthread_mutex_t __tmp = __GTHREAD_MUTEX_INIT;
	    *_S_bin[bin].mutex = __tmp;
	  }
#else
	  { __GTHREAD_MUTEX_INIT_FUNCTION (_S_bin[bin].mutex); }
#endif
#endif

          for (size_t thread = 0; thread <= _S_max_threads; thread++)
            {
              _S_bin[bin].first[thread] = NULL;
              _S_bin[bin].last[thread] = NULL;
              _S_bin[bin].free[thread] = 0;
              _S_bin[bin].used[thread] = 0;
            }
        }

      _S_initialized = true;
    }

#ifdef __GTHREADS
  template<typename _Tp>
    void
    __mt_alloc<_Tp>::
    _S_thread_key_destr(void* freelist_pos)
    {
      /*
       * If the thread - when it dies - still has records on its
       * freelist we return them to the global pool here.
       */
      for (size_t bin = 0; bin < _S_no_of_bins; bin++)
        {
          block_record* block =
            _S_bin[bin].first[((thread_record*)freelist_pos)->id];

          if (block != NULL)
            {
              __gthread_mutex_lock(_S_bin[bin].mutex);
              while (block != NULL)
                {
                  if (_S_bin[bin].first[0] == NULL)
                    _S_bin[bin].first[0] = block;
                  else
                    _S_bin[bin].last[0]->next = block;

                  _S_bin[bin].last[0] = block;
                  block = block->next;
                  _S_bin[bin].free[0]++;
                }

              _S_bin[bin].last[0]->next = NULL;
              __gthread_mutex_unlock(_S_bin[bin].mutex);
            }
        }

      /*
       * Return this thread id record to thread_freelist
       */
      __gthread_mutex_lock(&_S_thread_freelist_mutex);
      _S_thread_freelist_last->next = (thread_record*)freelist_pos;
      _S_thread_freelist_last = (thread_record*)freelist_pos;
      _S_thread_freelist_last->next = NULL;
      __gthread_mutex_unlock(&_S_thread_freelist_mutex);
    }

  template<typename _Tp>
    size_t
    __mt_alloc<_Tp>::
    _S_get_thread_id()
    {
      /*
       * If we have thread support and it's active we check the thread
       * key value and return it's id or if it's not set we take the
       * first record from _S_thread_freelist and sets the key and
       * returns it's id.
       */
      if (__gthread_active_p())
        {
          thread_record* freelist_pos;

          if ((freelist_pos =
              (thread_record*)__gthread_getspecific(_S_thread_key)) == NULL)
            {
              /*
               * Since _S_max_threads must be larger than the
               * theoretical max number of threads of the OS the list
               * can never be empty.
               */
              __gthread_mutex_lock(&_S_thread_freelist_mutex);
              freelist_pos = _S_thread_freelist_first;
              _S_thread_freelist_first = _S_thread_freelist_first->next;
              __gthread_mutex_unlock(&_S_thread_freelist_mutex);

              __gthread_setspecific(_S_thread_key, (void*)freelist_pos);

              /*
               * Since thread_ids may/will be reused (espcially in
               * producer/consumer applications) we make sure that the
               * list pointers and free counter is reset BUT as the
               * "old" thread may still be owner of some memory (which
               * is referred to by other threads and thus not freed)
               * we don't reset the used counter.
               */
              for (size_t bin = 0; bin < _S_no_of_bins; bin++)
                {
                  _S_bin[bin].first[freelist_pos->id] = NULL;
                  _S_bin[bin].last[freelist_pos->id] = NULL;
                  _S_bin[bin].free[freelist_pos->id] = 0;
                }
            }

          return freelist_pos->id;
        }

      /*
       * Otherwise (no thread support or inactive) all requests are
       * served from the global pool 0.
       */
      return 0;
    }

  template<typename _Tp> __gthread_once_t
  __mt_alloc<_Tp>::_S_once_mt = __GTHREAD_ONCE_INIT;
#endif

  template<typename _Tp> bool
  __mt_alloc<_Tp>::_S_initialized = false;

  template<typename _Tp> typename __mt_alloc<_Tp>::binmap_type*
  __mt_alloc<_Tp>::_S_binmap = NULL;

  /*
   * Allocation requests (after round-up to power of 2) below this
   * value will be handled by the allocator. A raw malloc/free() call
   * will be used for requests larger than this value.
   */
  template<typename _Tp> size_t
  __mt_alloc<_Tp>::_S_max_bytes = 128;

  /*
   * In order to avoid fragmenting and minimize the number of malloc()
   * calls we always request new memory using this value. Based on
   * previous discussions on the libstdc++ mailing list we have
   * choosen the value below. See
   * http://gcc.gnu.org/ml/libstdc++/2001-07/msg00077.html
   */
  template<typename _Tp> size_t
  __mt_alloc<_Tp>::_S_chunk_size = 4096 - 4 * sizeof(void*);

  /*
   * The maximum number of supported threads. Our Linux 2.4.18 reports
   * 4070 in /proc/sys/kernel/threads-max
   */
  template<typename _Tp> size_t
  __mt_alloc<_Tp>::_S_max_threads = 4096;

  /*
   * Actual value calculated in _S_init()
   */
  template<typename _Tp> size_t
  __mt_alloc<_Tp>::_S_no_of_bins = 1;

  /*
   * Each time a deallocation occurs in a threaded application we make
   * sure that there are no more than _S_freelist_headroom % of used
   * memory on the freelist. If the number of additional records is
   * more than _S_freelist_headroom % of the freelist, we move these
   * records back to the global pool.
   */
  template<typename _Tp> size_t
  __mt_alloc<_Tp>::_S_freelist_headroom = 10;

  /*
   * Actual initialization in _S_init()
   */
#ifdef __GTHREADS
  template<typename _Tp> typename __mt_alloc<_Tp>::thread_record*
  __mt_alloc<_Tp>::_S_thread_freelist_first = NULL;

  template<typename _Tp> typename __mt_alloc<_Tp>::thread_record*
  __mt_alloc<_Tp>::_S_thread_freelist_last = NULL;

  template<typename _Tp> __gthread_mutex_t
  __mt_alloc<_Tp>::_S_thread_freelist_mutex = __GTHREAD_MUTEX_INIT;

  /*
   * Actual initialization in _S_init()
   */
  template<typename _Tp> __gthread_key_t
  __mt_alloc<_Tp>::_S_thread_key;
#endif

  template<typename _Tp> typename __mt_alloc<_Tp>::bin_record*
  __mt_alloc<_Tp>::_S_bin = NULL;
} // namespace __gnu_cxx

#endif

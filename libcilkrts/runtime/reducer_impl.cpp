/* reducer_impl.cpp                  -*-C++-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2009-2013, Intel Corporation
 *  All rights reserved.
 *  
 *  @copyright
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *  
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *    * Neither the name of Intel Corporation nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *  
 *  @copyright
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 *  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 *  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *
 *  Patents Pending, Intel Corporation.
 **************************************************************************/

/**
 * Support for reducers
 */

// ICL: Don't complain about conversion from pointer to same-sized integral type
// in hashfun.  That's why we're using size_t
#ifdef _WIN32
#   pragma warning(disable: 1684)
#endif

#include "reducer_impl.h"
#include "scheduler.h"
#include "bug.h"
#include "os.h"
#include "global_state.h"
#include "frame_malloc.h"

#include "cilk/hyperobject_base.h"
#include "cilktools/cilkscreen.h"
#include "internal/abi.h"

#if REDPAR_DEBUG > 0
#include <stdio.h>
#include <stdlib.h>
#endif


#define DBG if(0) // if(1) enables some internal checks

// Check that w is the currently executing worker.  This method is a
// no-op unless the debug level is set high enough.
static inline void verify_current_wkr(__cilkrts_worker *w)
{
#if REDPAR_DEBUG >= 5
    __cilkrts_worker* tmp = __cilkrts_get_tls_worker();
    if (w != tmp) {
        fprintf(stderr, "W=%d, actual=%d... missing a refresh....\n",
                w->self,
                tmp->self);
    }
    CILK_ASSERT(w == tmp); // __cilkrts_get_tls_worker());
#endif
}

// Suppress clang warning that the expression result is unused
#if defined(__clang__) && (! defined(__INTEL_COMPILER))
#   pragma clang diagnostic push
#   pragma clang diagnostic ignored "-Wunused-value"
#endif // __clang__

/// Helper class to disable and re-enable Cilkscreen
struct DisableCilkscreen
{
    DisableCilkscreen () { __cilkscreen_disable_checking(); }
    ~DisableCilkscreen () { __cilkscreen_enable_checking(); }
};

/// Helper class to enable and re-disable Cilkscreen
struct EnableCilkscreen
{
    EnableCilkscreen () { __cilkscreen_enable_checking(); }
    ~EnableCilkscreen () { __cilkscreen_disable_checking(); }
};

#if defined(__clang__) && (! defined(__INTEL_COMPILER))
#   pragma clang diagnostic pop
#endif // __clang__

/**
 * @brief Element for a hyperobject
 */
struct elem {
    void                       *key;  ///< Shared key for this hyperobject
    __cilkrts_hyperobject_base *hb;   ///< Base of the hyperobject.
    void                       *view; ///< Strand-private view of this hyperobject
    /// Destroy and deallocate the view object for this element and set view to
    /// null.
    void destroy();

    /// Returns true if this element contains a leftmost view.
    bool is_leftmost() const;
};

/** Bucket containing at most NMAX elements */
struct bucket {
    /// Size of the array of elements for this bucket
    size_t nmax;

    /**
     * We use the ``struct hack'' to allocate an array of variable
     * dimension at the end of the struct.  However, we allocate a
     * total of NMAX+1 elements instead of NMAX.  The last one always
     * has key == 0, which we use as a termination criterion
     */
    elem el[1];
};

/**
 * Class that implements the map for reducers so we can find the
 * view for a strand.
 */
struct cilkred_map {
    /** Handy pointer to the global state */
    global_state_t *g;

    /** Number of elements in table */
    size_t nelem;

    /** Number of buckets */
    size_t nbuckets;

    /** Array of pointers to buckets */
    bucket **buckets;

    /** Set true if merging (for debugging purposes) */
    bool merging;

    /** Set true for leftmost reducer map */
    bool is_leftmost;

    /** @brief Return element mapped to 'key' or null if not found. */
    elem *lookup(void *key);

    /**
     * @brief Insert key/value element into hash map without rehashing.
     * Does not check for duplicate key.
     */
    elem *insert_no_rehash(__cilkrts_worker           *w,
			   void                       *key,
			   __cilkrts_hyperobject_base *hb,
                           void                       *value);

    /**
     * @brief Insert key/value element into hash map, rehashing if necessary.
     * Does not check for duplicate key.
     */
    inline elem *rehash_and_insert(__cilkrts_worker           *w,
				   void                       *key,
				   __cilkrts_hyperobject_base *hb,
                                   void                       *value);

    /** @brief Grow bucket by one element, reallocating bucket if necessary */
    static elem *grow(__cilkrts_worker *w, bucket **bp);

    /** @brief Rehash a worker's reducer map */
    void rehash(__cilkrts_worker *);

    /**
     * @brief Returns true if a rehash is needed due to the number of elements that
     * have been inserted.
     */
    inline bool need_rehash_p() const;

    /** @brief Allocate and initialize the buckets */
    void make_buckets(__cilkrts_worker *w, size_t nbuckets);

    /**
     * Specify behavior when the same key is present in both maps passed
     * into merge().
     */
    enum merge_kind
    {
        MERGE_UNORDERED, ///< Assertion fails
        MERGE_INTO_LEFT, ///< Merges the argument from the right into the left
        MERGE_INTO_RIGHT ///< Merges the argument from the left into the right
    };

    /**
     * @brief Merge another reducer map into this one, destroying the other map in
     * the process.
     */
    __cilkrts_worker* merge(__cilkrts_worker *current_wkr,
			    cilkred_map      *other_map,
			    enum merge_kind   kind);

    /** @brief check consistency of a reducer map */
    void check(bool allow_null_view);

    /** @brief Test whether the cilkred_map is empty */
    bool is_empty() { return nelem == 0; }
};

static inline struct cilkred_map* install_new_reducer_map(__cilkrts_worker *w) {
    cilkred_map *h;
    h = __cilkrts_make_reducer_map(w);
    w->reducer_map = h;
    return h;
}

static size_t sizeof_bucket(size_t nmax)
{
    bucket *b = 0;
    return (sizeof(*b) + nmax * sizeof(b->el[0]));
}

static bucket *alloc_bucket(__cilkrts_worker *w, size_t nmax)
{
    bucket *b = (bucket *)
        __cilkrts_frame_malloc(w, sizeof_bucket(nmax));
    b->nmax = nmax;
    return b;
}

static void free_bucket(__cilkrts_worker *w, bucket **bp)
{
    bucket *b = *bp;
    if (b) {
        __cilkrts_frame_free(w, b, sizeof_bucket(b->nmax));
        *bp = 0;
    }
}

/* round up nmax to fill a memory allocator block completely */
static size_t roundup(size_t nmax)
{
    size_t sz = sizeof_bucket(nmax);

    /* round up size to a full malloc block */
    sz = __cilkrts_frame_malloc_roundup(sz);

    /* invert sizeof_bucket() */
    nmax = ((sz - sizeof(bucket)) / sizeof(elem));
     
    return nmax;
}

static bool is_power_of_2(size_t n)
{
    return (n & (n - 1)) == 0;
}

void cilkred_map::make_buckets(__cilkrts_worker *w, 
                               size_t            new_nbuckets)
{     
    nbuckets = new_nbuckets;

    CILK_ASSERT(is_power_of_2(nbuckets));
#if defined __GNUC__ && defined __ICC 
    /* bug workaround -- suppress calls to _intel_fast_memset */
    bucket *volatile*new_buckets = (bucket *volatile*)
#else
    bucket **new_buckets = (bucket **)
#endif
        __cilkrts_frame_malloc(w, nbuckets * sizeof(*(buckets)));

#if REDPAR_DEBUG >= 1
    fprintf(stderr, "W=%d, desc=make_buckets, new_buckets=%p, new_nbuckets=%zd\n",
	    w->self, new_buckets, new_nbuckets);
#endif

    for (size_t i = 0; i < new_nbuckets; ++i)
        new_buckets[i] = 0;
#if defined __GNUC__ && defined __ICC 
    buckets = (bucket **)new_buckets;
#else
    buckets = new_buckets;
#endif
    nelem = 0;
}

static void free_buckets(__cilkrts_worker  *w, 
                         bucket           **buckets,
                         size_t             nbuckets)
{
    size_t i;

#if REDPAR_DEBUG >= 1
    verify_current_wkr(w);
    fprintf(stderr, "W=%d, desc=free_buckets, buckets=%p, size=%zd\n",
	    w->self, buckets,
	    nbuckets * sizeof(*buckets));
#endif

    for (i = 0; i < nbuckets; ++i)
        free_bucket(w, buckets + i);

    __cilkrts_frame_free(w, buckets, nbuckets * sizeof(*buckets));
}

static size_t minsz(size_t nelem)
{
    return 1U + nelem + nelem / 8U;
}

static size_t nextsz(size_t nelem)
{
    return 2 * nelem;
}

bool cilkred_map::need_rehash_p() const
{
    return minsz(nelem) > nbuckets;
}

static inline size_t hashfun(const cilkred_map *h, void *key)
{
    size_t k = (size_t) key;

    k ^= k >> 21;
    k ^= k >> 8;
    k ^= k >> 3;

    return k & (h->nbuckets - 1);
}

// Given a __cilkrts_hyperobject_base, return the key to that hyperobject in
// the reducer map.
static inline void* get_hyperobject_key(__cilkrts_hyperobject_base *hb)
{
    // The current implementation uses the address of the lefmost view as the
    // key.
    return reinterpret_cast<char*>(hb) + hb->__view_offset;
}

// Given a hyperobject key, return a pointer to the leftmost object.  In the
// current implementation, the address of the leftmost object IS the key, so
// this function is an effective noop.
static inline void* get_leftmost_view(void *key)
{
    return key;
}

/* debugging support: check consistency of a reducer map */
void cilkred_map::check(bool allow_null_view)
{
    size_t count = 0;

    CILK_ASSERT(buckets);
    for (size_t i = 0; i < nbuckets; ++i) {
        bucket *b = buckets[i];
        if (b) 
            for (elem *el = b->el; el->key; ++el) {
                CILK_ASSERT(allow_null_view || el->view);
                ++count;
            }
    }
    CILK_ASSERT(nelem == count);
    /*global_reducer_map::check();*/
}             

/* grow bucket by one element, reallocating bucket if necessary */
elem *cilkred_map::grow(__cilkrts_worker *w, 
                        bucket          **bp)
{
    size_t i, nmax, nnmax;
    bucket *b, *nb;

    b = *bp;
    if (b) {
        nmax = b->nmax;
        /* find empty element if any */
        for (i = 0; i < nmax; ++i) 
            if (b->el[i].key == 0) 
                return &(b->el[i]);
        /* do not use the last one even if empty */
    } else {
        nmax = 0;
    }

    verify_current_wkr(w);
    /* allocate a new bucket */
    nnmax = roundup(2 * nmax);
    nb = alloc_bucket(w, nnmax);


    /* copy old bucket into new */
    for (i = 0; i < nmax; ++i)
        nb->el[i] = b->el[i];
     
    free_bucket(w, bp); *bp = nb;

    /* zero out extra elements */
    for (; i < nnmax; ++i)
        nb->el[i].key = 0;

    /* zero out the last one */
    nb->el[i].key = 0;
  
    return &(nb->el[nmax]);
}

elem *cilkred_map::insert_no_rehash(__cilkrts_worker           *w,
				    void                       *key,
				    __cilkrts_hyperobject_base *hb,
                                    void                       *view)
{

#if REDPAR_DEBUG >= 2
    fprintf(stderr, "[W=%d, desc=insert_no_rehash, this_map=%p]\n",
	    w->self, this);
    verify_current_wkr(w);
#endif
    
    CILK_ASSERT((w == 0 && g == 0) || w->g == g);
    CILK_ASSERT(key != 0);
    CILK_ASSERT(view != 0);
	    
    elem *el = grow(w, &(buckets[hashfun(this, key)]));

#if REDPAR_DEBUG >= 3
    fprintf(stderr, "[W=%d, this=%p, inserting key=%p, view=%p, el = %p]\n",
	    w->self, this, key, view, el);
#endif

    el->key = key;
    el->hb  = hb;
    el->view = view;
    ++nelem;

    return el;
}

void cilkred_map::rehash(__cilkrts_worker *w)
{
#if REDPAR_DEBUG >= 1
    fprintf(stderr, "[W=%d, desc=rehash, this_map=%p, g=%p, w->g=%p]\n",
	    w->self, this, g, w->g);
    verify_current_wkr(w);
#endif
    CILK_ASSERT((w == 0 && g == 0) || w->g == g);
    
    size_t onbuckets = nbuckets;
    size_t onelem = nelem;
    bucket **obuckets = buckets;
    size_t i;
    bucket *b;

    make_buckets(w, nextsz(nbuckets));
     
    for (i = 0; i < onbuckets; ++i) {
        b = obuckets[i];
        if (b) {
            elem *oel;
            for (oel = b->el; oel->key; ++oel)
                insert_no_rehash(w, oel->key, oel->hb, oel->view);
        }
    }

    CILK_ASSERT(nelem == onelem);

    free_buckets(w, obuckets, onbuckets);
}

elem *cilkred_map::rehash_and_insert(__cilkrts_worker           *w,
				     void                       *key,
                                     __cilkrts_hyperobject_base *hb,
				     void                       *view)
{

#if REDPAR_DEBUG >= 1
    fprintf(stderr, "W=%d, this_map =%p, inserting key=%p, view=%p\n",
	    w->self, this, key, view);
    verify_current_wkr(w);
#endif

    if (need_rehash_p()) 
        rehash(w);

    return insert_no_rehash(w, key, hb, view);
}


elem *cilkred_map::lookup(void *key)
{
    bucket *b = buckets[hashfun(this, key)];

    if (b) {
        elem *el;
        for (el = b->el; el->key; ++el) {
            if (el->key == key) {
                CILK_ASSERT(el->view);
                return el;
            }
        }
    }

    return 0;
}

void elem::destroy()
{
    if (! is_leftmost()) {

        // Call destroy_fn and deallocate_fn on the view, but not if it's the
        // leftmost view.
        cilk_c_monoid *monoid = &(hb->__c_monoid);
        cilk_c_reducer_destroy_fn_t    destroy_fn    = monoid->destroy_fn;
        cilk_c_reducer_deallocate_fn_t deallocate_fn = monoid->deallocate_fn;
	
        destroy_fn((void*)hb, view);
        deallocate_fn((void*)hb, view);
    }

    view = 0;
}

inline
bool elem::is_leftmost() const
{
    // implementation uses the address of the leftmost view as the key, so if
    // key == view, then this element refers to the leftmost view.
    return key == view;
}

/* remove the reducer from the current reducer map.  If the reducer
   exists in maps other than the current one, the behavior is
   undefined. */
extern "C"
CILK_EXPORT void __CILKRTS_STRAND_STALE(
    __cilkrts_hyper_destroy(__cilkrts_hyperobject_base *hb))
{
    // Disable Cilkscreen for the duration of this call.  The destructor for
    // this class will re-enable Cilkscreen when the method returns.  This
    // will prevent Cilkscreen from reporting apparent races in reducers
    DisableCilkscreen x;

    __cilkrts_worker* w = __cilkrts_get_tls_worker();
    if (! w) {
        // If no worker, then Cilk is not running and there is no reducer
        // map.  Do nothing.  The reducer's destructor will take care of
        // destroying the leftmost view.
        return;
    }

const char *UNSYNCED_REDUCER_MSG =
    "Destroying a reducer while it is visible to unsynced child tasks, or\n"
    "calling CILK_C_UNREGISTER_REDUCER() on an unregistered reducer.\n"
    "Did you forget a _Cilk_sync or CILK_C_REGISTER_REDUCER()?";

    cilkred_map* h = w->reducer_map;
    if (NULL == h)
	cilkos_error(UNSYNCED_REDUCER_MSG); // Does not return

    if (h->merging) {
	verify_current_wkr(w);
	__cilkrts_bug("User error: hyperobject used by another hyperobject");
    }

    void* key = get_hyperobject_key(hb);
    elem *el = h->lookup(key);

    // Verify that the reducer is being destroyed from the leftmost strand for
    // which the reducer is defined.
    if (! (el && el->is_leftmost()))
	cilkos_error(UNSYNCED_REDUCER_MSG);

#if REDPAR_DEBUG >= 3
    fprintf(stderr, "[W=%d, key=%p, lookup in map %p, found el=%p, about to destroy]\n",
            w->self, key, h, el);
#endif
	
    // Remove the element from the hash bucket.  Do not bother shrinking
    // the bucket. Note that the destroy() function does not actually
    // call the destructor for the leftmost view.
    el->destroy();
    do {
        el[0] = el[1];
        ++el;
    } while (el->key);
    --h->nelem;

#if REDPAR_DEBUG >= 2
    fprintf(stderr, "[W=%d, desc=hyper_destroy_finish, key=%p, w->reducer_map=%p]\n",
	    w->self, key, w->reducer_map);
#endif 
}
    
extern "C"
CILK_EXPORT
void __cilkrts_hyper_create(__cilkrts_hyperobject_base *hb)
{
    // This function registers the specified hyperobject in the current
    // reducer map and registers the initial value of the hyperobject as the
    // leftmost view of the reducer.
    __cilkrts_worker *w = __cilkrts_get_tls_worker();
    if (! w) {
        // If there is no worker, then there is nothing to do: The iniitial
        // value will automatically be used as the left-most view when we
        // enter Cilk.
        return;
    }

    // Disable Cilkscreen for the duration of this call.  The destructor for
    // this class will re-enable Cilkscreen when the method returns.  This
    // will prevent Cilkscreen from reporting apparent races in reducers
    DisableCilkscreen x;

    void* key = get_hyperobject_key(hb);
    void* view = get_leftmost_view(key);
    cilkred_map *h = w->reducer_map;

    if (__builtin_expect(!h, 0)) {
	h = install_new_reducer_map(w);
#if REDPAR_DEBUG >= 2
	fprintf(stderr, "[W=%d, hb=%p, hyper_create, isntalled new map %p, view=%p]\n",
		w->self, hb, h, view);
#endif
    }

    /* Must not exist. */
    CILK_ASSERT(h->lookup(key) == NULL);

#if REDPAR_DEBUG >= 3
    verify_current_wkr(w);
    fprintf(stderr, "[W=%d, hb=%p, lookup in map %p of view %p, should be null]\n",
	    w->self, hb, h, view);
    fprintf(stderr, "W=%d, h=%p, inserting key %p, view%p\n",
	    w->self,
	    h,
	    &(hb->__c_monoid),
	    view);
#endif    

    if (h->merging)
        __cilkrts_bug("User error: hyperobject used by another hyperobject");

    CILK_ASSERT(w->reducer_map == h);
    // The address of the leftmost value is the same as the key for lookup.
    (void) h->rehash_and_insert(w, view, hb, view);
}

extern "C"
CILK_EXPORT void* __CILKRTS_STRAND_PURE(
    __cilkrts_hyper_lookup(__cilkrts_hyperobject_base *hb))
{
    __cilkrts_worker* w = __cilkrts_get_tls_worker_fast();
    void* key = get_hyperobject_key(hb);
    if (! w)
        return get_leftmost_view(key);

    // Disable Cilkscreen for the duration of this call.  This will
    // prevent Cilkscreen from reporting apparent races in reducers
    DisableCilkscreen dguard;

    if (__builtin_expect(w->g->force_reduce, 0))
        __cilkrts_promote_own_deque(w);
    cilkred_map* h = w->reducer_map;

    if (__builtin_expect(!h, 0)) {
	h = install_new_reducer_map(w);
    }

    if (h->merging)
        __cilkrts_bug("User error: hyperobject used by another hyperobject");
    elem* el = h->lookup(key);
    if (! el) {
        /* lookup failed; insert a new default element */
        void *rep;

        {
            /* re-enable cilkscreen while calling the constructor */
            EnableCilkscreen eguard;
            if (h->is_leftmost)
            {
                // This special case is called only if the reducer was not
                // registered using __cilkrts_hyper_create, e.g., if this is a
                // C reducer in global scope or if there is no bound worker.
                rep = get_leftmost_view(key);
            }
            else
            {
                rep = hb->__c_monoid.allocate_fn((void*)hb,
						 hb->__view_size);
                // TBD: Handle exception on identity function
                hb->__c_monoid.identity_fn((void*)hb, rep);
            }
        }

#if REDPAR_DEBUG >= 3
	fprintf(stderr, "W=%d, h=%p, inserting key %p, view%p\n",
		w->self,
		h,
		&(hb->__c_monoid),
		rep);
	CILK_ASSERT(w->reducer_map == h);
#endif
        el = h->rehash_and_insert(w, key, hb, rep);
    }

    return el->view;
}

extern "C" CILK_EXPORT
void* __cilkrts_hyperobject_alloc(void* ignore, std::size_t bytes)
{
    return std::malloc(bytes);
}

extern "C" CILK_EXPORT
void __cilkrts_hyperobject_dealloc(void* ignore, void* view)
{
    std::free(view);
}

/* No-op destroy function */
extern "C" CILK_EXPORT
void __cilkrts_hyperobject_noop_destroy(void* ignore, void* ignore2)
{
}

cilkred_map *__cilkrts_make_reducer_map(__cilkrts_worker *w)
{
    CILK_ASSERT(w);

    cilkred_map *h;
    size_t nbuckets = 1; /* default value */
    
    h = (cilkred_map *)__cilkrts_frame_malloc(w, sizeof(*h));
#if REDPAR_DEBUG >= 1
    fprintf(stderr, "[W=%d, desc=make_reducer_frame_malloc_reducer_map, h=%p]\n",
	    w->self, h);
#endif

    h->g = w ? w->g : 0;
    h->make_buckets(w, nbuckets);
    h->merging = false;
    h->is_leftmost = false;

    return h;
}

/* Destroy a reducer map.  The map must have been allocated
   from the worker's global context and should have been
   allocated from the same worker. */
void __cilkrts_destroy_reducer_map(__cilkrts_worker *w, cilkred_map *h)
{
    CILK_ASSERT((w == 0 && h->g == 0) || w->g == h->g);
    verify_current_wkr(w);

    /* the reducer map is allowed to contain el->view == NULL here (and
       only here).  We set el->view == NULL only when we know that the
       map will be destroyed immediately afterwards. */
    DBG h->check(/*allow_null_view=*/true);

    bucket *b;
    size_t i;

    for (i = 0; i < h->nbuckets; ++i) {
        b = h->buckets[i];
        if (b) {
            elem *el;
            for (el = b->el; el->key; ++el) {
                if (el->view)
                    el->destroy();
            }
        }
    }

    free_buckets(w, h->buckets, h->nbuckets);

#if REDPAR_DEBUG >= 1
    fprintf(stderr, "W=%d, destroy_red_map, freeing map h=%p, size=%zd\n",
	    w->self, h, sizeof(*h));
#endif
    
    __cilkrts_frame_free(w, h, sizeof(*h));
}

/* Set the specified reducer map as the leftmost map if is_leftmost is true,
   otherwise, set it to not be the leftmost map. */
void __cilkrts_set_leftmost_reducer_map(cilkred_map *h, int is_leftmost)
{
    h->is_leftmost = is_leftmost;
}


__cilkrts_worker* cilkred_map::merge(__cilkrts_worker *w,
				     cilkred_map *other_map,
				     enum merge_kind kind)
{
    // Disable Cilkscreen while the we merge the maps.  The destructor for
    // the guard class will re-enable Cilkscreen when it goes out of scope.
    // This will prevent Cilkscreen from reporting apparent races in between
    // the reduce function and the reducer operations.  The Cilk runtime
    // guarantees that a pair of reducer maps will only be merged when no 
    // other strand will access them.
    DisableCilkscreen guard;

#if REDPAR_DEBUG >= 2
    fprintf(stderr, "[W=%d, desc=merge, this_map=%p, other_map=%p]\n",
	    w->self,
	    this, other_map);
#endif
    // Remember the current stack frame.
    __cilkrts_stack_frame *current_sf = w->current_stack_frame;
    merging = true;
    other_map->merging = true;

    // Merging to the leftmost view is a special case because every leftmost
    // element must be initialized before the merge.
    CILK_ASSERT(!other_map->is_leftmost /* || kind == MERGE_UNORDERED */);
    bool merge_to_leftmost = (this->is_leftmost
                              /* && !other_map->is_leftmost */);

    DBG check(/*allow_null_view=*/false);
    DBG other_map->check(/*allow_null_view=*/false);

    for (size_t i = 0; i < other_map->nbuckets; ++i) {
        bucket *b = other_map->buckets[i];
        if (b) {
            for (elem *other_el = b->el; other_el->key; ++other_el) {
                /* Steal the value from the other map, which will be
                   destroyed at the end of this operation. */
                void *other_view = other_el->view;
                CILK_ASSERT(other_view);

                void *key = other_el->key;
		__cilkrts_hyperobject_base *hb = other_el->hb;
                elem *this_el = lookup(key);

                if (this_el == 0 && merge_to_leftmost) {
                    /* Initialize leftmost view before merging. */
                    void* leftmost = get_leftmost_view(key);
                    // leftmost == other_view can be true if the initial view
                    // was created in other than the leftmost strand of the
                    // spawn tree, but then made visible to subsequent strands
                    // (E.g., the reducer was allocated on the heap and the
                    // pointer was returned to the caller.)  In such cases,
                    // parallel semantics says that syncing with earlier
                    // strands will always result in 'this_el' being null,
                    // thus propagating the initial view up the spawn tree
                    // until it reaches the leftmost strand.  When synching
                    // with the leftmost strand, leftmost == other_view will be
                    // true and we must avoid reducing the initial view with
                    // itself.
                    if (leftmost != other_view)
                        this_el = rehash_and_insert(w, key, hb, leftmost);
                }

                if (this_el == 0) {
                    /* move object from other map into this one */
                    rehash_and_insert(w, key, hb, other_view);
                    other_el->view = 0;
                    continue; /* No element-level merge necessary */
                }

                /* The same key is present in both maps with values
                   A and B.  Three choices: fail, A OP B, B OP A. */
                switch (kind)
                {
                case MERGE_UNORDERED:
                    __cilkrts_bug("TLS Reducer race");
                    break;
                case MERGE_INTO_RIGHT:
                    /* Swap elements in order to preserve object
                       identity */
                    other_el->view = this_el->view;
                    this_el->view = other_view;
                    /* FALL THROUGH */
                case MERGE_INTO_LEFT: {
                    /* Stealing should be disabled during reduce
                       (even if force-reduce is enabled). */

#if DISABLE_PARALLEL_REDUCERS
		    __cilkrts_stack_frame * volatile *saved_protected_tail;
		    saved_protected_tail = __cilkrts_disallow_stealing(w, NULL);
#endif

		    {			
			CILK_ASSERT(current_sf->worker == w);
			CILK_ASSERT(w->current_stack_frame == current_sf);

			/* TBD: if reduce throws an exception we need to stop it
			   here. */
			hb->__c_monoid.reduce_fn((void*)hb,
						 this_el->view,
						 other_el->view);
			w = current_sf->worker;

#if REDPAR_DEBUG >= 2
			verify_current_wkr(w);
			CILK_ASSERT(w->current_stack_frame == current_sf);
#endif
		    }

#if DISABLE_PARALLEL_REDUCERS
		    /* Restore stealing */
		    __cilkrts_restore_stealing(w, saved_protected_tail);
#endif

                  } break;
                }
            }
        }
    }
    this->is_leftmost = this->is_leftmost || other_map->is_leftmost;
    merging = false;
    other_map->merging = false;
    verify_current_wkr(w);
    __cilkrts_destroy_reducer_map(w, other_map);
    return w;
}


/**
 * Print routine for debugging the merging of reducer maps.
 * A no-op unless REDPAR_DEBUG set high enough.
 */
static inline
void debug_map_merge(__cilkrts_worker *w,
		     cilkred_map      *left_map,
		     cilkred_map      *right_map,
		     __cilkrts_worker **final_wkr)
{    
#if REDPAR_DEBUG >= 2
    fprintf(stderr, "[W=%d, desc=finish_merge, left_map=%p, right_map=%p, w->reducer_map=%p, right_ans=%p, final_wkr=%d]\n",
	    w->self, left_map, right_map, w->reducer_map, right_map, (*final_wkr)->self);
#endif
}


/**
 * merge RIGHT into LEFT;
 * return whichever map allows for faster merge, and destroy the other one.
 * 
 * *w_ptr should be the currently executing worker.
 * *w_ptr may change during execution if the reduction is parallel.
 */ 
cilkred_map*
merge_reducer_maps(__cilkrts_worker **w_ptr,
		   cilkred_map *left_map,
		   cilkred_map *right_map)
{
    __cilkrts_worker *w = *w_ptr;
    if (!left_map) {
	debug_map_merge(w, left_map, right_map, w_ptr);
        return right_map;
    }

    if (!right_map) {
	debug_map_merge(w, left_map, right_map, w_ptr);
        return left_map;
    }
    
    /* Special case, if left_map is leftmost, then always merge into it.
       For C reducers this forces lazy creation of the leftmost views. */
    if (left_map->is_leftmost || left_map->nelem > right_map->nelem) {	
	*w_ptr = left_map->merge(w, right_map, cilkred_map::MERGE_INTO_LEFT);
	debug_map_merge(*w_ptr, left_map, right_map, w_ptr);
        return left_map;
    } else {
        *w_ptr = right_map->merge(w, left_map, cilkred_map::MERGE_INTO_RIGHT);
	debug_map_merge(*w_ptr, left_map, right_map, w_ptr);
        return right_map;
    }
}

/**
 * Merges RIGHT into LEFT, and then repeatedly calls
 * merge_reducer_maps_helper() until (*w_ptr)->reducer_map is NULL.
 *
 *  *w_ptr may change as reductions execute.
 */ 
cilkred_map*
repeated_merge_reducer_maps(__cilkrts_worker **w_ptr,
			    cilkred_map      *left_map,
			    cilkred_map      *right_map)
{
    // Note: if right_map == NULL but w->reducer_map != NULL, then
    // this loop will reduce w->reducer_map into left_map.
    do {
	left_map = merge_reducer_maps(w_ptr, left_map, right_map);
	verify_current_wkr(*w_ptr);

	// Pull any newly created reducer map and loop around again.
	right_map = (*w_ptr)->reducer_map;
	(*w_ptr)->reducer_map = NULL;
    } while (right_map);
    return left_map;
}

/* End reducer_impl.cpp */

// Copyright 2010 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"
#include "arch.h"
#include "malloc.h"

enum { debug = 0 };

typedef struct Fin Fin;
struct Fin
{
	void (*fn)(void*);
	const struct __go_func_type *ft;
};

// Finalizer hash table.  Direct hash, linear scan, at most 3/4 full.
// Table size is power of 3 so that hash can be key % max.
// Key[i] == (void*)-1 denotes free but formerly occupied entry
// (doesn't stop the linear scan).
// Key and val are separate tables because the garbage collector
// must be instructed to ignore the pointers in key but follow the
// pointers in val.
typedef struct Fintab Fintab;
struct Fintab
{
	Lock;
	void **fkey;
	Fin *val;
	int32 nkey;	// number of non-nil entries in key
	int32 ndead;	// number of dead (-1) entries in key
	int32 max;	// size of key, val allocations
};

#define TABSZ 17
#define TAB(p) (&fintab[((uintptr)(p)>>3)%TABSZ])

static struct {
	Fintab;
	uint8 pad[0 /* CacheLineSize - sizeof(Fintab) */];	
} fintab[TABSZ];

static void
addfintab(Fintab *t, void *k, void (*fn)(void*), const struct __go_func_type *ft)
{
	int32 i, j;

	i = (uintptr)k % (uintptr)t->max;
	for(j=0; j<t->max; j++) {
		if(t->fkey[i] == nil) {
			t->nkey++;
			goto ret;
		}
		if(t->fkey[i] == (void*)-1) {
			t->ndead--;
			goto ret;
		}
		if(++i == t->max)
			i = 0;
	}

	// cannot happen - table is known to be non-full
	runtime_throw("finalizer table inconsistent");

ret:
	t->fkey[i] = k;
	t->val[i].fn = fn;
	t->val[i].ft = ft;
}

static bool
lookfintab(Fintab *t, void *k, bool del, Fin *f)
{
	int32 i, j;

	if(t->max == 0)
		return false;
	i = (uintptr)k % (uintptr)t->max;
	for(j=0; j<t->max; j++) {
		if(t->fkey[i] == nil)
			return false;
		if(t->fkey[i] == k) {
			if(f)
				*f = t->val[i];
			if(del) {
				t->fkey[i] = (void*)-1;
				t->val[i].fn = nil;
				t->val[i].ft = nil;
				t->ndead++;
			}
			return true;
		}
		if(++i == t->max)
			i = 0;
	}

	// cannot happen - table is known to be non-full
	runtime_throw("finalizer table inconsistent");
	return false;
}

static void
resizefintab(Fintab *tab)
{
	Fintab newtab;
	void *k;
	int32 i;

	runtime_memclr((byte*)&newtab, sizeof newtab);
	newtab.max = tab->max;
	if(newtab.max == 0)
		newtab.max = 3*3*3;
	else if(tab->ndead < tab->nkey/2) {
		// grow table if not many dead values.
		// otherwise just rehash into table of same size.
		newtab.max *= 3;
	}
	
	newtab.fkey = runtime_mallocgc(newtab.max*sizeof newtab.fkey[0], FlagNoPointers, 0, 1);
	newtab.val = runtime_mallocgc(newtab.max*sizeof newtab.val[0], 0, 0, 1);
	
	for(i=0; i<tab->max; i++) {
		k = tab->fkey[i];
		if(k != nil && k != (void*)-1)
			addfintab(&newtab, k, tab->val[i].fn, tab->val[i].ft);
	}
	
	runtime_free(tab->fkey);
	runtime_free(tab->val);
	
	tab->fkey = newtab.fkey;
	tab->val = newtab.val;
	tab->nkey = newtab.nkey;
	tab->ndead = newtab.ndead;
	tab->max = newtab.max;
}

bool
runtime_addfinalizer(void *p, void (*f)(void*), const struct __go_func_type *ft)
{
	Fintab *tab;
	byte *base;
	
	if(debug) {
		if(!runtime_mlookup(p, &base, nil, nil) || p != base)
			runtime_throw("addfinalizer on invalid pointer");
	}
	
	tab = TAB(p);
	runtime_lock(tab);
	if(f == nil) {
		lookfintab(tab, p, true, nil);
		runtime_unlock(tab);
		return true;
	}

	if(lookfintab(tab, p, false, nil)) {
		runtime_unlock(tab);
		return false;
	}

	if(tab->nkey >= tab->max/2+tab->max/4) {
		// keep table at most 3/4 full:
		// allocate new table and rehash.
		resizefintab(tab);
	}

	addfintab(tab, p, f, ft);
	runtime_setblockspecial(p, true);
	runtime_unlock(tab);
	return true;
}

// get finalizer; if del, delete finalizer.
// caller is responsible for updating RefHasFinalizer (special) bit.
bool
runtime_getfinalizer(void *p, bool del, void (**fn)(void*), const struct __go_func_type **ft)
{
	Fintab *tab;
	bool res;
	Fin f;
	
	tab = TAB(p);
	runtime_lock(tab);
	res = lookfintab(tab, p, del, &f);
	runtime_unlock(tab);
	if(res==false)
		return false;
	*fn = f.fn;
	*ft = f.ft;
	return true;
}

void
runtime_walkfintab(void (*fn)(void*), void (*addroot)(Obj))
{
	void **key;
	void **ekey;
	int32 i;

	for(i=0; i<TABSZ; i++) {
		runtime_lock(&fintab[i]);
		key = fintab[i].fkey;
		ekey = key + fintab[i].max;
		for(; key < ekey; key++)
			if(*key != nil && *key != ((void*)-1))
				fn(*key);
		addroot((Obj){(byte*)&fintab[i].fkey, sizeof(void*), 0});
		addroot((Obj){(byte*)&fintab[i].val, sizeof(void*), 0});
		runtime_unlock(&fintab[i]);
	}
}

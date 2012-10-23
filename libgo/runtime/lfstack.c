// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Lock-free stack.

#include "runtime.h"
#include "arch.h"

#if __SIZEOF_POINTER__ == 8
// Amd64 uses 48-bit virtual addresses, 47-th bit is used as kernel/user flag.
// So we use 17msb of pointers as ABA counter.
# define PTR_BITS 47
#else
# define PTR_BITS 32
#endif
#define PTR_MASK ((1ull<<PTR_BITS)-1)

void
runtime_lfstackpush(uint64 *head, LFNode *node)
{
	uint64 old, new;

	if((uintptr)node != ((uintptr)node&PTR_MASK)) {
		runtime_printf("p=%p\n", node);
		runtime_throw("runtime_lfstackpush: invalid pointer");
	}

	node->pushcnt++;
	new = (uint64)(uintptr)node|(((uint64)node->pushcnt)<<PTR_BITS);
	old = runtime_atomicload64(head);
	for(;;) {
		node->next = (LFNode*)(uintptr)(old&PTR_MASK);
		if(runtime_cas64(head, &old, new))
			break;
	}
}

LFNode*
runtime_lfstackpop(uint64 *head)
{
	LFNode *node, *node2;
	uint64 old, new;

	old = runtime_atomicload64(head);
	for(;;) {
		if(old == 0)
			return nil;
		node = (LFNode*)(uintptr)(old&PTR_MASK);
		node2 = runtime_atomicloadp(&node->next);
		new = 0;
		if(node2 != nil)
			new = (uint64)(uintptr)node2|(((uint64)node2->pushcnt)<<PTR_BITS);
		if(runtime_cas64(head, &old, new))
			return node;
	}
}

LFNode* runtime_lfstackpop2(uint64*)
  asm("runtime.lfstackpop2");

LFNode*
runtime_lfstackpop2(uint64 *head)
{
	return runtime_lfstackpop(head);
}

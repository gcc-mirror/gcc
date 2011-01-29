#include <errno.h>

#include "runtime.h"
#include "malloc.h"

#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#else
#define USE_DEV_ZERO
#define MAP_ANON 0
#endif
#endif

#ifdef USE_DEV_ZERO
static int dev_zero = -1;
#endif

void*
runtime_SysAlloc(uintptr n)
{
	void *p;
	int fd = -1;

	mstats.sys += n;

#ifdef USE_DEV_ZERO
	if (dev_zero == -1) {
		dev_zero = open("/dev/zero", O_RDONLY);
		if (dev_zero < 0) {
			printf("open /dev/zero: errno=%d\n", errno);
			exit(2);
		}
	}
	fd = dev_zero;
#endif

	p = runtime_mmap(nil, n, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_ANON|MAP_PRIVATE, fd, 0);
	if (p == MAP_FAILED) {
		if(errno == EACCES) {
			printf("mmap: access denied\n");
			printf("If you're running SELinux, enable execmem for this process.\n");
		} else {
			printf("mmap: errno=%d\n", errno);
		}
		exit(2);
	}
	return p;
}

void
runtime_SysUnused(void *v, uintptr n)
{
	USED(v);
	USED(n);
	// TODO(rsc): call madvise MADV_DONTNEED
}

void
runtime_SysFree(void *v, uintptr n)
{
	mstats.sys -= n;
	runtime_munmap(v, n);
}

void
runtime_SysMemInit(void)
{
	// Code generators assume that references to addresses
	// on the first page will fault.  Map the page explicitly with
	// no permissions, to head off possible bugs like the system
	// allocating that page as the virtual address space fills.
	// Ignore any error, since other systems might be smart
	// enough to never allow anything there.
	runtime_mmap(nil, 4096, PROT_NONE, MAP_FIXED|MAP_ANON|MAP_PRIVATE, -1, 0);
}

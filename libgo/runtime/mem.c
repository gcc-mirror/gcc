/* Defining _XOPEN_SOURCE hides the declaration of madvise() on Solaris <
   11 and the MADV_DONTNEED definition on IRIX 6.5.  */
#undef _XOPEN_SOURCE

#include <errno.h>
#include <unistd.h>

#include "runtime.h"
#include "arch.h"
#include "malloc.h"

#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#else
#define USE_DEV_ZERO
#define MAP_ANON 0
#endif
#endif

#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif

#ifdef USE_DEV_ZERO
static int dev_zero = -1;
#endif

static _Bool
addrspace_free(void *v __attribute__ ((unused)), uintptr n __attribute__ ((unused)))
{
#ifdef HAVE_MINCORE
	size_t page_size = getpagesize();
	size_t off;
	char one_byte;

	errno = 0;
	for(off = 0; off < n; off += page_size)
		if(mincore((char *)v + off, page_size, (void *)&one_byte) != -1
		   || errno != ENOMEM)
			return 0;
#endif
	return 1;
}

static void *
mmap_fixed(byte *v, uintptr n, int32 prot, int32 flags, int32 fd, uint32 offset)
{
	void *p;

	p = runtime_mmap((void *)v, n, prot, flags, fd, offset);
	if(p != v && addrspace_free(v, n)) {
		// On some systems, mmap ignores v without
		// MAP_FIXED, so retry if the address space is free.
		if(p != MAP_FAILED)
			runtime_munmap(p, n);
		p = runtime_mmap((void *)v, n, prot, flags|MAP_FIXED, fd, offset);
	}
	return p;
}

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
			runtime_printf("open /dev/zero: errno=%d\n", errno);
			exit(2);
		}
	}
	fd = dev_zero;
#endif

	p = runtime_mmap(nil, n, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_ANON|MAP_PRIVATE, fd, 0);
	if (p == MAP_FAILED) {
		if(errno == EACCES) {
			runtime_printf("runtime: mmap: access denied\n");
			runtime_printf("if you're running SELinux, enable execmem for this process.\n");
			exit(2);
		}
		if(errno == EAGAIN) {
			runtime_printf("runtime: mmap: too much locked memory (check 'ulimit -l').\n");
			runtime_exit(2);
		}
		return nil;
	}
	return p;
}

void
runtime_SysUnused(void *v __attribute__ ((unused)), uintptr n __attribute__ ((unused)))
{
#ifdef MADV_DONTNEED
	runtime_madvise(v, n, MADV_DONTNEED);
#endif
}

void
runtime_SysFree(void *v, uintptr n)
{
	mstats.sys -= n;
	runtime_munmap(v, n);
}

void*
runtime_SysReserve(void *v, uintptr n)
{
	int fd = -1;
	void *p;

#ifdef USE_DEV_ZERO
	if (dev_zero == -1) {
		dev_zero = open("/dev/zero", O_RDONLY);
		if (dev_zero < 0) {
			runtime_printf("open /dev/zero: errno=%d\n", errno);
			exit(2);
		}
	}
	fd = dev_zero;
#endif

	// On 64-bit, people with ulimit -v set complain if we reserve too
	// much address space.  Instead, assume that the reservation is okay
	// if we can reserve at least 64K and check the assumption in SysMap.
	// Only user-mode Linux (UML) rejects these requests.
	if(sizeof(void*) == 8 && (uintptr)v >= 0xffffffffU) {
		p = mmap_fixed(v, 64<<10, PROT_NONE, MAP_ANON|MAP_PRIVATE, fd, 0);
		if (p != v)
			return nil;
		runtime_munmap(p, 64<<10);
		return v;
	}
	
	// Use the MAP_NORESERVE mmap() flag here because typically most of
	// this reservation will never be used. It does not make sense
	// reserve a huge amount of unneeded swap space. This is important on
	// systems which do not overcommit memory by default.
	p = runtime_mmap(v, n, PROT_NONE, MAP_ANON|MAP_PRIVATE|MAP_NORESERVE, fd, 0);
	if(p == MAP_FAILED)
		return nil;
	return p;
}

void
runtime_SysMap(void *v, uintptr n)
{
	void *p;
	int fd = -1;
	
	mstats.sys += n;

#ifdef USE_DEV_ZERO
	if (dev_zero == -1) {
		dev_zero = open("/dev/zero", O_RDONLY);
		if (dev_zero < 0) {
			runtime_printf("open /dev/zero: errno=%d\n", errno);
			exit(2);
		}
	}
	fd = dev_zero;
#endif

	// On 64-bit, we don't actually have v reserved, so tread carefully.
	if(sizeof(void*) == 8 && (uintptr)v >= 0xffffffffU) {
		p = mmap_fixed(v, n, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_ANON|MAP_PRIVATE, fd, 0);
		if(p == MAP_FAILED && errno == ENOMEM)
			runtime_throw("runtime: out of memory");
		if(p != v) {
			runtime_printf("runtime: address space conflict: map(%p) = %p\n", v, p);
			runtime_throw("runtime: address space conflict");
		}
		return;
	}

	p = runtime_mmap(v, n, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_ANON|MAP_FIXED|MAP_PRIVATE, fd, 0);
	if(p != v)
		runtime_throw("runtime: cannot map pages in arena address space");
}

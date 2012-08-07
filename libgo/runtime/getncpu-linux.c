// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <string.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

#include "runtime.h"
#include "defs.h"

#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif

int32
getproccount(void)
{
	int32 fd, rd, cnt, cpustrlen;
	const char *cpustr;
	const byte *pos;
	byte *bufpos;
	byte buf[256];

	fd = open("/proc/stat", O_RDONLY|O_CLOEXEC, 0);
	if(fd == -1)
		return 1;
	cnt = 0;
	bufpos = buf;
	cpustr = "\ncpu";
	cpustrlen = strlen(cpustr);
	for(;;) {
		rd = read(fd, bufpos, sizeof(buf)-cpustrlen);
		if(rd == -1)
			break;
		bufpos[rd] = 0;
		for(pos=buf; (pos=(const byte*)strstr((const char*)pos, cpustr)) != nil; cnt++, pos++) {
		}
		if(rd < cpustrlen)
			break;
		memmove(buf, bufpos+rd-cpustrlen+1, cpustrlen-1);
		bufpos = buf+cpustrlen-1;
	}
	close(fd);
	return cnt ? cnt : 1;
}

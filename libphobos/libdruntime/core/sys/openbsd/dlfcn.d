/**
 * D header file for OpenBSD.
 *
 * $(LINK2 https://cvsweb.openbsd.org/cgi-bin/cvsweb/~checkout~/src/include/dlfcn.h?rev=1.15&content-type=text/plain, dlfcn.h)
 */
module core.sys.openbsd.dlfcn;

public import core.sys.posix.dlfcn;

version (OpenBSD):
extern (C):
nothrow:

enum RTLD_NEXT    = cast(void *)-1;
enum RTLD_DEFAULT = cast(void *)-2;
enum RTLD_SELF    = cast(void *)-3;

enum DL_GETERRNO     = 1;
enum DL_SETTHREADLCK = 2;
enum DL_SETBINDLCK   = 3;
enum DL_REFERENCE    = 4;

enum DL_LAZY         = RTLD_LAZY;

int dlctl(void *, int, void *);

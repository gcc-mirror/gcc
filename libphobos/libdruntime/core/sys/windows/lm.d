/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_lm.d)
 */
module core.sys.windows.lm;
version (Windows):

/* removed - now supporting only Win2k up
version (WindowsVista) {
    version = WIN32_WINNT_ONLY;
} else version (Windows2003) {
    version = WIN32_WINNT_ONLY;
} else version (WindowsXP) {
    version = WIN32_WINNT_ONLY;
} else version (WindowsNTonly) {
    version = WIN32_WINNT_ONLY;
}
*/
public import core.sys.windows.lmcons;
public import core.sys.windows.lmaccess;
public import core.sys.windows.lmalert;
public import core.sys.windows.lmat;
public import core.sys.windows.lmerr;
public import core.sys.windows.lmshare;
public import core.sys.windows.lmapibuf;
public import core.sys.windows.lmremutl;
public import core.sys.windows.lmrepl;
public import core.sys.windows.lmuse;
public import core.sys.windows.lmstats;
public import core.sys.windows.lmwksta;
public import core.sys.windows.lmserver;

version (Windows2000) {
} else {
    public import core.sys.windows.lmmsg;
}

// FIXME: Everything in these next files seems to be deprecated!
import core.sys.windows.lmaudit;
import core.sys.windows.lmchdev; // can't find many docs for functions from this file.
import core.sys.windows.lmconfig;
import core.sys.windows.lmerrlog;
import core.sys.windows.lmsvc;
import core.sys.windows.lmsname; // in MinGW, this was publicly included by lm.lmsvc

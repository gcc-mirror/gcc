/**
 * Windows API header module
 *
 * Translated from MinGW API for MS-Windows 3.10
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_exdispid.d)
 */
module core.sys.windows.exdispid;
version (Windows):

enum : int {
    DISPID_STATUSTEXTCHANGE = 102,
    DISPID_PROGRESSCHANGE   = 108,
    DISPID_TITLECHANGE      = 113,
    DISPID_BEFORENAVIGATE2  = 250,
    DISPID_NEWWINDOW2       = 251,
    DISPID_DOCUMENTCOMPLETE = 259
}

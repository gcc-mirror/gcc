/**
 * Windows API header module
 *
 * Translated from MinGW API for MS-Windows 3.10
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_idispids.d)
 */
module core.sys.windows.idispids;
version (Windows):

enum : int {
    DISPID_AMBIENT_OFFLINEIFNOTCONNECTED = -5501,
    DISPID_AMBIENT_SILENT                = -5502
}

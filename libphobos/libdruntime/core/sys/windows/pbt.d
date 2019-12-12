/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_pbt.d)
 */
module core.sys.windows.pbt;
version (Windows):

private import core.sys.windows.windef;

enum : WPARAM {
    PBT_APMQUERYSUSPEND,
    PBT_APMQUERYSTANDBY,
    PBT_APMQUERYSUSPENDFAILED,
    PBT_APMQUERYSTANDBYFAILED,
    PBT_APMSUSPEND,
    PBT_APMSTANDBY,
    PBT_APMRESUMECRITICAL,
    PBT_APMRESUMESUSPEND,
    PBT_APMRESUMESTANDBY,
    PBT_APMBATTERYLOW,
    PBT_APMPOWERSTATUSCHANGE,
    PBT_APMOEMEVENT // = 11
}

enum LPARAM PBTF_APMRESUMEFROMFAILURE = 1;

/**
 * Windows API header module
 *
 * Translated from Windows SDK API
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/sdkddkver.d)
 */
module core.sys.windows.sdkddkver;

version (Windows):
@system:

enum _WIN32_WINNT_NT4                    = 0x0400;
enum _WIN32_WINNT_WIN2K                  = 0x0500;
enum _WIN32_WINNT_WINXP                  = 0x0501;
enum _WIN32_WINNT_WS03                   = 0x0502;
enum _WIN32_WINNT_WIN6                   = 0x0600;
enum _WIN32_WINNT_VISTA                  = 0x0600;
enum _WIN32_WINNT_WS08                   = 0x0600;
enum _WIN32_WINNT_LONGHORN               = 0x0600;
enum _WIN32_WINNT_WIN7                   = 0x0601;
enum _WIN32_WINNT_WIN8                   = 0x0602;
enum _WIN32_WINNT_WINBLUE                = 0x0603;
enum _WIN32_WINNT_WIN10                  = 0x0A00;

enum _WIN32_IE_IE20                      = 0x0200;
enum _WIN32_IE_IE30                      = 0x0300;
enum _WIN32_IE_IE302                     = 0x0302;
enum _WIN32_IE_IE40                      = 0x0400;
enum _WIN32_IE_IE401                     = 0x0401;
enum _WIN32_IE_IE50                      = 0x0500;
enum _WIN32_IE_IE501                     = 0x0501;
enum _WIN32_IE_IE55                      = 0x0550;
enum _WIN32_IE_IE60                      = 0x0600;
enum _WIN32_IE_IE60SP1                   = 0x0601;
enum _WIN32_IE_IE60SP2                   = 0x0603;
enum _WIN32_IE_IE70                      = 0x0700;
enum _WIN32_IE_IE80                      = 0x0800;
enum _WIN32_IE_IE90                      = 0x0900;
enum _WIN32_IE_IE100                     = 0x0A00;

enum _WIN32_IE_NT4                    =  _WIN32_IE_IE20;
enum _WIN32_IE_NT4SP1                 =  _WIN32_IE_IE20;
enum _WIN32_IE_NT4SP2                 =  _WIN32_IE_IE20;
enum _WIN32_IE_NT4SP3                 =  _WIN32_IE_IE302;
enum _WIN32_IE_NT4SP4                 =  _WIN32_IE_IE401;
enum _WIN32_IE_NT4SP5                 =  _WIN32_IE_IE401;
enum _WIN32_IE_NT4SP6                 =  _WIN32_IE_IE50;
enum _WIN32_IE_WIN98                  =  _WIN32_IE_IE401;
enum _WIN32_IE_WIN98SE                =  _WIN32_IE_IE50;
enum _WIN32_IE_WINME                  =  _WIN32_IE_IE55;
enum _WIN32_IE_WIN2K                  =  _WIN32_IE_IE501;
enum _WIN32_IE_WIN2KSP1               =  _WIN32_IE_IE501;
enum _WIN32_IE_WIN2KSP2               =  _WIN32_IE_IE501;
enum _WIN32_IE_WIN2KSP3               =  _WIN32_IE_IE501;
enum _WIN32_IE_WIN2KSP4               =  _WIN32_IE_IE501;
enum _WIN32_IE_XP                     =  _WIN32_IE_IE60;
enum _WIN32_IE_XPSP1                  =  _WIN32_IE_IE60SP1;
enum _WIN32_IE_XPSP2                  =  _WIN32_IE_IE60SP2;
enum _WIN32_IE_WS03                   =  0x0602;
enum _WIN32_IE_WS03SP1                =  _WIN32_IE_IE60SP2;
enum _WIN32_IE_WIN6                   =  _WIN32_IE_IE70;
enum _WIN32_IE_LONGHORN               =  _WIN32_IE_IE70;
enum _WIN32_IE_WIN7                   =  _WIN32_IE_IE80;
enum _WIN32_IE_WIN8                   =  _WIN32_IE_IE100;
enum _WIN32_IE_WINBLUE                =  _WIN32_IE_IE100;


enum NTDDI_WIN2K                         = 0x05000000;
enum NTDDI_WIN2KSP1                      = 0x05000100;
enum NTDDI_WIN2KSP2                      = 0x05000200;
enum NTDDI_WIN2KSP3                      = 0x05000300;
enum NTDDI_WIN2KSP4                      = 0x05000400;

enum NTDDI_WINXP                         = 0x05010000;
enum NTDDI_WINXPSP1                      = 0x05010100;
enum NTDDI_WINXPSP2                      = 0x05010200;
enum NTDDI_WINXPSP3                      = 0x05010300;
enum NTDDI_WINXPSP4                      = 0x05010400;

enum NTDDI_WS03                          = 0x05020000;
enum NTDDI_WS03SP1                       = 0x05020100;
enum NTDDI_WS03SP2                       = 0x05020200;
enum NTDDI_WS03SP3                       = 0x05020300;
enum NTDDI_WS03SP4                       = 0x05020400;

enum NTDDI_WIN6                          = 0x06000000;
enum NTDDI_WIN6SP1                       = 0x06000100;
enum NTDDI_WIN6SP2                       = 0x06000200;
enum NTDDI_WIN6SP3                       = 0x06000300;
enum NTDDI_WIN6SP4                       = 0x06000400;

enum NTDDI_VISTA                       = NTDDI_WIN6;
enum NTDDI_VISTASP1                    = NTDDI_WIN6SP1;
enum NTDDI_VISTASP2                    = NTDDI_WIN6SP2;
enum NTDDI_VISTASP3                    = NTDDI_WIN6SP3;
enum NTDDI_VISTASP4                    = NTDDI_WIN6SP4;

enum NTDDI_LONGHORN                    = NTDDI_VISTA;

enum NTDDI_WS08                        = NTDDI_WIN6SP1;
enum NTDDI_WS08SP2                     = NTDDI_WIN6SP2;
enum NTDDI_WS08SP3                     = NTDDI_WIN6SP3;
enum NTDDI_WS08SP4                     = NTDDI_WIN6SP4;

enum NTDDI_WIN7                          = 0x06010000;
enum NTDDI_WIN8                          = 0x06020000;
enum NTDDI_WINBLUE                       = 0x06030000;

enum OSVERSION_MASK      = 0xFFFF0000;
enum SPVERSION_MASK      = 0x0000FF00;
enum SUBVERSION_MASK     = 0x000000FF;

enum _WIN32_WINNT    = 0x0603;

enum NTDDI_VERSION   = 0x06030000;
enum WINVER          =  _WIN32_WINNT;

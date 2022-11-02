/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_msacm.d)
 */
module core.sys.windows.msacm;
version (Windows):

version (ANSI) {} else version = Unicode;

import core.sys.windows.basetsd, core.sys.windows.mmsystem, core.sys.windows.windef;

mixin DECLARE_HANDLE!("HACMDRIVERID");
mixin DECLARE_HANDLE!("HACMDRIVER");
alias HACMDRIVER* LPHACMDRIVER;

enum size_t
    ACMDRIVERDETAILS_SHORTNAME_CHARS =  32,
    ACMDRIVERDETAILS_LONGNAME_CHARS  = 128,
    ACMDRIVERDETAILS_COPYRIGHT_CHARS =  80,
    ACMDRIVERDETAILS_LICENSING_CHARS = 128,
    ACMDRIVERDETAILS_FEATURES_CHARS  = 512;

enum size_t
    ACMFORMATDETAILS_FORMAT_CHARS       = 128,
    ACMFORMATTAGDETAILS_FORMATTAG_CHARS = 48;

align(1):

struct ACMFORMATDETAILSA {
    DWORD          cbStruct = ACMFORMATDETAILSA.sizeof;
    DWORD          dwFormatIndex;
    DWORD          dwFormatTag;
    DWORD          fdwSupport;
    LPWAVEFORMATEX pwfx;
    DWORD          cbwfx;
    char[ACMFORMATDETAILS_FORMAT_CHARS] szFormat = 0;
}
alias ACMFORMATDETAILSA* LPACMFORMATDETAILSA;

struct ACMFORMATDETAILSW {
    DWORD          cbStruct = ACMFORMATDETAILSW.sizeof;
    DWORD          dwFormatIndex;
    DWORD          dwFormatTag;
    DWORD          fdwSupport;
    LPWAVEFORMATEX pwfx;
    DWORD          cbwfx;
    WCHAR[ACMFORMATDETAILS_FORMAT_CHARS] szFormat = 0;
}
alias ACMFORMATDETAILSW* LPACMFORMATDETAILSW;

struct ACMFORMATTAGDETAILSA {
    DWORD cbStruct = ACMFORMATTAGDETAILSA.sizeof;
    DWORD dwFormatTagIndex;
    DWORD dwFormatTag;
    DWORD cbFormatSize;
    DWORD fdwSupport;
    DWORD cStandardFormats;
    char[ACMFORMATTAGDETAILS_FORMATTAG_CHARS] szFormatTag = 0;
}
alias ACMFORMATTAGDETAILSA* LPACMFORMATTAGDETAILSA;

struct ACMFORMATTAGDETAILSW {
    DWORD cbStruct = ACMFORMATTAGDETAILSW.sizeof;
    DWORD dwFormatTagIndex;
    DWORD dwFormatTag;
    DWORD cbFormatSize;
    DWORD fdwSupport;
    DWORD cStandardFormats;
    WCHAR[ACMFORMATTAGDETAILS_FORMATTAG_CHARS] szFormatTag = 0;
}
alias ACMFORMATTAGDETAILSW* LPACMFORMATTAGDETAILSW;

struct ACMDRIVERDETAILSA {
align(1):
    DWORD  cbStruct = ACMDRIVERDETAILSA.sizeof;
    FOURCC fccType;
    FOURCC fccComp;
    WORD   wMid;
    WORD   wPid;
    DWORD  vdwACM;
    DWORD  vdwDriver;
    DWORD  fdwSupport;
    DWORD  cFormatTags;
    DWORD  cFilterTags;
    HICON  hicon;
    char[ACMDRIVERDETAILS_SHORTNAME_CHARS] szShortName = 0;
    char[ACMDRIVERDETAILS_LONGNAME_CHARS]  szLongName = 0;
    char[ACMDRIVERDETAILS_COPYRIGHT_CHARS] szCopyright = 0;
    char[ACMDRIVERDETAILS_LICENSING_CHARS] szLicensing = 0;
    char[ACMDRIVERDETAILS_FEATURES_CHARS]  szFeatures = 0;
}
alias ACMDRIVERDETAILSA* LPACMDRIVERDETAILSA;

struct ACMDRIVERDETAILSW {
align(1):
    DWORD  cbStruct = ACMDRIVERDETAILSW.sizeof;
    FOURCC fccType;
    FOURCC fccComp;
    WORD   wMid;
    WORD   wPid;
    DWORD  vdwACM;
    DWORD  vdwDriver;
    DWORD  fdwSupport;
    DWORD  cFormatTags;
    DWORD  cFilterTags;
    HICON  hicon;
    WCHAR[ACMDRIVERDETAILS_SHORTNAME_CHARS] szShortName = 0;
    WCHAR[ACMDRIVERDETAILS_LONGNAME_CHARS]  szLongName = 0;
    WCHAR[ACMDRIVERDETAILS_COPYRIGHT_CHARS] szCopyright = 0;
    WCHAR[ACMDRIVERDETAILS_LICENSING_CHARS] szLicensing = 0;
    WCHAR[ACMDRIVERDETAILS_FEATURES_CHARS]  szFeatures = 0;
}
alias ACMDRIVERDETAILSW* LPACMDRIVERDETAILSW;

extern (Windows) {
    alias BOOL function(HACMDRIVERID hadid, LPACMFORMATDETAILSA pafd,
      DWORD_PTR dwInstance, DWORD fdwSupport) ACMFORMATENUMCBA;
    alias BOOL function(HACMDRIVERID hadid, LPACMFORMATDETAILSW pafd,
      DWORD_PTR dwInstance, DWORD fdwSupport) ACMFORMATENUMCBW;
    alias BOOL function(HACMDRIVERID hadid, LPACMFORMATTAGDETAILSA paftd,
      DWORD_PTR dwInstance, DWORD fdwSupport) ACMFORMATTAGENUMCBA;
    alias BOOL function(HACMDRIVERID hadid, LPACMFORMATTAGDETAILSW paftd,
      DWORD_PTR dwInstance, DWORD fdwSupport) ACMFORMATTAGENUMCBW;
    alias BOOL function(HACMDRIVERID hadid, DWORD_PTR dwInstance,
      DWORD fdwSupport) ACMDRIVERENUMCB;

    MMRESULT acmDriverOpen(LPHACMDRIVER phad, HACMDRIVERID hadid,
      DWORD fdwOpen);
    MMRESULT acmDriverEnum(ACMDRIVERENUMCB fnCallback, DWORD_PTR dwInstance,
      DWORD fdwEnum);
    MMRESULT acmFormatEnumA(HACMDRIVER had, LPACMFORMATDETAILSA pafd,
      ACMFORMATENUMCBA fnCallback, DWORD_PTR dwInstance, DWORD fdwEnum);
    MMRESULT acmFormatEnumW(HACMDRIVER had, LPACMFORMATDETAILSW pafd,
      ACMFORMATENUMCBW fnCallback, DWORD_PTR dwInstance, DWORD fdwEnum);
    MMRESULT acmDriverClose(HACMDRIVER had, DWORD fdwClose);
    MMRESULT acmDriverDetailsA(HACMDRIVERID hadid, LPACMDRIVERDETAILSA padd,
      DWORD fdwDetails);
    MMRESULT acmDriverDetailsW(HACMDRIVERID hadid, LPACMDRIVERDETAILSW padd,
      DWORD fdwDetails);
    MMRESULT acmFormatTagEnumA(HACMDRIVER had, LPACMFORMATTAGDETAILSA paftd,
      ACMFORMATTAGENUMCBA fnCallback, DWORD_PTR dwInstance, DWORD fdwEnum);
    MMRESULT acmFormatTagEnumW(HACMDRIVER had, LPACMFORMATTAGDETAILSW paftd,
      ACMFORMATTAGENUMCBW fnCallback, DWORD_PTR dwInstance, DWORD fdwEnum);
}

version (Unicode) {
    alias ACMFORMATDETAILSW ACMFORMATDETAILS;
    alias ACMFORMATTAGDETAILSW ACMFORMATTAGDETAILS;
    alias ACMDRIVERDETAILSW ACMDRIVERDETAILS;
    alias ACMFORMATENUMCBW ACMFORMATENUMCB;
    alias ACMFORMATTAGENUMCBW ACMFORMATTAGENUMCB;
    alias acmFormatEnumW acmFormatEnum;
    alias acmDriverDetailsW acmDriverDetails;
    alias acmFormatTagEnumW acmFormatTagEnum;
} else {
    alias ACMFORMATDETAILSA ACMFORMATDETAILS;
    alias ACMFORMATTAGDETAILSA ACMFORMATTAGDETAILS;
    alias ACMDRIVERDETAILSA ACMDRIVERDETAILS;
    alias ACMFORMATENUMCBA ACMFORMATENUMCB;
    alias ACMFORMATTAGENUMCBA ACMFORMATTAGENUMCB;
    alias acmFormatEnumA acmFormatEnum;
    alias acmDriverDetailsA acmDriverDetails;
    alias acmFormatTagEnumA acmFormatTagEnum;
}

alias ACMFORMATDETAILS* LPACMFORMATDETAILS;
alias ACMFORMATTAGDETAILS* LPACMFORMATTAGDETAILS;
alias ACMDRIVERDETAILS* LPACMDRIVERDETAILS;

/**
 * Windows API header module
 *
 * written in the D programming language
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_vfw.d)
 */

module core.sys.windows.vfw;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "vfw32");

private import
    core.sys.windows.commdlg,
    core.sys.windows.wingdi,
    core.sys.windows.mmsystem,
    core.sys.windows.unknwn,
    core.sys.windows.w32api,
    core.sys.windows.windef,
    core.sys.windows.winuser;

extern(Windows) {
    DWORD VideoForWindowsVersion();
    LONG InitVFW();
    LONG TermVFW();
}

DWORD MKFOURCC(char ch0, char ch1, char ch2, char ch3) {
    return (cast(DWORD)ch0) | ((cast(DWORD)ch1) << 8) | ((cast(DWORD)ch2) << 16) | ((cast(DWORD)ch3) << 24);
}

/**
 * COMPMAN - Installable Compression Manager.
 */

enum ICVERSION = 0x0104;

alias TypeDef!(HANDLE) HIC;

enum BI_1632 = 0x32333631;

template aviTWOCC(char c0, char c1) {
enum WORD aviTWOCC = c0 | (c1 << 8);
}

enum ICTYPE_VIDEO  = mmioFOURCC!('v', 'i', 'd', 'c');
enum ICTYPE_AUDIO  = mmioFOURCC!('a', 'u', 'd', 'c');

enum {
    ICERR_OK            = 0,
    ICERR_DONTDRAW      = 1,
    ICERR_NEWPALETTE    = 2,
    ICERR_GOTOKEYFRAME  = 3,
    ICERR_STOPDRAWING   = 4,
}

enum ICERR_UNSUPPORTED = -1;
enum ICERR_BADFORMAT   = -2;
enum ICERR_MEMORY      = -3;
enum ICERR_INTERNAL    = -4;
enum ICERR_BADFLAGS    = -5;
enum ICERR_BADPARAM    = -6;
enum ICERR_BADSIZE     = -7;
enum ICERR_BADHANDLE   = -8;
enum ICERR_CANTUPDATE  = -9;
enum ICERR_ABORT       = -10;
enum ICERR_ERROR       = -100;
enum ICERR_BADBITDEPTH = -200;
enum ICERR_BADIMAGESIZE = -201;

enum ICERR_CUSTOM = -400;

enum {
    ICMODE_COMPRESS         = 1,
    ICMODE_DECOMPRESS,
    ICMODE_FASTDECOMPRESS,
    ICMODE_QUERY,
    ICMODE_FASTCOMPRESS,
    ICMODE_DRAW             = 8,
}

enum ICMODE_INTERNALF_FUNCTION32   = 0x8000;
enum ICMODE_INTERNALF_MASK         = 0x8000;

enum {
    AVIIF_LIST      = 0x00000001,
    AVIIF_TWOCC     = 0x00000002,
    AVIIF_KEYFRAME  = 0x00000010,
}

enum ICQUALITY_LOW     = 0;
enum ICQUALITY_HIGH    = 10000;
enum ICQUALITY_DEFAULT = -1;

enum {
    ICM_USER            = DRV_USER + 0x0000,
    ICM_RESERVED_LOW    = DRV_USER + 0x1000,
    ICM_RESERVED_HIGH   = DRV_USER + 0x2000,
    ICM_RESERVED        = ICM_RESERVED_LOW,
}

// messages

enum {
    ICM_GETSTATE            = ICM_RESERVED + 0,
    ICM_SETSTATE            = ICM_RESERVED + 1,
    ICM_GETINFO             = ICM_RESERVED + 2,
    ICM_CONFIGURE           = ICM_RESERVED + 10,
    ICM_ABOUT               = ICM_RESERVED + 11,
    ICM_GETERRORTEXT        = ICM_RESERVED + 12,
    ICM_GETFORMATNAME       = ICM_RESERVED + 20,
    ICM_ENUMFORMATS         = ICM_RESERVED + 21,
    ICM_GETDEFAULTQUALITY   = ICM_RESERVED + 30,
    ICM_GETQUALITY          = ICM_RESERVED + 31,
    ICM_SETQUALITY          = ICM_RESERVED + 32,
    ICM_SET                 = ICM_RESERVED + 40,
    ICM_GET                 = ICM_RESERVED + 41,
}

enum ICM_FRAMERATE     = mmioFOURCC!('F','r','m','R');
enum ICM_KEYFRAMERATE  = mmioFOURCC!('K','e','y','R');

// ICM specific messages.

enum {
    ICM_COMPRESS_GET_FORMAT     = ICM_USER + 4,
    ICM_COMPRESS_GET_SIZE       = ICM_USER + 5,
    ICM_COMPRESS_QUERY          = ICM_USER + 6,
    ICM_COMPRESS_BEGIN          = ICM_USER + 7,
    ICM_COMPRESS                = ICM_USER + 8,
    ICM_COMPRESS_END            = ICM_USER + 9,
    ICM_DECOMPRESS_GET_FORMAT   = ICM_USER + 10,
    ICM_DECOMPRESS_QUERY        = ICM_USER + 11,
    ICM_DECOMPRESS_BEGIN        = ICM_USER + 12,
    ICM_DECOMPRESS              = ICM_USER + 13,
    ICM_DECOMPRESS_END          = ICM_USER + 14,
    ICM_DECOMPRESS_SET_PALETTE  = ICM_USER + 29,
    ICM_DECOMPRESS_GET_PALETTE  = ICM_USER + 30,
    ICM_DRAW_QUERY              = ICM_USER + 31,
    ICM_DRAW_BEGIN              = ICM_USER + 15,
    ICM_DRAW_GET_PALETTE        = ICM_USER + 16,
    ICM_DRAW_UPDATE             = ICM_USER + 17,
    ICM_DRAW_START              = ICM_USER + 18,
    ICM_DRAW_STOP               = ICM_USER + 19,
    ICM_DRAW_BITS               = ICM_USER + 20,
    ICM_DRAW_END                = ICM_USER + 21,
    ICM_DRAW_GETTIME            = ICM_USER + 32,
    ICM_DRAW                    = ICM_USER + 33,
    ICM_DRAW_WINDOW             = ICM_USER + 34,
    ICM_DRAW_SETTIME            = ICM_USER + 35,
    ICM_DRAW_REALIZE            = ICM_USER + 36,
    ICM_DRAW_FLUSH              = ICM_USER + 37,
    ICM_DRAW_RENDERBUFFER       = ICM_USER + 38,
    ICM_DRAW_START_PLAY         = ICM_USER + 39,
    ICM_DRAW_STOP_PLAY          = ICM_USER + 40,
    ICM_DRAW_SUGGESTFORMAT      = ICM_USER + 50,
    ICM_DRAW_CHANGEPALETTE      = ICM_USER + 51,
    ICM_DRAW_IDLE               = ICM_USER + 52,
    ICM_GETBUFFERSWANTED        = ICM_USER + 41,
    ICM_GETDEFAULTKEYFRAMERATE  = ICM_USER + 42,
    ICM_DECOMPRESSEX_BEGIN      = ICM_USER + 60,
    ICM_DECOMPRESSEX_QUERY      = ICM_USER + 61,
    ICM_DECOMPRESSEX            = ICM_USER + 62,
    ICM_DECOMPRESSEX_END        = ICM_USER + 63,
    ICM_COMPRESS_FRAMES_INFO    = ICM_USER + 70,
    ICM_COMPRESS_FRAMES         = ICM_USER + 71,
    ICM_SET_STATUS_PROC         = ICM_USER + 72,
}

struct ICOPEN {
    DWORD   dwSize;
    DWORD   fccType;
    DWORD   fccHandler;
    DWORD   dwVersion;
    DWORD   dwFlags;
    LRESULT dwError;
    LPVOID  pV1Reserved;
    LPVOID  pV2Reserved;
    DWORD   dnDevNode;
}

struct ICINFO {
    DWORD   dwSize;
    DWORD   fccType;
    DWORD   fccHandler;
    DWORD   dwFlags;
    DWORD   dwVersion;
    DWORD   dwVersionICM;
    WCHAR[16]   szName = 0;
    WCHAR[128]  szDescription = 0;
    WCHAR[128]  szDriver = 0;
}

enum {
    VIDCF_QUALITY           = 0x0001,
    VIDCF_CRUNCH            = 0x0002,
    VIDCF_TEMPORAL          = 0x0004,
    VIDCF_COMPRESSFRAMES    = 0x0008,
    VIDCF_DRAW              = 0x0010,
    VIDCF_FASTTEMPORALC     = 0x0020,
    VIDCF_FASTTEMPORALD     = 0x0080,
}

enum ICCOMPRESS_KEYFRAME = 0x00000001L;

struct ICCOMPRESS {
    DWORD               dwFlags;
    LPBITMAPINFOHEADER  lpbiOutput;
    LPVOID              lpOutput;
    LPBITMAPINFOHEADER  lpbiInput;
    LPVOID              lpInput;
    LPDWORD             lpckid;
    LPDWORD             lpdwFlags;
    LONG                lFrameNum;
    DWORD               dwFrameSize;
    DWORD               dwQuality;
    LPBITMAPINFOHEADER  lpbiPrev;
    LPVOID              lpPrev;
}

enum ICCOMPRESSFRAMES_PADDING = 0x00000001;

struct ICCOMPRESSFRAMES {
    DWORD               dwFlags;
    LPBITMAPINFOHEADER  lpbiOutput;
    LPARAM              lOutput;
    LPBITMAPINFOHEADER  lpbiInput;
    LPARAM              lInput;
    LONG                lStartFrame;
    LONG                lFrameCount;
    LONG                lQuality;
    LONG                lDataRate;
    LONG                lKeyRate;
    DWORD               dwRate;
    DWORD               dwScale;    DWORD       dwOverheadPerFrame;
    DWORD               dwReserved2;
extern (Windows):
    LONG function(LPARAM lInput, LONG lFrame, LPVOID lpBits, LONG len) GetData;
    LONG function(LPARAM lOutput, LONG lFrame, LPVOID lpBits, LONG len) PutData;
}

enum {
    ICSTATUS_START  = 0,
    ICSTATUS_STATUS = 1,
    ICSTATUS_END    = 2,
    ICSTATUS_ERROR  = 3,
    ICSTATUS_YIELD  = 4,
}

struct ICSETSTATUSPROC {
    DWORD   dwFlags;
    LPARAM  lParam;
extern (Windows)
    LONG function(LPARAM lParam, UINT message, LONG l) Status;
}

enum {
    ICDECOMPRESS_NOTKEYFRAME    = 0x08000000,
    ICDECOMPRESS_NULLFRAME      = 0x10000000,
    ICDECOMPRESS_PREROLL        = 0x20000000,
    ICDECOMPRESS_UPDATE         = 0x40000000,
    ICDECOMPRESS_HURRYUP        = 0x80000000,
}

struct ICDECOMPRESS {
    DWORD               dwFlags;
    LPBITMAPINFOHEADER  lpbiInput;
    LPVOID              lpInput;
    LPBITMAPINFOHEADER  lpbiOutput;
    LPVOID              lpOutput;
    DWORD               ckid;
}

struct ICDECOMPRESSEX {
    DWORD               dwFlags;
    LPBITMAPINFOHEADER  lpbiSrc;
    LPVOID              lpSrc;
    LPBITMAPINFOHEADER  lpbiDst;
    LPVOID              lpDst;
    int                 xDst;
    int                 yDst;
    int                 dxDst;
    int                 dyDst;
    int                 xSrc;
    int                 ySrc;
    int                 dxSrc;
    int                 dySrc;
}

enum {
    ICDRAW_QUERY        = 0x00000001,
    ICDRAW_FULLSCREEN   = 0x00000002,
    ICDRAW_HDC          = 0x00000004,
    ICDRAW_ANIMATE      = 0x00000008,
    ICDRAW_CONTINUE     = 0x00000010,
    ICDRAW_MEMORYDC     = 0x00000020,
    ICDRAW_UPDATING     = 0x00000040,
    ICDRAW_RENDER       = 0x00000080,
    ICDRAW_BUFFER       = 0x00000100,
}

struct ICDRAWBEGIN {
    DWORD               dwFlags;
    HPALETTE            hpal;
    HWND                hwnd;
    HDC                 hdc;
    int                 xDst;
    int                 yDst;
    int                 dxDst;
    int                 dyDst;
    LPBITMAPINFOHEADER  lpbi;
    int                 xSrc;
    int                 ySrc;
    int                 dxSrc;
    int                 dySrc;
    DWORD               dwRate;
    DWORD               dwScale;
}

enum {
    ICDRAW_NOTKEYFRAME  = 0x08000000,
    ICDRAW_NULLFRAME    = 0x10000000,
    ICDRAW_PREROLL      = 0x20000000,
    ICDRAW_UPDATE       = 0x40000000,
    ICDRAW_HURRYUP      = 0x80000000,
}

struct ICDRAW {
    DWORD           dwFlags;
    LPVOID          lpFormat;
    LPVOID          lpData;
    DWORD           cbData;
    LONG            lTime;
}

struct ICDRAWSUGGEST {
    LPBITMAPINFOHEADER  lpbiIn;
    LPBITMAPINFOHEADER  lpbiSuggest;
    int                 dxSrc;
    int                 dySrc;
    int                 dxDst;
    int                 dyDst;
    HIC                 hicDecompressor;
}

struct ICPALETTE {
    DWORD           dwFlags;
    int             iStart;
    int             iLen;
    LPPALETTEENTRY  lppe;
}


/**
 * ICM function declarations
 */

extern (Windows) {
    BOOL ICInfo(DWORD fccType, DWORD fccHandler, ICINFO *lpicinfo);
    BOOL ICInstall(DWORD fccType, DWORD fccHandler, LPARAM lParam, LPSTR szDesc, UINT wFlags);
    BOOL ICRemove(DWORD fccType, DWORD fccHandler, UINT wFlags);
    LRESULT ICGetInfo(HIC hic, ICINFO *picinfo, DWORD cb);
    HIC ICOpen(DWORD fccType, DWORD fccHandler, UINT wMode);
    HIC ICOpenFunction(DWORD fccType, DWORD fccHandler, UINT wMode, FARPROC lpfnHandler);
    LRESULT ICClose(HIC hic);
    LRESULT ICSendMessage(HIC hic, UINT msg, DWORD_PTR dw1, DWORD_PTR dw2);
}

enum {
    ICINSTALL_FUNCTION  = 0x0001,
    ICINSTALL_DRIVER    = 0x0002,
    ICINSTALL_HDRV      = 0x0004,
    ICINSTALL_UNICODE   = 0x8000,
    ICINSTALL_DRIVERW   = 0x8002,
}

// query macros

enum ICMF_CONFIGURE_QUERY  = 0x00000001;
enum ICMF_ABOUT_QUERY      = 0x00000001;

DWORD ICQueryAbout(HIC hic) {
    return ICSendMessage(hic, ICM_ABOUT, -1, ICMF_ABOUT_QUERY) == ICERR_OK;
}

DWORD ICAbout(HIC hic, HWND hwnd) {
    return cast(DWORD) ICSendMessage(hic, ICM_ABOUT, cast(DWORD_PTR) cast(UINT_PTR) hwnd, 0);
}

DWORD ICQueryConfigure(HIC hic) {
    return (ICSendMessage(hic, ICM_CONFIGURE, -1, ICMF_CONFIGURE_QUERY) == ICERR_OK);
}

DWORD ICConfigure(HIC hic, HWND hwnd) {
    return cast(DWORD) ICSendMessage(hic, ICM_CONFIGURE, cast(DWORD_PTR) cast(UINT_PTR) hwnd, 0);
}

DWORD ICGetState(HIC hic, LPVOID pv, DWORD_PTR cb) {
    return cast(DWORD) ICSendMessage(hic, ICM_GETSTATE, cast(DWORD_PTR) pv, cb);
}

DWORD ICSetState(HIC hic, LPVOID pv, DWORD_PTR cb) {
    return cast(DWORD) ICSendMessage(hic, ICM_SETSTATE, cast(DWORD_PTR) pv, cb);
}

DWORD ICGetStateSize(HIC hic) {
    return ICGetState(hic, null, 0);
}

DWORD dwICValue;

DWORD ICGetDefaultQuality(HIC hic) {
    ICSendMessage(hic, ICM_GETDEFAULTQUALITY, cast(DWORD_PTR)&dwICValue, DWORD.sizeof);
    return dwICValue;
}

DWORD ICGetDefaultKeyFrameRate(HIC hic) {
    ICSendMessage(hic, ICM_GETDEFAULTKEYFRAMERATE, cast(DWORD_PTR)&dwICValue, DWORD.sizeof);
    return dwICValue;
}

DWORD ICDrawWindow(HIC hic, LPVOID prc) {
    return cast(DWORD) ICSendMessage(hic, ICM_DRAW_WINDOW, cast(DWORD_PTR) prc, RECT.sizeof);
}

extern (Windows) {
    DWORD ICCompress(HIC hic, DWORD dwFlags, LPBITMAPINFOHEADER lpbiOutput, LPVOID lpData,
        LPBITMAPINFOHEADER lpbiInput, LPVOID lpBits, LPDWORD lpckid, LPDWORD lpdwFlags,
        LONG lFrameNum, DWORD dwFrameSize, DWORD dwQuality, LPBITMAPINFOHEADER lpbiPrev, LPVOID lpPrev);
}

LRESULT ICCompressBegin(HIC hic, LPVOID lpbiInput, LPVOID lpbiOutput) {
    return ICSendMessage(hic, ICM_COMPRESS_BEGIN, cast(DWORD_PTR)lpbiInput, cast(DWORD_PTR)lpbiOutput);
}
LRESULT ICCompressQuery(HIC hic, LPVOID lpbiInput, LPVOID lpbiOutput) {
    return ICSendMessage(hic, ICM_COMPRESS_QUERY, cast(DWORD_PTR)lpbiInput, cast(DWORD_PTR)lpbiOutput);
}
LRESULT ICCompressGetFormat(HIC hic, LPVOID lpbiInput, LPVOID lpbiOutput) {
    return ICSendMessage(hic, ICM_COMPRESS_GET_FORMAT, cast(DWORD_PTR)lpbiInput, cast(DWORD_PTR)lpbiOutput);
}
DWORD ICCompressGetFormatSize(HIC hic, LPVOID lpbi) {
    return cast(DWORD)ICCompressGetFormat(hic, lpbi, null);
}
DWORD ICCompressGetSize(HIC hic, LPVOID lpbiInput, LPVOID lpbiOutput) {
    return cast(DWORD)ICSendMessage(hic, ICM_COMPRESS_GET_SIZE, cast(DWORD_PTR)lpbiInput, cast(DWORD_PTR)lpbiOutput);
}
LRESULT ICCompressEnd(HIC hic) {
    return ICSendMessage(hic, ICM_COMPRESS_END, 0, 0);
}

extern (Windows) {
    DWORD ICDecompress(HIC hic, DWORD dwFlags, LPBITMAPINFOHEADER lpbiFormat, LPVOID lpData, LPBITMAPINFOHEADER lpbi, LPVOID lpBits);
}

LRESULT ICDecompressBegin(HIC hic, LPVOID lpbiInput, LPVOID lpbiOutput) {
    return ICSendMessage(hic, ICM_DECOMPRESS_BEGIN, cast(DWORD_PTR)lpbiInput, cast(DWORD_PTR)lpbiOutput);
}
LRESULT ICDecompressQuery(HIC hic, LPVOID lpbiInput, LPVOID lpbiOutput) {
    return ICSendMessage(hic, ICM_DECOMPRESS_QUERY, cast(DWORD_PTR)lpbiInput, cast(DWORD_PTR)lpbiOutput);
}
LONG ICDecompressGetFormat(HIC hic, LPVOID lpbiInput, LPVOID lpbiOutput) {
    return cast(LONG)ICSendMessage(hic, ICM_DECOMPRESS_GET_FORMAT, cast(DWORD_PTR)lpbiInput, cast(DWORD_PTR)lpbiOutput);
}
LONG ICDecompressGetFormatSize(HIC hic, LPVOID lpbi) {
    return ICDecompressGetFormat(hic, lpbi, null);
}
LRESULT ICDecompressGetPalette(HIC hic, LPVOID lpbiInput, LPVOID lpbiOutput) {
    return ICSendMessage(hic, ICM_DECOMPRESS_GET_PALETTE, cast(DWORD_PTR)lpbiInput, cast(DWORD_PTR)lpbiOutput);
}
LRESULT ICDecompressSetPalette(HIC hic, LPVOID lpbiPalette) {
    return ICSendMessage(hic, ICM_DECOMPRESS_SET_PALETTE, cast(DWORD_PTR)lpbiPalette, 0);
}
LRESULT ICDecompressEnd(HIC hic) {
    return ICSendMessage(hic, ICM_DECOMPRESS_END, 0, 0);
}

LRESULT ICDecompressEx(HIC hic, DWORD dwFlags, LPBITMAPINFOHEADER lpbiSrc,
    LPVOID lpSrc, int xSrc, int ySrc, int dxSrc, int dySrc, LPBITMAPINFOHEADER lpbiDst,
    LPVOID lpDst, int xDst, int yDst, int dxDst, int dyDst) {
    ICDECOMPRESSEX ic;

    ic.dwFlags = dwFlags;
    ic.lpbiSrc = lpbiSrc;
    ic.lpSrc = lpSrc;
    ic.xSrc = xSrc;
    ic.ySrc = ySrc;
    ic.dxSrc = dxSrc;
    ic.dySrc = dySrc;
    ic.lpbiDst = lpbiDst;
    ic.lpDst = lpDst;
    ic.xDst = xDst;
    ic.yDst = yDst;
    ic.dxDst = dxDst;
    ic.dyDst = dyDst;

    return ICSendMessage(hic, ICM_DECOMPRESSEX, cast(DWORD_PTR)&ic, ic.sizeof);
}

LRESULT ICDecompressExBegin(HIC hic, DWORD dwFlags, LPBITMAPINFOHEADER lpbiSrc,
    LPVOID lpSrc, int xSrc, int ySrc, int dxSrc, int dySrc, LPBITMAPINFOHEADER lpbiDst,
    LPVOID lpDst, int xDst, int yDst, int dxDst, int dyDst) {
    ICDECOMPRESSEX ic;

    ic.dwFlags = dwFlags;
    ic.lpbiSrc = lpbiSrc;
    ic.lpSrc = lpSrc;
    ic.xSrc = xSrc;
    ic.ySrc = ySrc;
    ic.dxSrc = dxSrc;
    ic.dySrc = dySrc;
    ic.lpbiDst = lpbiDst;
    ic.lpDst = lpDst;
    ic.xDst = xDst;
    ic.yDst = yDst;
    ic.dxDst = dxDst;
    ic.dyDst = dyDst;

    return ICSendMessage(hic, ICM_DECOMPRESSEX_BEGIN, cast(DWORD_PTR)&ic, ic.sizeof);
}

LRESULT ICDecompressExQuery(HIC hic, DWORD dwFlags, LPBITMAPINFOHEADER lpbiSrc,
    LPVOID lpSrc, int xSrc, int ySrc, int dxSrc, int dySrc, LPBITMAPINFOHEADER lpbiDst,
    LPVOID lpDst, int xDst, int yDst, int dxDst, int dyDst) {
    ICDECOMPRESSEX ic;

    ic.dwFlags = dwFlags;
    ic.lpbiSrc = lpbiSrc;
    ic.lpSrc = lpSrc;
    ic.xSrc = xSrc;
    ic.ySrc = ySrc;
    ic.dxSrc = dxSrc;
    ic.dySrc = dySrc;
    ic.lpbiDst = lpbiDst;
    ic.lpDst = lpDst;
    ic.xDst = xDst;
    ic.yDst = yDst;
    ic.dxDst = dxDst;
    ic.dyDst = dyDst;

    return ICSendMessage(hic, ICM_DECOMPRESSEX_QUERY, cast(DWORD_PTR)&ic, ic.sizeof);
}

LRESULT ICDecompressExEnd(HIC hic) {
    return ICSendMessage(hic, ICM_DECOMPRESSEX_END, 0, 0);
}

extern (Windows) {
    DWORD ICDrawBegin(HIC hic, DWORD dwFlags, HPALETTE hpal, HWND hwnd, HDC hdc,
        int xDst, int yDst, int dxDst, int dyDst, LPBITMAPINFOHEADER lpbi,
        int xSrc, int ySrc, int dxSrc, int dySrc, DWORD dwRate, DWORD dwScale);
}

extern (Windows) {
    DWORD ICDraw(HIC hic, DWORD dwFlags, LPVOID lpFormat, LPVOID lpData, DWORD cbData, LONG lTime);
}

LRESULT ICDrawSuggestFormat(HIC hic, LPBITMAPINFOHEADER lpbiIn, LPBITMAPINFOHEADER lpbiOut,
    int dxSrc, int dySrc, int dxDst, int dyDst, HIC hicDecomp) {
    ICDRAWSUGGEST ic;

    ic.lpbiIn = lpbiIn;
    ic.lpbiSuggest = lpbiOut;
    ic.dxSrc = dxSrc;
    ic.dySrc = dySrc;
    ic.dxDst = dxDst;
    ic.dyDst = dyDst;
    ic.hicDecompressor = hicDecomp;

    return ICSendMessage(hic, ICM_DRAW_SUGGESTFORMAT, cast(DWORD_PTR)&ic, ic.sizeof);
}

LRESULT ICDrawQuery(HIC hic, LPVOID lpbiInput) {
    return ICSendMessage(hic, ICM_DRAW_QUERY, cast(DWORD_PTR)lpbiInput, 0L);
}
LRESULT ICDrawChangePalette(HIC hic, LPVOID lpbiInput) {
    return ICSendMessage(hic, ICM_DRAW_CHANGEPALETTE, cast(DWORD_PTR)lpbiInput, 0L);
}
LRESULT ICGetBuffersWanted(HIC hic, LPVOID lpdwBuffers) {
    return ICSendMessage(hic, ICM_GETBUFFERSWANTED, cast(DWORD_PTR)lpdwBuffers, 0);
}
LRESULT ICDrawEnd(HIC hic) {
    return ICSendMessage(hic, ICM_DRAW_END, 0, 0);
}
LRESULT ICDrawStart(HIC hic) {
    return ICSendMessage(hic, ICM_DRAW_START, 0, 0);
}
LRESULT ICDrawStartPlay(HIC hic, DWORD lFrom, DWORD lTo) {
    return ICSendMessage(hic, ICM_DRAW_START_PLAY, cast(DWORD_PTR)lFrom, cast(DWORD_PTR)lTo);
}
LRESULT ICDrawStop(HIC hic) {
    return ICSendMessage(hic, ICM_DRAW_STOP, 0, 0);
}
LRESULT ICDrawStopPlay(HIC hic) {
    return ICSendMessage(hic, ICM_DRAW_STOP_PLAY, 0, 0);
}
LRESULT ICDrawGetTime(HIC hic, LPVOID lplTime) {
    return ICSendMessage(hic, ICM_DRAW_GETTIME, cast(DWORD_PTR)lplTime, 0);
}
LRESULT ICDrawSetTime(HIC hic, DWORD lTime) {
    return ICSendMessage(hic, ICM_DRAW_SETTIME, cast(DWORD_PTR)lTime, 0);
}
LRESULT ICDrawRealize(HIC hic, HDC hdc, BOOL fBackground) {
    return ICSendMessage(hic, ICM_DRAW_REALIZE, cast(DWORD_PTR)hdc, cast(DWORD_PTR)fBackground);
}
LRESULT ICDrawFlush(HIC hic) {
    return ICSendMessage(hic, ICM_DRAW_FLUSH, 0, 0);
}
LRESULT ICDrawRenderBuffer(HIC hic) {
    return ICSendMessage(hic, ICM_DRAW_RENDERBUFFER, 0, 0);
}

extern (Windows)
LRESULT ICSetStatusProc(HIC hic, DWORD dwFlags, LRESULT lParam, LONG function(LPARAM, UINT, LONG) fpfnStatus) {
    ICSETSTATUSPROC ic;

    ic.dwFlags = dwFlags;
    ic.lParam = lParam;
    ic.Status = fpfnStatus;

    return ICSendMessage(hic, ICM_SET_STATUS_PROC, cast(DWORD_PTR)&ic, ic.sizeof);
}

HIC ICDecompressOpen(DWORD fccType, DWORD fccHandler, LPBITMAPINFOHEADER lpbiIn, LPBITMAPINFOHEADER lpbiOut) {
    return ICLocate(fccType, fccHandler, lpbiIn, lpbiOut, ICMODE_DECOMPRESS);
}

HIC ICDrawOpen(DWORD fccType, DWORD fccHandler, LPBITMAPINFOHEADER lpbiIn) {
    return ICLocate(fccType, fccHandler, lpbiIn, null, ICMODE_DRAW);
}

extern (Windows) {
    HIC ICLocate(DWORD fccType, DWORD fccHandler, LPBITMAPINFOHEADER lpbiIn, LPBITMAPINFOHEADER lpbiOut, WORD wFlags);
    HIC ICGetDisplayFormat(HIC hic, LPBITMAPINFOHEADER lpbiIn, LPBITMAPINFOHEADER lpbiOut, int BitDepth, int dx, int dy);
    HANDLE ICImageCompress(HIC hic, UINT uiFlags, LPBITMAPINFO lpbiIn, LPVOID lpBits, LPBITMAPINFO lpbiOut, LONG lQuality, LONG* plSize);
    HANDLE ICImageDecompress(HIC hic, UINT uiFlags, LPBITMAPINFO lpbiIn, LPVOID lpBits, LPBITMAPINFO lpbiOut);
}

struct COMPVARS {
    LONG        cbSize = this.sizeof;
    DWORD       dwFlags;
    HIC         hic;
    DWORD               fccType;
    DWORD               fccHandler;
    LPBITMAPINFO    lpbiIn;
    LPBITMAPINFO    lpbiOut;
    LPVOID      lpBitsOut;
    LPVOID      lpBitsPrev;
    LONG        lFrame;
    LONG        lKey;
    LONG        lDataRate;
    LONG        lQ;
    LONG        lKeyCount;
    LPVOID      lpState;
    LONG        cbState;
}
alias COMPVARS* PCOMPVARS;

enum ICMF_COMPVARS_VALID = 0x00000001;

extern (Windows) {
    BOOL ICCompressorChoose(HWND hwnd, UINT uiFlags, LPVOID pvIn, LPVOID lpData, PCOMPVARS pc, LPSTR lpszTitle);
}

enum {
    ICMF_CHOOSE_KEYFRAME        = 0x0001,
    ICMF_CHOOSE_DATARATE        = 0x0002,
    ICMF_CHOOSE_PREVIEW         = 0x0004,
    ICMF_CHOOSE_ALLCOMPRESSORS  = 0x0008,
}

extern (Windows) {
    BOOL ICSeqCompressFrameStart(PCOMPVARS pc, LPBITMAPINFO lpbiIn);
    void ICSeqCompressFrameEnd(PCOMPVARS pc);
    LPVOID ICSeqCompressFrame(PCOMPVARS pc, UINT uiFlags, LPVOID lpBits, BOOL* pfKey, LONG* plSize);
    void ICCompressorFree(PCOMPVARS pc);
}

mixin DECLARE_HANDLE!("HDRAWDIB");

enum {
    DDF_0001            = 0x0001,
    DDF_UPDATE          = 0x0002,
    DDF_SAME_HDC        = 0x0004,
    DDF_SAME_DRAW       = 0x0008,
    DDF_DONTDRAW        = 0x0010,
    DDF_ANIMATE         = 0x0020,
    DDF_BUFFER          = 0x0040,
    DDF_JUSTDRAWIT      = 0x0080,
    DDF_FULLSCREEN      = 0x0100,
    DDF_BACKGROUNDPAL   = 0x0200,
    DDF_NOTKEYFRAME     = 0x0400,
    DDF_HURRYUP         = 0x0800,
    DDF_HALFTONE        = 0x1000,
    DDF_2000            = 0x2000,
    DDF_PREROLL         = DDF_DONTDRAW,
    DDF_SAME_DIB        = DDF_SAME_DRAW,
    DDF_SAME_SIZE       = DDF_SAME_DRAW,
}

extern (Windows) {
    BOOL DrawDibInit();
    HDRAWDIB DrawDibOpen();
    BOOL DrawDibClose(HDRAWDIB hdd);
    LPVOID DrawDibGetBuffer(HDRAWDIB hdd, LPBITMAPINFOHEADER lpbi, DWORD dwSize, DWORD dwFlags);
    UINT DrawDibError(HDRAWDIB hdd);
    HPALETTE DrawDibGetPalette(HDRAWDIB hdd);
    BOOL DrawDibSetPalette(HDRAWDIB hdd, HPALETTE hpal);
    BOOL DrawDibChangePalette(HDRAWDIB hdd, int iStart, int iLen, LPPALETTEENTRY lppe);
    UINT DrawDibRealize(HDRAWDIB hdd, HDC hdc, BOOL fBackground);
    BOOL DrawDibStart(HDRAWDIB hdd, DWORD rate);
    BOOL DrawDibStop(HDRAWDIB hdd);
    BOOL DrawDibBegin(HDRAWDIB hdd, HDC hdc, int dxDst, int dyDst, LPBITMAPINFOHEADER lpbi, int dxSrc, int dySrc, UINT wFlags);
    BOOL DrawDibDraw(HDRAWDIB hdd, HDC hdc, int xDst, int yDst, int dxDst, int dyDst, LPBITMAPINFOHEADER lpbi,
        LPVOID lpBits, int xSrc, int ySrc, int dxSrc, int dySrc, UINT wFlags);
}

BOOL DrawDibUpdate(HDRAWDIB hdd, HDC hdc, int x, int y) {
    return DrawDibDraw(hdd, hdc, x, y, 0, 0, null, null, 0, 0, 0, 0, DDF_UPDATE);
}

extern (Windows) {
    BOOL DrawDibEnd(HDRAWDIB hdd);
}

struct DRAWDIBTIME {
    LONG    timeCount;
    LONG    timeDraw;
    LONG    timeDecompress;
    LONG    timeDither;
    LONG    timeStretch;
    LONG    timeBlt;
    LONG    timeSetDIBits;
}
alias DRAWDIBTIME* LPDRAWDIBTIME;

extern (Windows) {
    BOOL DrawDibTime(HDRAWDIB hdd, LPDRAWDIBTIME lpddtime);
}

enum {
    PD_CAN_DRAW_DIB         = 0x0001,
    PD_CAN_STRETCHDIB       = 0x0002,
    PD_STRETCHDIB_1_1_OK    = 0x0004,
    PD_STRETCHDIB_1_2_OK    = 0x0008,
    PD_STRETCHDIB_1_N_OK    = 0x0010,
}

extern (Windows) {
    LRESULT DrawDibProfileDisplay(LPBITMAPINFOHEADER lpbi);
    void StretchDIB(LPBITMAPINFOHEADER biDst, LPVOID lpDst, int DstX, int DstY,
        int DstXE, int DstYE, LPBITMAPINFOHEADER biSrc, LPVOID lpSrc,
        int SrcX, int SrcY, int SrcXE, int SrcYE);
}

alias DWORD FOURCC;

alias WORD TWOCC;

enum formtypeAVI           = mmioFOURCC!('A', 'V', 'I', ' ');
enum listtypeAVIHEADER     = mmioFOURCC!('h', 'd', 'r', 'l');
enum ckidAVIMAINHDR        = mmioFOURCC!('a', 'v', 'i', 'h');
enum listtypeSTREAMHEADER  = mmioFOURCC!('s', 't', 'r', 'l');
enum ckidSTREAMHEADER      = mmioFOURCC!('s', 't', 'r', 'h');
enum ckidSTREAMFORMAT      = mmioFOURCC!('s', 't', 'r', 'f');
enum ckidSTREAMHANDLERDATA = mmioFOURCC!('s', 't', 'r', 'd');
enum ckidSTREAMNAME        = mmioFOURCC!('s', 't', 'r', 'n');
enum listtypeAVIMOVIE      = mmioFOURCC!('m', 'o', 'v', 'i');
enum listtypeAVIRECORD     = mmioFOURCC!('r', 'e', 'c', ' ');
enum ckidAVINEWINDEX       = mmioFOURCC!('i', 'd', 'x', '1');
enum streamtypeVIDEO       = mmioFOURCC!('v', 'i', 'd', 's');
enum streamtypeAUDIO       = mmioFOURCC!('a', 'u', 'd', 's');
enum streamtypeMIDI        = mmioFOURCC!('m', 'i', 'd', 's');
enum streamtypeTEXT        = mmioFOURCC!('t', 'x', 't', 's');

enum cktypeDIBbits         = aviTWOCC!('d', 'b');
enum cktypeDIBcompressed   = aviTWOCC!('d', 'c');
enum cktypePALchange       = aviTWOCC!('p', 'c');
enum cktypeWAVEbytes       = aviTWOCC!('w', 'b');

enum ckidAVIPADDING        = mmioFOURCC!('J', 'U', 'N', 'K');

DWORD FromHex(char n) {
    return (n >= 'A') ? n + 10 - 'A' : n - '0';
}

WORD StreamFromFOURCC(DWORD fcc) {
    return cast(WORD)((FromHex(LOBYTE(LOWORD(fcc))) << 4) + (FromHex(HIBYTE(LOWORD(fcc)))));
}

WORD TWOCCFromFOURCC(DWORD fcc) {
    return HIWORD(fcc);
}

BYTE ToHex(DWORD n) {
    return cast(BYTE)((n > 9) ? n - 10 + 'A' : n + '0');
}

DWORD MAKEAVICKID(WORD tcc, WORD stream) {
    return MAKELONG(cast(WORD)((ToHex(stream & 0x0f) << 8) | (ToHex((stream & 0xf0) >> 4))), tcc);
}

enum {
    AVIF_HASINDEX       = 0x00000010,
    AVIF_MUSTUSEINDEX   = 0x00000020,
    AVIF_ISINTERLEAVED  = 0x00000100,
    AVIF_WASCAPTUREFILE = 0x00010000,
    AVIF_COPYRIGHTED    = 0x00020000,
}

enum AVI_HEADERSIZE = 2048;

struct MainAVIHeader {
    DWORD dwMicroSecPerFrame;
    DWORD dwMaxBytesPerSec;
    DWORD dwPaddingGranularity;
    DWORD dwFlags;
    DWORD dwTotalFrames;
    DWORD dwInitialFrames;
    DWORD dwStreams;
    DWORD dwSuggestedBufferSize;
    DWORD dwWidth;
    DWORD dwHeight;
    DWORD[4] dwReserved;
}

enum AVISF_DISABLED = 0x00000001;

enum AVISF_VIDEO_PALCHANGES = 0x00010000;

struct AVIStreamHeader {
    FOURCC      fccType;
    FOURCC      fccHandler;
    DWORD       dwFlags;
    WORD        wPriority;
    WORD        wLanguage;
    DWORD       dwInitialFrames;
    DWORD       dwScale;
    DWORD       dwRate;
    DWORD       dwStart;
    DWORD       dwLength;
    DWORD       dwSuggestedBufferSize;
    DWORD       dwQuality;
    DWORD       dwSampleSize;
    RECT        rcFrame;
}

enum {
    AVIIF_FIRSTPART = 0x00000020L,
    AVIIF_LASTPART  = 0x00000040L,
    AVIIF_MIDPART   = (AVIIF_LASTPART|AVIIF_FIRSTPART),
    AVIIF_NOTIME    = 0x00000100L,
    AVIIF_COMPUSE   = 0x0FFF0000L,
}

struct AVIINDEXENTRY {
    DWORD       ckid;
    DWORD       dwFlags;
    DWORD       dwChunkOffset;
    DWORD       dwChunkLength;
}

struct AVIPALCHANGE {
    BYTE        bFirstEntry;
    BYTE        bNumEntries;
    WORD        wFlags;
    PALETTEENTRY[1] _peNew;
    PALETTEENTRY* peNew() return { return _peNew.ptr; }
}

enum AVIGETFRAMEF_BESTDISPLAYFMT = 1;

struct AVISTREAMINFOW {
    DWORD   fccType;
    DWORD   fccHandler;
    DWORD   dwFlags;
    DWORD   dwCaps;
    WORD    wPriority;
    WORD    wLanguage;
    DWORD   dwScale;
    DWORD   dwRate;
    DWORD   dwStart;
    DWORD   dwLength;
    DWORD   dwInitialFrames;
    DWORD   dwSuggestedBufferSize;
    DWORD   dwQuality;
    DWORD   dwSampleSize;
    RECT    rcFrame;
    DWORD   dwEditCount;
    DWORD   dwFormatChangeCount;
    WCHAR[64]   szName = 0;
}
alias AVISTREAMINFOW* LPAVISTREAMINFOW;

struct AVISTREAMINFOA {
    DWORD   fccType;
    DWORD   fccHandler;
    DWORD   dwFlags;
    DWORD   dwCaps;
    WORD    wPriority;
    WORD    wLanguage;
    DWORD   dwScale;
    DWORD   dwRate;
    DWORD   dwStart;
    DWORD   dwLength;
    DWORD   dwInitialFrames;
    DWORD   dwSuggestedBufferSize;
    DWORD   dwQuality;
    DWORD   dwSampleSize;
    RECT    rcFrame;
    DWORD   dwEditCount;
    DWORD   dwFormatChangeCount;
    char[64]    szName = 0;
}
alias AVISTREAMINFOA* LPAVISTREAMINFOA;

version (Unicode) {
    alias AVISTREAMINFOW    AVISTREAMINFO;
    alias LPAVISTREAMINFOW  LPAVISTREAMINFO;
} else { // Unicode
    alias AVISTREAMINFOA    AVISTREAMINFO;
    alias LPAVISTREAMINFOA  LPAVISTREAMINFO;
}

enum AVISTREAMINFO_DISABLED        = 0x00000001;
enum AVISTREAMINFO_FORMATCHANGES   = 0x00010000;

struct AVIFILEINFOW {
    DWORD   dwMaxBytesPerSec;
    DWORD   dwFlags;
    DWORD   dwCaps;
    DWORD   dwStreams;
    DWORD   dwSuggestedBufferSize;
    DWORD   dwWidth;
    DWORD   dwHeight;
    DWORD   dwScale;
    DWORD   dwRate;
    DWORD   dwLength;
    DWORD   dwEditCount;
    WCHAR[64]   szFileType = 0;
}
alias AVIFILEINFOW* LPAVIFILEINFOW;

struct AVIFILEINFOA {
    DWORD   dwMaxBytesPerSec;
    DWORD   dwFlags;
    DWORD   dwCaps;
    DWORD   dwStreams;
    DWORD   dwSuggestedBufferSize;
    DWORD   dwWidth;
    DWORD   dwHeight;
    DWORD   dwScale;
    DWORD   dwRate;
    DWORD   dwLength;
    DWORD   dwEditCount;
    char[64]    szFileType = 0;
}
alias AVIFILEINFOA* LPAVIFILEINFOA;

version (Unicode) {
    alias AVIFILEINFOW  AVIFILEINFO;
    alias LPAVIFILEINFOW    LPAVIFILEINFO;
} else { // Unicode
    alias AVIFILEINFOA  AVIFILEINFO;
    alias LPAVIFILEINFOA    LPAVIFILEINFO;
}

enum {
    AVIFILEINFO_HASINDEX        = 0x00000010,
    AVIFILEINFO_MUSTUSEINDEX    = 0x00000020,
    AVIFILEINFO_ISINTERLEAVED   = 0x00000100,
    AVIFILEINFO_WASCAPTUREFILE  = 0x00010000,
    AVIFILEINFO_COPYRIGHTED     = 0x00020000,
}

enum {
    AVIFILECAPS_CANREAD         = 0x00000001,
    AVIFILECAPS_CANWRITE        = 0x00000002,
    AVIFILECAPS_ALLKEYFRAMES    = 0x00000010,
    AVIFILECAPS_NOCOMPRESSION   = 0x00000020,
}

extern (Windows) {
    alias BOOL function(int) AVISAVECALLBACK;
}

struct AVICOMPRESSOPTIONS {
    DWORD   fccType;
    DWORD   fccHandler;
    DWORD   dwKeyFrameEvery;
    DWORD   dwQuality;
    DWORD   dwBytesPerSecond;
    DWORD   dwFlags;
    LPVOID  lpFormat;
    DWORD   cbFormat;
    LPVOID  lpParms;
    DWORD   cbParms;
    DWORD   dwInterleaveEvery;
}
alias AVICOMPRESSOPTIONS* LPAVICOMPRESSOPTIONS;

enum {
    AVICOMPRESSF_INTERLEAVE = 0x00000001,
    AVICOMPRESSF_DATARATE   = 0x00000002,
    AVICOMPRESSF_KEYFRAMES  = 0x00000004,
    AVICOMPRESSF_VALID      = 0x00000008,
}

/+ TODO:
DECLARE_INTERFACE_(IAVIStream, IUnknown)
{
    STDMETHOD(QueryInterface) (THIS_ REFIID riid, LPVOID FAR* ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef) (THIS)  PURE;
    STDMETHOD_(ULONG,Release) (THIS) PURE;

    STDMETHOD(Create)      (THIS_ LPARAM lParam1, LPARAM lParam2) PURE ;
    STDMETHOD(Info)        (THIS_ AVISTREAMINFOW FAR * psi, LONG lSize) PURE ;
    STDMETHOD_(LONG, FindSample)(THIS_ LONG lPos, LONG lFlags) PURE ;
    STDMETHOD(ReadFormat)  (THIS_ LONG lPos,
                LPVOID lpFormat, LONG FAR *lpcbFormat) PURE ;
    STDMETHOD(SetFormat)   (THIS_ LONG lPos,
                LPVOID lpFormat, LONG cbFormat) PURE ;
    STDMETHOD(Read)        (THIS_ LONG lStart, LONG lSamples,
                LPVOID lpBuffer, LONG cbBuffer,
                LONG FAR * plBytes, LONG FAR * plSamples) PURE ;
    STDMETHOD(Write)       (THIS_ LONG lStart, LONG lSamples,
                LPVOID lpBuffer, LONG cbBuffer,
                DWORD dwFlags,
                LONG FAR *plSampWritten,
                LONG FAR *plBytesWritten) PURE ;
    STDMETHOD(Delete)      (THIS_ LONG lStart, LONG lSamples) PURE;
    STDMETHOD(ReadData)    (THIS_ DWORD fcc, LPVOID lp, LONG FAR *lpcb) PURE ;
    STDMETHOD(WriteData)   (THIS_ DWORD fcc, LPVOID lp, LONG cb) PURE ;
#ifdef _WIN32
    STDMETHOD(SetInfo) (THIS_ AVISTREAMINFOW FAR * lpInfo,
                LONG cbInfo) PURE;
#else
    STDMETHOD(Reserved1)            (THIS) PURE;
    STDMETHOD(Reserved2)            (THIS) PURE;
    STDMETHOD(Reserved3)            (THIS) PURE;
    STDMETHOD(Reserved4)            (THIS) PURE;
    STDMETHOD(Reserved5)            (THIS) PURE;
#endif
};

alias TypeDef!(IAVIStream FAR*) PAVISTREAM;

#undef  INTERFACE
#define INTERFACE   IAVIStreaming

DECLARE_INTERFACE_(IAVIStreaming, IUnknown)
{
    STDMETHOD(QueryInterface) (THIS_ REFIID riid, LPVOID FAR* ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef) (THIS)  PURE;
    STDMETHOD_(ULONG,Release) (THIS) PURE;

    STDMETHOD(Begin) (THIS_
              LONG  lStart,
              LONG  lEnd,
              LONG  lRate) PURE;
    STDMETHOD(End)   (THIS) PURE;
};

alias TypeDef!(IAVIStreaming FAR*) PAVISTREAMING;


#undef  INTERFACE
#define INTERFACE   IAVIEditStream

DECLARE_INTERFACE_(IAVIEditStream, IUnknown)
{
    STDMETHOD(QueryInterface) (THIS_ REFIID riid, LPVOID FAR* ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef) (THIS)  PURE;
    STDMETHOD_(ULONG,Release) (THIS) PURE;

    STDMETHOD(Cut) (THIS_ LONG FAR *plStart,
              LONG FAR *plLength,
              PAVISTREAM FAR * ppResult) PURE;
    STDMETHOD(Copy) (THIS_ LONG FAR *plStart,
               LONG FAR *plLength,
               PAVISTREAM FAR * ppResult) PURE;
    STDMETHOD(Paste) (THIS_ LONG FAR *plPos,
                LONG FAR *plLength,
                PAVISTREAM pstream,
                LONG lStart,
                LONG lEnd) PURE;
    STDMETHOD(Clone) (THIS_ PAVISTREAM FAR *ppResult) PURE;
    STDMETHOD(SetInfo) (THIS_ AVISTREAMINFOW FAR * lpInfo,
                LONG cbInfo) PURE;
};

alias TypeDef!(IAVIEditStream FAR*) PAVIEDITSTREAM;

#undef  INTERFACE
#define INTERFACE   IAVIPersistFile

DECLARE_INTERFACE_(IAVIPersistFile, IPersistFile)
{
    STDMETHOD(Reserved1)(THIS) PURE;
};

alias TypeDef!(IAVIPersistFile FAR*) PAVIPERSISTFILE;

#undef  INTERFACE
#define INTERFACE   IAVIFile
#define PAVIFILE IAVIFile FAR*

DECLARE_INTERFACE_(IAVIFile, IUnknown)
{
    STDMETHOD(QueryInterface) (THIS_ REFIID riid, LPVOID FAR* ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef) (THIS)  PURE;
    STDMETHOD_(ULONG,Release) (THIS) PURE;

    STDMETHOD(Info)                 (THIS_
                                     AVIFILEINFOW FAR * pfi,
                                     LONG lSize) PURE;
    STDMETHOD(GetStream)            (THIS_
                                     PAVISTREAM FAR * ppStream,
                     DWORD fccType,
                                     LONG lParam) PURE;
    STDMETHOD(CreateStream)         (THIS_
                                     PAVISTREAM FAR * ppStream,
                                     AVISTREAMINFOW FAR * psi) PURE;
    STDMETHOD(WriteData)            (THIS_
                                     DWORD ckid,
                                     LPVOID lpData,
                                     LONG cbData) PURE;
    STDMETHOD(ReadData)             (THIS_
                                     DWORD ckid,
                                     LPVOID lpData,
                                     LONG FAR *lpcbData) PURE;
    STDMETHOD(EndRecord)            (THIS) PURE;
    STDMETHOD(DeleteStream)         (THIS_
                     DWORD fccType,
                                     LONG lParam) PURE;
};

#undef PAVIFILE
alias TypeDef!(IAVIFile FAR*) PAVIFILE;

#undef  INTERFACE
#define INTERFACE   IGetFrame
#define PGETFRAME   IGetFrame FAR*

DECLARE_INTERFACE_(IGetFrame, IUnknown)
{
    STDMETHOD(QueryInterface) (THIS_ REFIID riid, LPVOID FAR* ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef) (THIS)  PURE;
    STDMETHOD_(ULONG,Release) (THIS) PURE;

    STDMETHOD_(LPVOID,GetFrame) (THIS_ LONG lPos) PURE;

    STDMETHOD(Begin) (THIS_ LONG lStart, LONG lEnd, LONG lRate) PURE;
    STDMETHOD(End) (THIS) PURE;

    STDMETHOD(SetFormat) (THIS_ LPBITMAPINFOHEADER lpbi, LPVOID lpBits, int x, int y, int dx, int dy) PURE;
};

#undef PGETFRAME
alias TypeDef!(IGetFrame FAR*) PGETFRAME;

#define DEFINE_AVIGUID(name, l, w1, w2)    DEFINE_GUID(name, l, w1, w2, 0xC0,0,0,0,0,0,0,0x46)

DEFINE_AVIGUID(IID_IAVIFile,            0x00020020, 0, 0);
DEFINE_AVIGUID(IID_IAVIStream,          0x00020021, 0, 0);
DEFINE_AVIGUID(IID_IAVIStreaming,       0x00020022, 0, 0);
DEFINE_AVIGUID(IID_IGetFrame,           0x00020023, 0, 0);
DEFINE_AVIGUID(IID_IAVIEditStream,      0x00020024, 0, 0);
DEFINE_AVIGUID(IID_IAVIPersistFile,     0x00020025, 0, 0);
#ifndef UNICODE
DEFINE_AVIGUID(CLSID_AVISimpleUnMarshal,        0x00020009, 0, 0);
#endif

DEFINE_AVIGUID(CLSID_AVIFile,           0x00020000, 0, 0);

#define AVIFILEHANDLER_CANREAD      0x0001
#define AVIFILEHANDLER_CANWRITE     0x0002
#define AVIFILEHANDLER_CANACCEPTNONRGB  0x0004

STDAPI_(void) AVIFileInit(void);
STDAPI_(void) AVIFileExit(void);

STDAPI_(ULONG) AVIFileAddRef       (PAVIFILE pfile);
STDAPI_(ULONG) AVIFileRelease      (PAVIFILE pfile);

#ifdef _WIN32
STDAPI AVIFileOpenA       (PAVIFILE FAR * ppfile, LPCSTR szFile,
              UINT uMode, LPCLSID lpHandler);
STDAPI AVIFileOpenW       (PAVIFILE FAR * ppfile, LPCWSTR szFile,
              UINT uMode, LPCLSID lpHandler);
#ifdef UNICODE
#define AVIFileOpen   AVIFileOpenW
#else
#define AVIFileOpen   AVIFileOpenA
#endif
#else
STDAPI AVIFileOpen       (PAVIFILE FAR * ppfile, LPCSTR szFile,
              UINT uMode, LPCLSID lpHandler);
#define AVIFileOpenW    AVIFileOpen
#endif

#ifdef _WIN32
STDAPI AVIFileInfoW (PAVIFILE pfile, LPAVIFILEINFOW pfi, LONG lSize);
STDAPI AVIFileInfoA (PAVIFILE pfile, LPAVIFILEINFOA pfi, LONG lSize);
#ifdef UNICODE
#define AVIFileInfo AVIFileInfoW
#else
#define AVIFileInfo AVIFileInfoA
#endif
#else
STDAPI AVIFileInfo (PAVIFILE pfile, LPAVIFILEINFO pfi, LONG lSize);
#define AVIFileInfoW AVIFileInfo
#endif


STDAPI AVIFileGetStream     (PAVIFILE pfile, PAVISTREAM FAR * ppavi, DWORD fccType, LONG lParam);


#ifdef _WIN32
STDAPI AVIFileCreateStreamW (PAVIFILE pfile, PAVISTREAM FAR *ppavi, AVISTREAMINFOW FAR * psi);
STDAPI AVIFileCreateStreamA (PAVIFILE pfile, PAVISTREAM FAR *ppavi, AVISTREAMINFOA FAR * psi);
#ifdef UNICODE
#define AVIFileCreateStream AVIFileCreateStreamW
#else
#define AVIFileCreateStream AVIFileCreateStreamA
#endif
#else
STDAPI AVIFileCreateStream(PAVIFILE pfile, PAVISTREAM FAR *ppavi, AVISTREAMINFO FAR * psi);
#define AVIFileCreateStreamW AVIFileCreateStream
#endif

STDAPI AVIFileWriteData (PAVIFILE pfile,
                     DWORD ckid,
                     LPVOID lpData,
                     LONG cbData);
STDAPI AVIFileReadData  (PAVIFILE pfile,
                     DWORD ckid,
                     LPVOID lpData,
                     LONG FAR *lpcbData);
STDAPI AVIFileEndRecord (PAVIFILE pfile);

STDAPI_(ULONG) AVIStreamAddRef       (PAVISTREAM pavi);
STDAPI_(ULONG) AVIStreamRelease      (PAVISTREAM pavi);

STDAPI AVIStreamInfoW (PAVISTREAM pavi, LPAVISTREAMINFOW psi, LONG lSize);
STDAPI AVIStreamInfoA (PAVISTREAM pavi, LPAVISTREAMINFOA psi, LONG lSize);
#ifdef UNICODE
#define AVIStreamInfo   AVIStreamInfoW
#else
#define AVIStreamInfo   AVIStreamInfoA
#endif

STDAPI_(LONG) AVIStreamFindSample(PAVISTREAM pavi, LONG lPos, LONG lFlags);
STDAPI AVIStreamReadFormat   (PAVISTREAM pavi, LONG lPos,LPVOID lpFormat,LONG FAR *lpcbFormat);
STDAPI AVIStreamSetFormat    (PAVISTREAM pavi, LONG lPos,LPVOID lpFormat,LONG cbFormat);
STDAPI AVIStreamReadData     (PAVISTREAM pavi, DWORD fcc, LPVOID lp, LONG FAR *lpcb);
STDAPI AVIStreamWriteData    (PAVISTREAM pavi, DWORD fcc, LPVOID lp, LONG cb);

STDAPI AVIStreamRead         (PAVISTREAM pavi,
                  LONG lStart,
                  LONG lSamples,
                  LPVOID lpBuffer,
                  LONG cbBuffer,
                  LONG FAR * plBytes,
                  LONG FAR * plSamples);
#define AVISTREAMREAD_CONVENIENT    (-1L)

STDAPI AVIStreamWrite        (PAVISTREAM pavi,
                  LONG lStart, LONG lSamples,
                  LPVOID lpBuffer, LONG cbBuffer, DWORD dwFlags,
                  LONG FAR *plSampWritten,
                  LONG FAR *plBytesWritten);

STDAPI_(LONG) AVIStreamStart        (PAVISTREAM pavi);
STDAPI_(LONG) AVIStreamLength       (PAVISTREAM pavi);
STDAPI_(LONG) AVIStreamTimeToSample (PAVISTREAM pavi, LONG lTime);
STDAPI_(LONG) AVIStreamSampleToTime (PAVISTREAM pavi, LONG lSample);


STDAPI AVIStreamBeginStreaming(PAVISTREAM pavi, LONG lStart, LONG lEnd, LONG lRate);
STDAPI AVIStreamEndStreaming(PAVISTREAM pavi);

STDAPI_(PGETFRAME) AVIStreamGetFrameOpen(PAVISTREAM pavi,
                     LPBITMAPINFOHEADER lpbiWanted);
STDAPI_(LPVOID) AVIStreamGetFrame(PGETFRAME pg, LONG lPos);
STDAPI AVIStreamGetFrameClose(PGETFRAME pg);

STDAPI AVIStreamOpenFromFileA(PAVISTREAM FAR *ppavi, LPCSTR szFile,
                 DWORD fccType, LONG lParam,
                 UINT mode, CLSID FAR *pclsidHandler);
STDAPI AVIStreamOpenFromFileW(PAVISTREAM FAR *ppavi, LPCWSTR szFile,
                 DWORD fccType, LONG lParam,
                 UINT mode, CLSID FAR *pclsidHandler);
#ifdef UNICODE
#define AVIStreamOpenFromFile   AVIStreamOpenFromFileW
#else
#define AVIStreamOpenFromFile   AVIStreamOpenFromFileA
#endif

STDAPI AVIStreamCreate(PAVISTREAM FAR *ppavi, LONG lParam1, LONG lParam2,
               CLSID FAR *pclsidHandler);



#define FIND_DIR        0x0000000FL
#define FIND_NEXT       0x00000001L
#define FIND_PREV       0x00000004L
#define FIND_FROM_START 0x00000008L

#define FIND_TYPE       0x000000F0L
#define FIND_KEY        0x00000010L
#define FIND_ANY        0x00000020L
#define FIND_FORMAT     0x00000040L

#define FIND_RET        0x0000F000L
#define FIND_POS        0x00000000L
#define FIND_LENGTH     0x00001000L
#define FIND_OFFSET     0x00002000L
#define FIND_SIZE       0x00003000L
#define FIND_INDEX      0x00004000L

#define AVIStreamFindKeyFrame AVIStreamFindSample
#define FindKeyFrame    FindSample

#define AVIStreamClose AVIStreamRelease
#define AVIFileClose   AVIFileRelease
#define AVIStreamInit  AVIFileInit
#define AVIStreamExit  AVIFileExit

#define SEARCH_NEAREST  FIND_PREV
#define SEARCH_BACKWARD FIND_PREV
#define SEARCH_FORWARD  FIND_NEXT
#define SEARCH_KEY      FIND_KEY
#define SEARCH_ANY      FIND_ANY

#define     AVIStreamSampleToSample(pavi1, pavi2, l)            AVIStreamTimeToSample(pavi1,AVIStreamSampleToTime(pavi2, l))

#define     AVIStreamNextSample(pavi, l)            AVIStreamFindSample(pavi,l+1,FIND_NEXT|FIND_ANY)

#define     AVIStreamPrevSample(pavi, l)            AVIStreamFindSample(pavi,l-1,FIND_PREV|FIND_ANY)

#define     AVIStreamNearestSample(pavi, l)            AVIStreamFindSample(pavi,l,FIND_PREV|FIND_ANY)

#define     AVIStreamNextKeyFrame(pavi,l)            AVIStreamFindSample(pavi,l+1,FIND_NEXT|FIND_KEY)

#define     AVIStreamPrevKeyFrame(pavi, l)            AVIStreamFindSample(pavi,l-1,FIND_PREV|FIND_KEY)

#define     AVIStreamNearestKeyFrame(pavi, l)            AVIStreamFindSample(pavi,l,FIND_PREV|FIND_KEY)

#define     AVIStreamIsKeyFrame(pavi, l)            (AVIStreamNearestKeyFrame(pavi,l) == l)

#define     AVIStreamPrevSampleTime(pavi, t)            AVIStreamSampleToTime(pavi, AVIStreamPrevSample(pavi,AVIStreamTimeToSample(pavi,t)))

#define     AVIStreamNextSampleTime(pavi, t)            AVIStreamSampleToTime(pavi, AVIStreamNextSample(pavi,AVIStreamTimeToSample(pavi,t)))

#define     AVIStreamNearestSampleTime(pavi, t)            AVIStreamSampleToTime(pavi, AVIStreamNearestSample(pavi,AVIStreamTimeToSample(pavi,t)))

#define     AVIStreamNextKeyFrameTime(pavi, t)            AVIStreamSampleToTime(pavi, AVIStreamNextKeyFrame(pavi,AVIStreamTimeToSample(pavi, t)))

#define     AVIStreamPrevKeyFrameTime(pavi, t)            AVIStreamSampleToTime(pavi, AVIStreamPrevKeyFrame(pavi,AVIStreamTimeToSample(pavi, t)))

#define     AVIStreamNearestKeyFrameTime(pavi, t)            AVIStreamSampleToTime(pavi, AVIStreamNearestKeyFrame(pavi,AVIStreamTimeToSample(pavi, t)))

#define     AVIStreamStartTime(pavi)            AVIStreamSampleToTime(pavi, AVIStreamStart(pavi))

#define     AVIStreamLengthTime(pavi)            AVIStreamSampleToTime(pavi, AVIStreamLength(pavi))

#define     AVIStreamEnd(pavi)            (AVIStreamStart(pavi) + AVIStreamLength(pavi))

#define     AVIStreamEndTime(pavi)            AVIStreamSampleToTime(pavi, AVIStreamEnd(pavi))

#define     AVIStreamSampleSize(pavi, lPos, plSize)     AVIStreamRead(pavi,lPos,1,NULL,0,plSize,NULL)

#define     AVIStreamFormatSize(pavi, lPos, plSize)            AVIStreamReadFormat(pavi,lPos,NULL,plSize)

#define     AVIStreamDataSize(pavi, fcc, plSize)            AVIStreamReadData(pavi,fcc,NULL,plSize)

#ifndef comptypeDIB
#define comptypeDIB         mmioFOURCC('D', 'I', 'B', ' ')
#endif

STDAPI AVIMakeCompressedStream(
        PAVISTREAM FAR *        ppsCompressed,
        PAVISTREAM          ppsSource,
        AVICOMPRESSOPTIONS FAR *    lpOptions,
        CLSID FAR *pclsidHandler);

EXTERN_C HRESULT CDECL AVISaveA (LPCSTR               szFile,
        CLSID FAR *pclsidHandler,
        AVISAVECALLBACK     lpfnCallback,
        int                 nStreams,
        PAVISTREAM      pfile,
        LPAVICOMPRESSOPTIONS lpOptions,
        ...);

STDAPI AVISaveVA(LPCSTR               szFile,
        CLSID FAR *pclsidHandler,
        AVISAVECALLBACK     lpfnCallback,
        int                 nStreams,
        PAVISTREAM FAR *    ppavi,
        LPAVICOMPRESSOPTIONS FAR *plpOptions);
EXTERN_C HRESULT CDECL AVISaveW (LPCWSTR               szFile,
        CLSID FAR *pclsidHandler,
        AVISAVECALLBACK     lpfnCallback,
        int                 nStreams,
        PAVISTREAM      pfile,
        LPAVICOMPRESSOPTIONS lpOptions,
        ...);

STDAPI AVISaveVW(LPCWSTR               szFile,
        CLSID FAR *pclsidHandler,
        AVISAVECALLBACK     lpfnCallback,
        int                 nStreams,
        PAVISTREAM FAR *    ppavi,
        LPAVICOMPRESSOPTIONS FAR *plpOptions);
#ifdef UNICODE
#define AVISave     AVISaveW
#define AVISaveV    AVISaveVW
#else
#define AVISave     AVISaveA
#define AVISaveV    AVISaveVA
#endif



STDAPI_(INT_PTR) AVISaveOptions(HWND hwnd,
                 UINT   uiFlags,
                 int    nStreams,
                 PAVISTREAM FAR *ppavi,
                 LPAVICOMPRESSOPTIONS FAR *plpOptions);

STDAPI AVISaveOptionsFree(int nStreams,
                 LPAVICOMPRESSOPTIONS FAR *plpOptions);

STDAPI AVIBuildFilterW(LPWSTR lpszFilter, LONG cbFilter, BOOL fSaving);
STDAPI AVIBuildFilterA(LPSTR lpszFilter, LONG cbFilter, BOOL fSaving);
#ifdef UNICODE
#define AVIBuildFilter  AVIBuildFilterW
#else
#define AVIBuildFilter  AVIBuildFilterA
#endif
STDAPI AVIMakeFileFromStreams(PAVIFILE FAR *    ppfile,
                   int      nStreams,
                   PAVISTREAM FAR * papStreams);

STDAPI AVIMakeStreamFromClipboard(UINT cfFormat, HANDLE hGlobal, PAVISTREAM FAR *ppstream);

STDAPI AVIPutFileOnClipboard(PAVIFILE pf);

STDAPI AVIGetFromClipboard(PAVIFILE FAR * lppf);

STDAPI AVIClearClipboard(void);

STDAPI CreateEditableStream(
        PAVISTREAM FAR *        ppsEditable,
        PAVISTREAM          psSource);

STDAPI EditStreamCut(PAVISTREAM pavi, LONG FAR *plStart, LONG FAR *plLength, PAVISTREAM FAR * ppResult);

STDAPI EditStreamCopy(PAVISTREAM pavi, LONG FAR *plStart, LONG FAR *plLength, PAVISTREAM FAR * ppResult);

STDAPI EditStreamPaste(PAVISTREAM pavi, LONG FAR *plPos, LONG FAR *plLength, PAVISTREAM pstream, LONG lStart, LONG lEnd);

STDAPI EditStreamClone(PAVISTREAM pavi, PAVISTREAM FAR *ppResult);


STDAPI EditStreamSetNameA(PAVISTREAM pavi, LPCSTR lpszName);
STDAPI EditStreamSetNameW(PAVISTREAM pavi, LPCWSTR lpszName);
STDAPI EditStreamSetInfoW(PAVISTREAM pavi, LPAVISTREAMINFOW lpInfo, LONG cbInfo);
STDAPI EditStreamSetInfoA(PAVISTREAM pavi, LPAVISTREAMINFOA lpInfo, LONG cbInfo);
#ifdef UNICODE
#define EditStreamSetInfo   EditStreamSetInfoW
#define EditStreamSetName   EditStreamSetNameW
#else
#define EditStreamSetInfo   EditStreamSetInfoA
#define EditStreamSetName   EditStreamSetNameA
#endif
+/
enum AVIERR_OK = 0L;

SCODE MAKE_AVIERR(DWORD error) {
    return MAKE_SCODE(SEVERITY_ERROR, FACILITY_ITF, 0x4000 + error);
}

enum AVIERR_UNSUPPORTED    = MAKE_AVIERR(101);
enum AVIERR_BADFORMAT      = MAKE_AVIERR(102);
enum AVIERR_MEMORY         = MAKE_AVIERR(103);
enum AVIERR_INTERNAL       = MAKE_AVIERR(104);
enum AVIERR_BADFLAGS       = MAKE_AVIERR(105);
enum AVIERR_BADPARAM       = MAKE_AVIERR(106);
enum AVIERR_BADSIZE        = MAKE_AVIERR(107);
enum AVIERR_BADHANDLE      = MAKE_AVIERR(108);
enum AVIERR_FILEREAD       = MAKE_AVIERR(109);
enum AVIERR_FILEWRITE      = MAKE_AVIERR(110);
enum AVIERR_FILEOPEN       = MAKE_AVIERR(111);
enum AVIERR_COMPRESSOR     = MAKE_AVIERR(112);
enum AVIERR_NOCOMPRESSOR   = MAKE_AVIERR(113);
enum AVIERR_READONLY       = MAKE_AVIERR(114);
enum AVIERR_NODATA         = MAKE_AVIERR(115);
enum AVIERR_BUFFERTOOSMALL = MAKE_AVIERR(116);
enum AVIERR_CANTCOMPRESS   = MAKE_AVIERR(117);
enum AVIERR_USERABORT      = MAKE_AVIERR(198);
enum AVIERR_ERROR          = MAKE_AVIERR(199);

const TCHAR[] MCIWND_WINDOW_CLASS = "MCIWndClass";

extern (Windows) {
    HWND MCIWndCreateA(HWND hwndParent, HINSTANCE hInstance, DWORD dwStyle, LPCSTR szFile);
    HWND MCIWndCreateW(HWND hwndParent, HINSTANCE hInstance, DWORD dwStyle, LPCWSTR szFile);
}

version (Unicode) {
    alias MCIWndCreateW MCIWndCreate;
} else { // Unicode
    alias MCIWndCreateA MCIWndCreate;
}

extern(Windows) {
    BOOL MCIWndRegisterClass();
}

enum {
    MCIWNDOPENF_NEW             = 0x0001,
    MCIWNDF_NOAUTOSIZEWINDOW    = 0x0001,
    MCIWNDF_NOPLAYBAR           = 0x0002,
    MCIWNDF_NOAUTOSIZEMOVIE     = 0x0004,
    MCIWNDF_NOMENU              = 0x0008,
    MCIWNDF_SHOWNAME            = 0x0010,
    MCIWNDF_SHOWPOS             = 0x0020,
    MCIWNDF_SHOWMODE            = 0x0040,
    MCIWNDF_SHOWALL             = 0x0070,
    MCIWNDF_NOTIFYMODE          = 0x0100,
    MCIWNDF_NOTIFYPOS           = 0x0200,
    MCIWNDF_NOTIFYSIZE          = 0x0400,
    MCIWNDF_NOTIFYERROR         = 0x1000,
    MCIWNDF_NOTIFYALL           = 0x1F00,
    MCIWNDF_NOTIFYANSI          = 0x0080,
    MCIWNDF_NOTIFYMEDIAA        = 0x0880,
    MCIWNDF_NOTIFYMEDIAW        = 0x0800,
}

version (Unicode) {
    alias MCIWNDF_NOTIFYMEDIAW  MCIWNDF_NOTIFYMEDIA;
} else { // Unicode
    alias MCIWNDF_NOTIFYMEDIAA  MCIWNDF_NOTIFYMEDIA;
}

enum {
    MCIWNDF_RECORD      = 0x2000,
    MCIWNDF_NOERRORDLG  = 0x4000,
    MCIWNDF_NOOPEN      = 0x8000,
}

// can macros

BOOL MCIWndCanPlay(HWND hwnd)
    { return cast(BOOL)SendMessage(hwnd, MCIWNDM_CAN_PLAY, 0, 0); }
BOOL MCIWndCanRecord(HWND hwnd)
    { return cast(BOOL)SendMessage(hwnd, MCIWNDM_CAN_RECORD, 0, 0); }
BOOL MCIWndCanSave(HWND hwnd)
    { return cast(BOOL)SendMessage(hwnd, MCIWNDM_CAN_SAVE, 0, 0); }
BOOL MCIWndCanWindow(HWND hwnd)
    { return cast(BOOL)SendMessage(hwnd, MCIWNDM_CAN_WINDOW, 0, 0); }
BOOL MCIWndCanEject(HWND hwnd)
    { return cast(BOOL)SendMessage(hwnd, MCIWNDM_CAN_EJECT, 0, 0); }
BOOL MCIWndCanConfig(HWND hwnd)
    { return cast(BOOL)SendMessage(hwnd, MCIWNDM_CAN_CONFIG, 0, 0); }
BOOL MCIWndPaletteKick(HWND hwnd)
    { return cast(BOOL)SendMessage(hwnd, MCIWNDM_PALETTEKICK, 0, 0); }
LONG MCIWndSave(HWND hwnd, LPVOID szFile)
    { return cast(LONG)SendMessage(hwnd, MCI_SAVE, 0, cast(LPARAM)szFile); }
LONG MCIWndSaveDialog(HWND hwnd)
    { return MCIWndSave(hwnd, cast(LPVOID)-1); }
LONG MCIWndNew(HWND hwnd, LPVOID lp)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_NEW, 0, cast(LPARAM)lp); }
LONG MCIWndRecord(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCI_RECORD, 0, 0); }
LONG MCIWndOpen(HWND hwnd, LPVOID sz, UINT f)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_OPEN, cast(WPARAM)f, cast(LPARAM)sz); }
LONG MCIWndOpenDialog(HWND hwnd)
    { return MCIWndOpen(hwnd, cast(LPVOID)-1, 0); }
LONG MCIWndClose(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCI_CLOSE, 0, 0); }
LONG MCIWndPlay(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCI_PLAY, 0, 0); }
LONG MCIWndStop(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCI_STOP, 0, 0); }
LONG MCIWndPause(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCI_PAUSE, 0, 0); }
LONG MCIWndResume(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCI_RESUME, 0, 0); }
LONG MCIWndSeek(HWND hwnd, LONG lPos)
    { return cast(LONG)SendMessage(hwnd, MCI_SEEK, 0, cast(LPARAM)lPos); }
LONG MCIWndHome(HWND hwnd)
    { return MCIWndSeek(hwnd, MCIWND_START); }
LONG MCIWndEnd(HWND hwnd)
    { return MCIWndSeek(hwnd, MCIWND_END); }
LONG MCIWndEject(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_EJECT, 0, 0); }
LONG MCIWndGetSource(HWND hwnd, LPRECT prc)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GET_SOURCE, 0, cast(LPARAM)prc); }
LONG MCIWndPutSource(HWND hwnd, LPRECT prc)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_PUT_SOURCE, 0, cast(LPARAM)prc); }
LONG MCIWndGetDest(HWND hwnd, LPRECT prc)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GET_DEST, 0, cast(LPARAM)prc); }
LONG MCIWndPutDest(HWND hwnd, LPRECT prc)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_PUT_DEST, 0, cast(LPARAM)prc); }
LONG MCIWndPlayReverse(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_PLAYREVERSE, 0, 0); }
LONG MCIWndPlayFrom(HWND hwnd, LONG lPos)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_PLAYFROM, 0, cast(LPARAM)lPos); }
LONG MCIWndPlayTo(HWND hwnd, LONG lPos)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_PLAYTO, 0, cast(LPARAM)lPos); }
LONG MCIWndPlayFromTo(HWND hwnd, LONG lStart, LONG lEnd)
    { MCIWndSeek(hwnd, lStart); return MCIWndPlayTo(hwnd, lEnd); }
UINT MCIWndGetDeviceID(HWND hwnd)
    { return cast(UINT)SendMessage(hwnd, MCIWNDM_GETDEVICEID, 0, 0); }
UINT MCIWndGetAlias(HWND hwnd)
    { return cast(UINT)SendMessage(hwnd, MCIWNDM_GETALIAS, 0, 0); }
LONG MCIWndGetMode(HWND hwnd, LPTSTR lp, UINT len)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GETMODE, cast(WPARAM)len, cast(LPARAM)lp); }
LONG MCIWndGetPosition(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GETPOSITION, 0, 0); }
LONG MCIWndGetPositionString(HWND hwnd, LPTSTR lp, UINT len)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GETPOSITION, cast(WPARAM)len, cast(LPARAM)lp); }
LONG MCIWndGetStart(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GETSTART, 0, 0); }
LONG MCIWndGetLength(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GETLENGTH, 0, 0); }
LONG MCIWndGetEnd(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GETEND, 0, 0); }
LONG MCIWndStep(HWND hwnd, LONG n)
    { return cast(LONG)SendMessage(hwnd, MCI_STEP, 0, cast(LPARAM)n); }
void MCIWndDestroy(HWND hwnd)
    { SendMessage(hwnd, WM_CLOSE, 0, 0); }
void MCIWndSetZoom(HWND hwnd, UINT iZoom)
    { SendMessage(hwnd, MCIWNDM_SETZOOM, 0, cast(LPARAM)iZoom); }
UINT MCIWndGetZoom(HWND hwnd)
    { return cast(UINT)SendMessage(hwnd, MCIWNDM_GETZOOM, 0, 0); }
LONG MCIWndSetVolume(HWND hwnd, UINT iVol)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_SETVOLUME, 0, cast(LPARAM)iVol); }
LONG MCIWndGetVolume(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GETVOLUME, 0, 0); }
LONG MCIWndSetSpeed(HWND hwnd, UINT iSpeed)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_SETSPEED, 0, cast(LPARAM)iSpeed); }
LONG MCIWndGetSpeed(HWND hwnd)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GETSPEED, 0, 0); }
LONG MCIWndSetTimeFormat(HWND hwnd, LPTSTR lp)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_SETTIMEFORMAT, 0, cast(LPARAM)lp); }
LONG MCIWndUseFrames(HWND hwnd)
    { return MCIWndSetTimeFormat(hwnd, (cast(TCHAR[])"frames").ptr); }
LONG MCIWndUseTime(HWND hwnd)
    { return MCIWndSetTimeFormat(hwnd, (cast(TCHAR[])"ms").ptr); }
LONG MCIWndGetTimeFormat(HWND hwnd, LPTSTR lp, UINT len)
    { return cast(LONG)SendMessage(hwnd, MCIWNDM_GETTIMEFORMAT, cast(WPARAM)len, cast(LPARAM)lp); }
void MCIWndValidateMedia(HWND hwnd)
    { SendMessage(hwnd, MCIWNDM_VALIDATEMEDIA, 0, 0); }
void MCIWndSetRepeat(HWND hwnd, BOOL f)
    { SendMessage(hwnd, MCIWNDM_SETREPEAT, 0, cast(LPARAM)f); }
BOOL MCIWndGetRepeat(HWND hwnd)
    { return cast(BOOL)SendMessage(hwnd, MCIWNDM_GETREPEAT, 0, 0); }
void MCIWndSetActiveTimer(HWND hwnd, UINT active)
    { SendMessage(hwnd, MCIWNDM_SETACTIVETIMER, cast(WPARAM)active, 0); }
void MCIWndSetInactiveTimer(HWND hwnd, UINT inactive)
    { SendMessage(hwnd, MCIWNDM_SETINACTIVETIMER, cast(WPARAM)inactive, 0); }
void MCIWndSetTimers(HWND hwnd, UINT active, UINT inactive)
    { SendMessage(hwnd, MCIWNDM_SETTIMERS, cast(WPARAM)active, cast(LPARAM)inactive); }
UINT MCIWndGetActiveTimer(HWND hwnd)
    { return cast(UINT)SendMessage(hwnd, MCIWNDM_GETACTIVETIMER, 0, 0); }
UINT MCIWndGetInactiveTimer(HWND hwnd)
    { return cast(UINT)SendMessage(hwnd, MCIWNDM_GETINACTIVETIMER, 0, 0); }
LONG MCIWndRealize(HWND hwnd, BOOL fBkgnd)
    { return cast(LONG) SendMessage(hwnd, MCIWNDM_REALIZE, cast(WPARAM)fBkgnd, 0); }
LONG MCIWndSendString(HWND hwnd, LPTSTR sz)
    { return cast(LONG) SendMessage(hwnd, MCIWNDM_SENDSTRING, 0, cast(LPARAM)sz); }
LONG MCIWndReturnString(HWND hwnd, LPVOID lp, UINT len)
    { return cast(LONG) SendMessage(hwnd, MCIWNDM_RETURNSTRING, cast(WPARAM)len, cast(LPARAM)lp); }
LONG MCIWndGetError(HWND hwnd, LPVOID lp, UINT len)
    { return cast(LONG) SendMessage(hwnd, MCIWNDM_GETERROR, cast(WPARAM)len, cast(LPARAM)lp); }
HPALETTE MCIWndGetPalette(HWND hwnd)
    { return cast(HPALETTE)SendMessage(hwnd, MCIWNDM_GETPALETTE, 0, 0); }
LONG MCIWndSetPalette(HWND hwnd, HPALETTE hpal)
    { return cast(LONG) SendMessage(hwnd, MCIWNDM_SETPALETTE, cast(WPARAM)hpal, 0); }
LONG MCIWndGetFileName(HWND hwnd, LPVOID lp, UINT len)
    { return cast(LONG) SendMessage(hwnd, MCIWNDM_GETFILENAME, cast(WPARAM)len, cast(LPARAM)lp); }
LONG MCIWndGetDevice(HWND hwnd, LPVOID lp, UINT len)
    { return cast(LONG) SendMessage(hwnd, MCIWNDM_GETDEVICE, cast(WPARAM)len, cast(LPARAM)lp); }
UINT MCIWndGetStyles(HWND hwnd)
    { return cast(UINT) SendMessage(hwnd, MCIWNDM_GETSTYLES, 0, 0); }
LONG MCIWndChangeStyles(HWND hwnd, UINT mask, LONG value)
    { return cast(LONG) SendMessage(hwnd, MCIWNDM_CHANGESTYLES, cast(WPARAM)mask, cast(LPARAM)value); }
LONG MCIWndOpenInterface(HWND hwnd, LPUNKNOWN pUnk)
    { return cast(LONG) SendMessage(hwnd, MCIWNDM_OPENINTERFACE, 0, cast(LPARAM)cast(void*)pUnk); }
LONG MCIWndSetOwner(HWND hwnd, HWND hwndP)
    { return cast(LONG) SendMessage(hwnd, MCIWNDM_SETOWNER, cast(WPARAM)hwndP, 0); }

enum {
    MCIWNDM_GETDEVICEID         = WM_USER + 100,
    MCIWNDM_SENDSTRINGA         = WM_USER + 101,
    MCIWNDM_GETPOSITIONA        = WM_USER + 102,
    MCIWNDM_GETSTART            = WM_USER + 103,
    MCIWNDM_GETLENGTH           = WM_USER + 104,
    MCIWNDM_GETEND              = WM_USER + 105,
    MCIWNDM_GETMODEA            = WM_USER + 106,
    MCIWNDM_EJECT               = WM_USER + 107,
    MCIWNDM_SETZOOM             = WM_USER + 108,
    MCIWNDM_GETZOOM             = WM_USER + 109,
    MCIWNDM_SETVOLUME           = WM_USER + 110,
    MCIWNDM_GETVOLUME           = WM_USER + 111,
    MCIWNDM_SETSPEED            = WM_USER + 112,
    MCIWNDM_GETSPEED            = WM_USER + 113,
    MCIWNDM_SETREPEAT           = WM_USER + 114,
    MCIWNDM_GETREPEAT           = WM_USER + 115,
    MCIWNDM_REALIZE             = WM_USER + 118,
    MCIWNDM_SETTIMEFORMATA      = WM_USER + 119,
    MCIWNDM_GETTIMEFORMATA      = WM_USER + 120,
    MCIWNDM_VALIDATEMEDIA       = WM_USER + 121,
    MCIWNDM_PLAYFROM            = WM_USER + 122,
    MCIWNDM_PLAYTO              = WM_USER + 123,
    MCIWNDM_GETFILENAMEA        = WM_USER + 124,
    MCIWNDM_GETDEVICEA          = WM_USER + 125,
    MCIWNDM_GETPALETTE          = WM_USER + 126,
    MCIWNDM_SETPALETTE          = WM_USER + 127,
    MCIWNDM_GETERRORA           = WM_USER + 128,
    MCIWNDM_SETTIMERS           = WM_USER + 129,
    MCIWNDM_SETACTIVETIMER      = WM_USER + 130,
    MCIWNDM_SETINACTIVETIMER    = WM_USER + 131,
    MCIWNDM_GETACTIVETIMER      = WM_USER + 132,
    MCIWNDM_GETINACTIVETIMER    = WM_USER + 133,
    MCIWNDM_NEWA                = WM_USER + 134,
    MCIWNDM_CHANGESTYLES        = WM_USER + 135,
    MCIWNDM_GETSTYLES           = WM_USER + 136,
    MCIWNDM_GETALIAS            = WM_USER + 137,
    MCIWNDM_RETURNSTRINGA       = WM_USER + 138,
    MCIWNDM_PLAYREVERSE         = WM_USER + 139,
    MCIWNDM_GET_SOURCE          = WM_USER + 140,
    MCIWNDM_PUT_SOURCE          = WM_USER + 141,
    MCIWNDM_GET_DEST            = WM_USER + 142,
    MCIWNDM_PUT_DEST            = WM_USER + 143,
    MCIWNDM_CAN_PLAY            = WM_USER + 144,
    MCIWNDM_CAN_WINDOW          = WM_USER + 145,
    MCIWNDM_CAN_RECORD          = WM_USER + 146,
    MCIWNDM_CAN_SAVE            = WM_USER + 147,
    MCIWNDM_CAN_EJECT           = WM_USER + 148,
    MCIWNDM_CAN_CONFIG          = WM_USER + 149,
    MCIWNDM_PALETTEKICK         = WM_USER + 150,
    MCIWNDM_OPENINTERFACE       = WM_USER + 151,
    MCIWNDM_SETOWNER            = WM_USER + 152,
    MCIWNDM_OPENA               = WM_USER + 153,
    MCIWNDM_SENDSTRINGW         = WM_USER + 201,
    MCIWNDM_GETPOSITIONW        = WM_USER + 202,
    MCIWNDM_GETMODEW            = WM_USER + 206,
    MCIWNDM_SETTIMEFORMATW      = WM_USER + 219,
    MCIWNDM_GETTIMEFORMATW      = WM_USER + 220,
    MCIWNDM_GETFILENAMEW        = WM_USER + 224,
    MCIWNDM_GETDEVICEW          = WM_USER + 225,
    MCIWNDM_GETERRORW           = WM_USER + 228,
    MCIWNDM_NEWW                = WM_USER + 234,
    MCIWNDM_RETURNSTRINGW       = WM_USER + 238,
    MCIWNDM_OPENW               = WM_USER + 252,
}

version (Unicode) {
    alias MCIWNDM_SENDSTRINGW       MCIWNDM_SENDSTRING;
    alias MCIWNDM_GETPOSITIONW      MCIWNDM_GETPOSITION;
    alias MCIWNDM_GETMODEW          MCIWNDM_GETMODE;
    alias MCIWNDM_SETTIMEFORMATW    MCIWNDM_SETTIMEFORMAT;
    alias MCIWNDM_GETTIMEFORMATW    MCIWNDM_GETTIMEFORMAT;
    alias MCIWNDM_GETFILENAMEW      MCIWNDM_GETFILENAME;
    alias MCIWNDM_GETDEVICEW        MCIWNDM_GETDEVICE;
    alias MCIWNDM_GETERRORW         MCIWNDM_GETERROR;
    alias MCIWNDM_NEWW              MCIWNDM_NEW;
    alias MCIWNDM_RETURNSTRINGW     MCIWNDM_RETURNSTRING;
    alias MCIWNDM_OPENW             MCIWNDM_OPEN;
} else { // Unicode
    alias MCIWNDM_SENDSTRINGA       MCIWNDM_SENDSTRING;
    alias MCIWNDM_GETPOSITIONA      MCIWNDM_GETPOSITION;
    alias MCIWNDM_GETMODEA          MCIWNDM_GETMODE;
    alias MCIWNDM_SETTIMEFORMATA    MCIWNDM_SETTIMEFORMAT;
    alias MCIWNDM_GETTIMEFORMATA    MCIWNDM_GETTIMEFORMAT;
    alias MCIWNDM_GETFILENAMEA      MCIWNDM_GETFILENAME;
    alias MCIWNDM_GETDEVICEA        MCIWNDM_GETDEVICE;
    alias MCIWNDM_GETERRORA         MCIWNDM_GETERROR;
    alias MCIWNDM_NEWA              MCIWNDM_NEW;
    alias MCIWNDM_RETURNSTRINGA     MCIWNDM_RETURNSTRING;
    alias MCIWNDM_OPENA             MCIWNDM_OPEN;
}

enum {
    MCIWNDM_NOTIFYMODE  = WM_USER + 200,
    MCIWNDM_NOTIFYPOS   = WM_USER + 201,
    MCIWNDM_NOTIFYSIZE  = WM_USER + 202,
    MCIWNDM_NOTIFYMEDIA = WM_USER + 203,
    MCIWNDM_NOTIFYERROR = WM_USER + 205,
}

enum MCIWND_START  = -1;
enum MCIWND_END    = -2;

enum {
    MCI_CLOSE   = 0x0804,
    MCI_PLAY    = 0x0806,
    MCI_SEEK    = 0x0807,
    MCI_STOP    = 0x0808,
    MCI_PAUSE   = 0x0809,
    MCI_STEP    = 0x080E,
    MCI_RECORD  = 0x080F,
    MCI_SAVE    = 0x0813,
    MCI_CUT     = 0x0851,
    MCI_COPY    = 0x0852,
    MCI_PASTE   = 0x0853,
    MCI_RESUME  = 0x0855,
    MCI_DELETE  = 0x0856,
}

enum {
    MCI_MODE_NOT_READY  = 524,
    MCI_MODE_STOP,
    MCI_MODE_PLAY,
    MCI_MODE_RECORD,
    MCI_MODE_SEEK,
    MCI_MODE_PAUSE,
    MCI_MODE_OPEN,
}

alias TypeDef!(HANDLE) HVIDEO;
alias HVIDEO* LPHVIDEO;

// Error Return Values

enum {
    DV_ERR_OK               = 0,
    DV_ERR_BASE             = 1,
    DV_ERR_NONSPECIFIC      = DV_ERR_BASE,
    DV_ERR_BADFORMAT        = DV_ERR_BASE + 1,
    DV_ERR_STILLPLAYING     = DV_ERR_BASE + 2,
    DV_ERR_UNPREPARED       = DV_ERR_BASE + 3,
    DV_ERR_SYNC             = DV_ERR_BASE + 4,
    DV_ERR_TOOMANYCHANNELS  = DV_ERR_BASE + 5,
    DV_ERR_NOTDETECTED      = DV_ERR_BASE + 6,
    DV_ERR_BADINSTALL       = DV_ERR_BASE + 7,
    DV_ERR_CREATEPALETTE    = DV_ERR_BASE + 8,
    DV_ERR_SIZEFIELD        = DV_ERR_BASE + 9,
    DV_ERR_PARAM1           = DV_ERR_BASE + 10,
    DV_ERR_PARAM2           = DV_ERR_BASE + 11,
    DV_ERR_CONFIG1          = DV_ERR_BASE + 12,
    DV_ERR_CONFIG2          = DV_ERR_BASE + 13,
    DV_ERR_FLAGS            = DV_ERR_BASE + 14,
    DV_ERR_13               = DV_ERR_BASE + 15,
    DV_ERR_NOTSUPPORTED     = DV_ERR_BASE + 16,
    DV_ERR_NOMEM            = DV_ERR_BASE + 17,
    DV_ERR_ALLOCATED        = DV_ERR_BASE + 18,
    DV_ERR_BADDEVICEID      = DV_ERR_BASE + 19,
    DV_ERR_INVALHANDLE      = DV_ERR_BASE + 20,
    DV_ERR_BADERRNUM        = DV_ERR_BASE + 21,
    DV_ERR_NO_BUFFERS       = DV_ERR_BASE + 22,
    DV_ERR_MEM_CONFLICT     = DV_ERR_BASE + 23,
    DV_ERR_IO_CONFLICT      = DV_ERR_BASE + 24,
    DV_ERR_DMA_CONFLICT     = DV_ERR_BASE + 25,
    DV_ERR_INT_CONFLICT     = DV_ERR_BASE + 26,
    DV_ERR_PROTECT_ONLY     = DV_ERR_BASE + 27,
    DV_ERR_LASTERROR        = DV_ERR_BASE + 27,
    DV_ERR_USER_MSG         = DV_ERR_BASE + 1000,
}

// Callback Messages

enum {
    MM_DRVM_OPEN    = 0x3D0,
    MM_DRVM_CLOSE,
    MM_DRVM_DATA,
    MM_DRVM_ERROR,
}

enum {
    DV_VM_OPEN  = MM_DRVM_OPEN,
    DV_VM_CLOSE = MM_DRVM_CLOSE,
    DV_VM_DATA  = MM_DRVM_DATA,
    DV_VM_ERROR = MM_DRVM_ERROR,
}

/**
 * Structures
 */

struct VIDEOHDR {
    LPBYTE      lpData;
    DWORD       dwBufferLength;
    DWORD       dwBytesUsed;
    DWORD       dwTimeCaptured;
    DWORD_PTR   dwUser;
    DWORD       dwFlags;
    DWORD_PTR[4]dwReserved;
}
alias VIDEOHDR* PVIDEOHDR, LPVIDEOHDR;

enum {
    VHDR_DONE       = 0x00000001,
    VHDR_PREPARED   = 0x00000002,
    VHDR_INQUEUE    = 0x00000004,
    VHDR_KEYFRAME   = 0x00000008,
    VHDR_VALID      = 0x0000000F,
}

struct CHANNEL_CAPS {
    DWORD   dwFlags;
    DWORD   dwSrcRectXMod;
    DWORD   dwSrcRectYMod;
    DWORD   dwSrcRectWidthMod;
    DWORD   dwSrcRectHeightMod;
    DWORD   dwDstRectXMod;
    DWORD   dwDstRectYMod;
    DWORD   dwDstRectWidthMod;
    DWORD   dwDstRectHeightMod;
}
alias CHANNEL_CAPS* PCHANNEL_CAPS, LPCHANNEL_CAPS;

enum {
    VCAPS_OVERLAY       = 0x00000001,
    VCAPS_SRC_CAN_CLIP  = 0x00000002,
    VCAPS_DST_CAN_CLIP  = 0x00000004,
    VCAPS_CAN_SCALE     = 0x00000008,
}

/**
 * API Flags
 */

enum {
    VIDEO_EXTERNALIN            = 0x0001,
    VIDEO_EXTERNALOUT           = 0x0002,
    VIDEO_IN                    = 0x0004,
    VIDEO_OUT                   = 0x0008,
    VIDEO_DLG_QUERY             = 0x0010,
}

enum {
    VIDEO_CONFIGURE_QUERYSIZE   = 0x0001,
    VIDEO_CONFIGURE_CURRENT     = 0x0010,
    VIDEO_CONFIGURE_NOMINAL     = 0x0020,
    VIDEO_CONFIGURE_MIN         = 0x0040,
    VIDEO_CONFIGURE_MAX         = 0x0080,
    VIDEO_CONFIGURE_SET         = 0x1000,
    VIDEO_CONFIGURE_GET         = 0x2000,
    VIDEO_CONFIGURE_QUERY       = 0x8000,
}

/**
 * CONFIGURE MESSAGES
 */

enum {
    DVM_USER            = 0x4000,
    DVM_CONFIGURE_START = 0x1000,
    DVM_CONFIGURE_END   = 0x1FFF,
    DVM_PALETTE         = DVM_CONFIGURE_START + 1,
    DVM_FORMAT          = DVM_CONFIGURE_START + 2,
    DVM_PALETTERGB555   = DVM_CONFIGURE_START + 3,
    DVM_SRC_RECT        = DVM_CONFIGURE_START + 4,
    DVM_DST_RECT        = DVM_CONFIGURE_START + 5,
}

/**
 * AVICap window class
 */

LRESULT AVICapSM(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    if (IsWindow(hWnd)) {
        return SendMessage(hWnd, msg, wParam, lParam);
    }
    return 0;
}

enum {
    WM_CAP_START                = WM_USER,
    WM_CAP_UNICODE_START        = WM_USER + 100,

    WM_CAP_GET_CAPSTREAMPTR     = WM_CAP_START + 1,
    WM_CAP_SET_CALLBACK_ERRORA  = WM_CAP_START + 2,
    WM_CAP_SET_CALLBACK_STATUSA = WM_CAP_START + 3,

    WM_CAP_SET_CALLBACK_ERRORW  = WM_CAP_UNICODE_START + 2,
    WM_CAP_SET_CALLBACK_STATUSW = WM_CAP_UNICODE_START + 3,
}

version (Unicode) {
    alias WM_CAP_SET_CALLBACK_ERRORW    WM_CAP_SET_CALLBACK_ERROR;
    alias WM_CAP_SET_CALLBACK_STATUSW   WM_CAP_SET_CALLBACK_STATUS;
} else { // Unicode
    alias WM_CAP_SET_CALLBACK_ERRORA    WM_CAP_SET_CALLBACK_ERROR;
    alias WM_CAP_SET_CALLBACK_STATUSA   WM_CAP_SET_CALLBACK_STATUS;
}

enum {
    WM_CAP_SET_CALLBACK_YIELD       = WM_CAP_START + 4,
    WM_CAP_SET_CALLBACK_FRAME       = WM_CAP_START + 5,
    WM_CAP_SET_CALLBACK_VIDEOSTREAM = WM_CAP_START + 6,
    WM_CAP_SET_CALLBACK_WAVESTREAM  = WM_CAP_START + 7,
    WM_CAP_GET_USER_DATA            = WM_CAP_START + 8,
    WM_CAP_SET_USER_DATA            = WM_CAP_START + 9,
    WM_CAP_DRIVER_CONNECT           = WM_CAP_START + 10,
    WM_CAP_DRIVER_DISCONNECT        = WM_CAP_START + 11,
    WM_CAP_DRIVER_GET_NAMEA         = WM_CAP_START + 12,
    WM_CAP_DRIVER_GET_VERSIONA      = WM_CAP_START + 13,

    WM_CAP_DRIVER_GET_NAMEW         = WM_CAP_UNICODE_START + 12,
    WM_CAP_DRIVER_GET_VERSIONW      = WM_CAP_UNICODE_START + 13,
}

version (Unicode) {
    alias WM_CAP_DRIVER_GET_NAMEW       WM_CAP_DRIVER_GET_NAME;
    alias WM_CAP_DRIVER_GET_VERSIONW    WM_CAP_DRIVER_GET_VERSION;
} else { // Unicode
    alias WM_CAP_DRIVER_GET_NAMEA       WM_CAP_DRIVER_GET_NAME;
    alias WM_CAP_DRIVER_GET_VERSIONA    WM_CAP_DRIVER_GET_VERSION;
}

enum {
    WM_CAP_DRIVER_GET_CAPS          = WM_CAP_START + 14,
    WM_CAP_FILE_SET_CAPTURE_FILEA   = WM_CAP_START + 20,
    WM_CAP_FILE_GET_CAPTURE_FILEA   = WM_CAP_START + 21,
    WM_CAP_FILE_SAVEASA             = WM_CAP_START + 23,
    WM_CAP_FILE_SAVEDIBA            = WM_CAP_START + 25,

    WM_CAP_FILE_SET_CAPTURE_FILEW   = WM_CAP_UNICODE_START + 20,
    WM_CAP_FILE_GET_CAPTURE_FILEW   = WM_CAP_UNICODE_START + 21,
    WM_CAP_FILE_SAVEASW             = WM_CAP_UNICODE_START + 23,
    WM_CAP_FILE_SAVEDIBW            = WM_CAP_UNICODE_START + 25,
}

version (Unicode) {
    alias WM_CAP_FILE_SET_CAPTURE_FILEW WM_CAP_FILE_SET_CAPTURE_FILE;
    alias WM_CAP_FILE_GET_CAPTURE_FILEW WM_CAP_FILE_GET_CAPTURE_FILE;
    alias WM_CAP_FILE_SAVEASW           WM_CAP_FILE_SAVEAS;
    alias WM_CAP_FILE_SAVEDIBW          WM_CAP_FILE_SAVEDIB;
} else { // Unicode
    alias WM_CAP_FILE_SET_CAPTURE_FILEA WM_CAP_FILE_SET_CAPTURE_FILE;
    alias WM_CAP_FILE_GET_CAPTURE_FILEA WM_CAP_FILE_GET_CAPTURE_FILE;
    alias WM_CAP_FILE_SAVEASA           WM_CAP_FILE_SAVEAS;
    alias WM_CAP_FILE_SAVEDIBA          WM_CAP_FILE_SAVEDIB;
}

enum {
    WM_CAP_FILE_ALLOCATE        = WM_CAP_START + 22,
    WM_CAP_FILE_SET_INFOCHUNK   = WM_CAP_START + 24,
    WM_CAP_EDIT_COPY            = WM_CAP_START + 30,
    WM_CAP_SET_AUDIOFORMAT      = WM_CAP_START + 35,
    WM_CAP_GET_AUDIOFORMAT      = WM_CAP_START + 36,
    WM_CAP_DLG_VIDEOFORMAT      = WM_CAP_START + 41,
    WM_CAP_DLG_VIDEOSOURCE      = WM_CAP_START + 42,
    WM_CAP_DLG_VIDEODISPLAY     = WM_CAP_START + 43,
    WM_CAP_GET_VIDEOFORMAT      = WM_CAP_START + 44,
    WM_CAP_SET_VIDEOFORMAT      = WM_CAP_START + 45,
    WM_CAP_DLG_VIDEOCOMPRESSION = WM_CAP_START + 46,
    WM_CAP_SET_PREVIEW          = WM_CAP_START + 50,
    WM_CAP_SET_OVERLAY          = WM_CAP_START + 51,
    WM_CAP_SET_PREVIEWRATE      = WM_CAP_START + 52,
    WM_CAP_SET_SCALE            = WM_CAP_START + 53,
    WM_CAP_GET_STATUS           = WM_CAP_START + 54,
    WM_CAP_SET_SCROLL           = WM_CAP_START + 55,
    WM_CAP_GRAB_FRAME           = WM_CAP_START + 60,
    WM_CAP_GRAB_FRAME_NOSTOP    = WM_CAP_START + 61,
    WM_CAP_SEQUENCE             = WM_CAP_START + 62,
    WM_CAP_SEQUENCE_NOFILE      = WM_CAP_START + 63,
    WM_CAP_SET_SEQUENCE_SETUP   = WM_CAP_START + 64,
    WM_CAP_GET_SEQUENCE_SETUP   = WM_CAP_START + 65,
    WM_CAP_SET_MCI_DEVICEA      = WM_CAP_START + 66,
    WM_CAP_GET_MCI_DEVICEA      = WM_CAP_START + 67,

    WM_CAP_SET_MCI_DEVICEW      = WM_CAP_UNICODE_START + 66,
    WM_CAP_GET_MCI_DEVICEW      = WM_CAP_UNICODE_START + 67,
}

version (Unicode) {
    alias WM_CAP_SET_MCI_DEVICEW    WM_CAP_SET_MCI_DEVICE;
    alias WM_CAP_GET_MCI_DEVICEW    WM_CAP_GET_MCI_DEVICE;
} else { // Unicode
    alias WM_CAP_SET_MCI_DEVICEA    WM_CAP_SET_MCI_DEVICE;
    alias WM_CAP_GET_MCI_DEVICEA    WM_CAP_GET_MCI_DEVICE;
}

enum {
    WM_CAP_STOP                 = WM_CAP_START + 68,
    WM_CAP_ABORT                = WM_CAP_START + 69,
    WM_CAP_SINGLE_FRAME_OPEN    = WM_CAP_START + 70,
    WM_CAP_SINGLE_FRAME_CLOSE   = WM_CAP_START + 71,
    WM_CAP_SINGLE_FRAME         = WM_CAP_START + 72,
    WM_CAP_PAL_OPENA            = WM_CAP_START + 80,
    WM_CAP_PAL_SAVEA            = WM_CAP_START + 81,

    WM_CAP_PAL_OPENW            = WM_CAP_UNICODE_START + 80,
    WM_CAP_PAL_SAVEW            = WM_CAP_UNICODE_START + 81,
}

version (Unicode) {
    alias WM_CAP_PAL_OPENW  WM_CAP_PAL_OPEN;
    alias WM_CAP_PAL_SAVEW  WM_CAP_PAL_SAVE;
} else { // Unicode
    alias WM_CAP_PAL_OPENA  WM_CAP_PAL_OPEN;
    alias WM_CAP_PAL_SAVEA  WM_CAP_PAL_SAVE;
}

enum {
    WM_CAP_PAL_PASTE                = WM_CAP_START + 82,
    WM_CAP_PAL_AUTOCREATE           = WM_CAP_START + 83,
    WM_CAP_PAL_MANUALCREATE         = WM_CAP_START + 84,
    WM_CAP_SET_CALLBACK_CAPCONTROL  = WM_CAP_START + 85,
    WM_CAP_UNICODE_END              = WM_CAP_PAL_SAVEW,
    WM_CAP_END                      = WM_CAP_UNICODE_END,
}

/**
 * message wrapper
 */

BOOL capSetCallbackOnError(HWND hWnd, LPVOID fpProc)                { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_CALLBACK_ERROR, 0, cast(LPARAM)fpProc); }
BOOL capSetCallbackOnStatus(HWND hWnd, LPVOID fpProc)               { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_CALLBACK_STATUS, 0, cast(LPARAM)fpProc); }
BOOL capSetCallbackOnYield(HWND hWnd, LPVOID fpProc)                { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_CALLBACK_YIELD, 0, cast(LPARAM)fpProc); }
BOOL capSetCallbackOnFrame(HWND hWnd, LPVOID fpProc)                { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_CALLBACK_FRAME, 0, cast(LPARAM)fpProc); }
BOOL capSetCallbackOnVideoStream(HWND hWnd, LPVOID fpProc)          { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_CALLBACK_VIDEOSTREAM, 0, cast(LPARAM)fpProc); }
BOOL capSetCallbackOnWaveStream(HWND hWnd, LPVOID fpProc)           { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_CALLBACK_WAVESTREAM, 0, cast(LPARAM)fpProc); }
BOOL capSetCallbackOnCapControl(HWND hWnd, LPVOID fpProc)           { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_CALLBACK_CAPCONTROL, 0, cast(LPARAM)fpProc); }

BOOL capSetUserData(HWND hWnd, LPARAM lUser)                        { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_USER_DATA, 0, lUser); }
BOOL capGetUserData(HWND hWnd)                                      { return cast(BOOL)AVICapSM(hWnd, WM_CAP_GET_USER_DATA, 0, 0); }

BOOL capDriverConnect(HWND hWnd, WPARAM i)                          { return cast(BOOL)AVICapSM(hWnd, WM_CAP_DRIVER_CONNECT, i, 0); }
BOOL capDriverDisconnect(HWND hWnd)                                 { return cast(BOOL)AVICapSM(hWnd, WM_CAP_DRIVER_DISCONNECT, 0, 0); }
BOOL capDriverGetName(HWND hWnd, LPTSTR szName, WPARAM wSize)       { return cast(BOOL)AVICapSM(hWnd, WM_CAP_DRIVER_GET_NAME, wSize, cast(LPARAM)szName); }
BOOL capDriverGetVersion(HWND hWnd, LPTSTR szVer, WPARAM wSize)     { return cast(BOOL)AVICapSM(hWnd, WM_CAP_DRIVER_GET_VERSION, wSize, cast(LPARAM)szVer); }
BOOL capDriverGetCaps(HWND hWnd, LPCAPDRIVERCAPS s, WPARAM wSize)   { return cast(BOOL)AVICapSM(hWnd, WM_CAP_DRIVER_GET_CAPS, wSize, cast(LPARAM)s); }

BOOL capFileSetCaptureFile(HWND hWnd, LPTSTR szName)                { return cast(BOOL)AVICapSM(hWnd, WM_CAP_FILE_SET_CAPTURE_FILE, 0, cast(LPARAM)szName); }
BOOL capFileGetCaptureFile(HWND hWnd, LPTSTR szName, WPARAM wSize)  { return cast(BOOL)AVICapSM(hWnd, WM_CAP_FILE_GET_CAPTURE_FILE, wSize, cast(LPARAM)szName); }
BOOL capFileAlloc(HWND hWnd, WPARAM wSize)                          { return cast(BOOL)AVICapSM(hWnd, WM_CAP_FILE_ALLOCATE, wSize, 0); }
BOOL capFileSaveAs(HWND hWnd, LPTSTR szName)                        { return cast(BOOL)AVICapSM(hWnd, WM_CAP_FILE_SAVEAS, 0, cast(LPARAM)szName); }
BOOL capFileSetInfoChunk(HWND hWnd, LPCAPINFOCHUNK lpInfoChunk)     { return cast(BOOL)AVICapSM(hWnd, WM_CAP_FILE_SET_INFOCHUNK, 0, cast(LPARAM)lpInfoChunk); }
BOOL capFileSaveDIB(HWND hWnd, LPTSTR szName)                       { return cast(BOOL)AVICapSM(hWnd, WM_CAP_FILE_SAVEDIB, 0, cast(LPARAM)szName); }

BOOL capEditCopy(HWND hWnd)                                         { return cast(BOOL)AVICapSM(hWnd, WM_CAP_EDIT_COPY, 0, 0); }

BOOL capSetAudioFormat(HWND hWnd, LPWAVEFORMATEX s, WPARAM wSize)   { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_AUDIOFORMAT, wSize, cast(LPARAM)s); }
DWORD capGetAudioFormat(HWND hWnd, LPWAVEFORMATEX s, WPARAM wSize)  { return cast(DWORD)AVICapSM(hWnd, WM_CAP_GET_AUDIOFORMAT, wSize, cast(LPARAM)s); }
DWORD capGetAudioFormatSize(HWND hWnd)                              { return cast(DWORD)AVICapSM(hWnd, WM_CAP_GET_AUDIOFORMAT, 0, 0); }

BOOL capDlgVideoFormat(HWND hWnd)                                   { return cast(BOOL)AVICapSM(hWnd, WM_CAP_DLG_VIDEOFORMAT, 0, 0); }
BOOL capDlgVideoSource(HWND hWnd)                                   { return cast(BOOL)AVICapSM(hWnd, WM_CAP_DLG_VIDEOSOURCE, 0, 0); }
BOOL capDlgVideoDisplay(HWND hWnd)                                  { return cast(BOOL)AVICapSM(hWnd, WM_CAP_DLG_VIDEODISPLAY, 0, 0); }
BOOL capDlgVideoCompression(HWND hWnd)                              { return cast(BOOL)AVICapSM(hWnd, WM_CAP_DLG_VIDEOCOMPRESSION, 0, 0); }

DWORD capGetVideoFormat(HWND hWnd, void* s, WPARAM wSize)           { return cast(DWORD)AVICapSM(hWnd, WM_CAP_GET_VIDEOFORMAT, wSize, cast(LPARAM)s); }
DWORD capGetVideoFormatSize(HWND hWnd)                              { return cast(DWORD)AVICapSM(hWnd, WM_CAP_GET_VIDEOFORMAT, 0, 0); }
BOOL capSetVideoFormat(HWND hWnd, void* s, WPARAM wSize)            { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_VIDEOFORMAT, wSize, cast(LPARAM)s); }

BOOL capPreview(HWND hWnd, BOOL f)                                  { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_PREVIEW, cast(WPARAM)f, 0); }
BOOL capPreviewRate(HWND hWnd, WPARAM wMS)                          { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_PREVIEWRATE, wMS, 0); }
BOOL capOverlay(HWND hWnd, BOOL f)                                  { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_OVERLAY, cast(WPARAM)f, 0); }
BOOL capPreviewScale(HWND hWnd, BOOL f)                             { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_SCALE, cast(WPARAM)f, 0); }
BOOL capGetStatus(HWND hWnd, LPCAPSTATUS s, WPARAM wSize)           { return cast(BOOL)AVICapSM(hWnd, WM_CAP_GET_STATUS, wSize, cast(LPARAM)s); }
BOOL capSetScrollPos(HWND hWnd, LPPOINT lpP)                        { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_SCROLL, 0, cast(LPARAM)lpP); }

BOOL capGrabFrame(HWND hWnd)                                        { return cast(BOOL)AVICapSM(hWnd, WM_CAP_GRAB_FRAME, 0, 0); }
BOOL capGrabFrameNoStop(HWND hWnd)                                  { return cast(BOOL)AVICapSM(hWnd, WM_CAP_GRAB_FRAME_NOSTOP, 0, 0); }

BOOL capCaptureSequence(HWND hWnd)                                  { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SEQUENCE, 0, 0); }
BOOL capCaptureSequenceNoFile(HWND hWnd)                            { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SEQUENCE_NOFILE, 0, 0); }
BOOL capCaptureStop(HWND hWnd)                                      { return cast(BOOL)AVICapSM(hWnd, WM_CAP_STOP, 0, 0); }
BOOL capCaptureAbort(HWND hWnd)                                     { return cast(BOOL)AVICapSM(hWnd, WM_CAP_ABORT, 0, 0); }

BOOL capCaptureSingleFrameOpen(HWND hWnd)                           { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SINGLE_FRAME_OPEN, 0, 0); }
BOOL capCaptureSingleFrameClose(HWND hWnd)                          { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SINGLE_FRAME_CLOSE, 0, 0); }
BOOL capCaptureSingleFrame(HWND hWnd)                               { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SINGLE_FRAME, 0, 0); }

BOOL capCaptureGetSetup(HWND hWnd, LPCAPTUREPARMS s, WPARAM wSize)  { return cast(BOOL)AVICapSM(hWnd, WM_CAP_GET_SEQUENCE_SETUP, wSize, cast(LPARAM)s); }
BOOL capCaptureSetSetup(HWND hWnd, LPCAPTUREPARMS s, WPARAM wSize)  { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_SEQUENCE_SETUP, wSize, cast(LPARAM)s); }

BOOL capSetMCIDeviceName(HWND hWnd, LPTSTR szName)                  { return cast(BOOL)AVICapSM(hWnd, WM_CAP_SET_MCI_DEVICE, 0, cast(LPARAM)szName); }
BOOL capGetMCIDeviceName(HWND hWnd, LPTSTR szName, WPARAM wSize)    { return cast(BOOL)AVICapSM(hWnd, WM_CAP_GET_MCI_DEVICE, wSize, cast(LPARAM)szName); }

BOOL capPaletteOpen(HWND hWnd, LPTSTR szName)                       { return cast(BOOL)AVICapSM(hWnd, WM_CAP_PAL_OPEN, 0, cast(LPARAM)szName); }
BOOL capPaletteSave(HWND hWnd, LPTSTR szName)                       { return cast(BOOL)AVICapSM(hWnd, WM_CAP_PAL_SAVE, 0, cast(LPARAM)szName); }
BOOL capPalettePaste(HWND hWnd)                                     { return cast(BOOL)AVICapSM(hWnd, WM_CAP_PAL_PASTE, 0, 0); }
BOOL capPaletteAuto(HWND hWnd, WPARAM iFrames, LPARAM iColors)      { return cast(BOOL)AVICapSM(hWnd, WM_CAP_PAL_AUTOCREATE, iFrames, iColors); }
BOOL capPaletteManual(HWND hWnd, WPARAM fGrab, LPARAM iColors)      { return cast(BOOL)AVICapSM(hWnd, WM_CAP_PAL_MANUALCREATE, fGrab, iColors); }

/**
 * structs
 */

struct CAPDRIVERCAPS {
    UINT    wDeviceIndex;
    BOOL    fHasOverlay;
    BOOL    fHasDlgVideoSource;
    BOOL    fHasDlgVideoFormat;
    BOOL    fHasDlgVideoDisplay;
    BOOL    fCaptureInitialized;
    BOOL    fDriverSuppliesPalettes;
    HANDLE  hVideoIn;
    HANDLE  hVideoOut;
    HANDLE  hVideoExtIn;
    HANDLE  hVideoExtOut;
}
alias CAPDRIVERCAPS* PCAPDRIVERCAPS, LPCAPDRIVERCAPS;

struct CAPSTATUS {
    UINT        uiImageWidth;
    UINT        uiImageHeight;
    BOOL        fLiveWindow;
    BOOL        fOverlayWindow;
    BOOL        fScale;
    POINT       ptScroll;
    BOOL        fUsingDefaultPalette;
    BOOL        fAudioHardware;
    BOOL        fCapFileExists;
    DWORD       dwCurrentVideoFrame;
    DWORD       dwCurrentVideoFramesDropped;
    DWORD       dwCurrentWaveSamples;
    DWORD       dwCurrentTimeElapsedMS;
    HPALETTE    hPalCurrent;
    BOOL        fCapturingNow;
    DWORD       dwReturn;
    UINT        wNumVideoAllocated;
    UINT        wNumAudioAllocated;
}
alias CAPSTATUS* PCAPSTATUS, LPCAPSTATUS;

struct CAPTUREPARMS {
    DWORD   dwRequestMicroSecPerFrame;
    BOOL    fMakeUserHitOKToCapture;
    UINT    wPercentDropForError;
    BOOL    fYield;
    DWORD   dwIndexSize;
    UINT    wChunkGranularity;
    BOOL    fUsingDOSMemory;
    UINT    wNumVideoRequested;
    BOOL    fCaptureAudio;
    UINT    wNumAudioRequested;
    UINT    vKeyAbort;
    BOOL    fAbortLeftMouse;
    BOOL    fAbortRightMouse;
    BOOL    fLimitEnabled;
    UINT    wTimeLimit;
    BOOL    fMCIControl;
    BOOL    fStepMCIDevice;
    DWORD   dwMCIStartTime;
    DWORD   dwMCIStopTime;
    BOOL    fStepCaptureAt2x;
    UINT    wStepCaptureAverageFrames;
    DWORD   dwAudioBufferSize;
    BOOL    fDisableWriteCache;
    UINT    AVStreamMaster;
}
alias CAPTUREPARMS* PCAPTUREPARMS, LPCAPTUREPARMS;

enum AVSTREAMMASTER_AUDIO = 0;
enum AVSTREAMMASTER_NONE  = 1;

struct CAPINFOCHUNK {
    FOURCC  fccInfoID;
    LPVOID  lpData;
    LONG    cbData;
}
alias CAPINFOCHUNK* PCAPINFOCHUNK, LPCAPINFOCHUNK;

// Callback Definitions

extern (Windows) {
    alias LRESULT function(HWND hWnd) CAPYIELDCALLBACK;
    alias LRESULT function(HWND hWnd, int nID, LPCWSTR lpsz) CAPSTATUSCALLBACKW;
    alias LRESULT function(HWND hWnd, int nID, LPCWSTR lpsz) CAPERRORCALLBACKW;
    alias LRESULT function(HWND hWnd, int nID, LPCSTR lpsz) CAPSTATUSCALLBACKA;
    alias LRESULT function(HWND hWnd, int nID, LPCSTR lpsz) CAPERRORCALLBACKA;
}

version (Unicode) {
    alias CAPSTATUSCALLBACKW    CAPSTATUSCALLBACK;
    alias CAPERRORCALLBACKW     CAPERRORCALLBACK;
} else { // Unicode
    alias CAPSTATUSCALLBACKA    CAPSTATUSCALLBACK;
    alias CAPERRORCALLBACKA     CAPERRORCALLBACK;
}

extern (Windows) {
    alias LRESULT function(HWND hWnd, LPVIDEOHDR lpVHdr) CAPVIDEOCALLBACK;
    alias LRESULT function(HWND hWnd, LPWAVEHDR lpWHdr) CAPWAVECALLBACK;
    alias LRESULT function(HWND hWnd, int nState) CAPCONTROLCALLBACK;
}

//  CapControlCallback states
enum CONTROLCALLBACK_PREROLL   = 1;
enum CONTROLCALLBACK_CAPTURING = 2;

extern (Windows) {
    HWND capCreateCaptureWindowA(LPCSTR lpszWindowName, DWORD dwStyle, int x, int y, int nWidth, int nHeight, HWND hwndParent, int nID);
    BOOL capGetDriverDescriptionA(UINT wDriverIndex, LPSTR lpszName, int cbName, LPSTR lpszVer, int cbVer);
    HWND capCreateCaptureWindowW(LPCWSTR lpszWindowName, DWORD dwStyle, int x, int y, int nWidth, int nHeight, HWND hwndParent, int nID);
    BOOL capGetDriverDescriptionW(UINT wDriverIndex, LPWSTR lpszName, int cbName, LPWSTR lpszVer, int cbVer);
}

version (Unicode) {
    alias capCreateCaptureWindowW   capCreateCaptureWindow;
    alias capGetDriverDescriptionW  capGetDriverDescription;
} else { // Unicode
    alias capCreateCaptureWindowA   capCreateCaptureWindow;
    alias capGetDriverDescriptionA  capGetDriverDescription;
}

// New Information chunk IDs
enum infotypeDIGITIZATION_TIME = mmioFOURCC!('I', 'D', 'I', 'T');
enum infotypeSMPTE_TIME        = mmioFOURCC!('I', 'S', 'M', 'P');

// status and error callbacks
enum {
    IDS_CAP_BEGIN                   = 300,
    IDS_CAP_END                     = 301,

    IDS_CAP_INFO                    = 401,
    IDS_CAP_OUTOFMEM                = 402,
    IDS_CAP_FILEEXISTS              = 403,
    IDS_CAP_ERRORPALOPEN            = 404,
    IDS_CAP_ERRORPALSAVE            = 405,
    IDS_CAP_ERRORDIBSAVE            = 406,
    IDS_CAP_DEFAVIEXT               = 407,
    IDS_CAP_DEFPALEXT               = 408,
    IDS_CAP_CANTOPEN                = 409,
    IDS_CAP_SEQ_MSGSTART            = 410,
    IDS_CAP_SEQ_MSGSTOP             = 411,

    IDS_CAP_VIDEDITERR              = 412,
    IDS_CAP_READONLYFILE            = 413,
    IDS_CAP_WRITEERROR              = 414,
    IDS_CAP_NODISKSPACE             = 415,
    IDS_CAP_SETFILESIZE             = 416,
    IDS_CAP_SAVEASPERCENT           = 417,

    IDS_CAP_DRIVER_ERROR            = 418,

    IDS_CAP_WAVE_OPEN_ERROR         = 419,
    IDS_CAP_WAVE_ALLOC_ERROR        = 420,
    IDS_CAP_WAVE_PREPARE_ERROR      = 421,
    IDS_CAP_WAVE_ADD_ERROR          = 422,
    IDS_CAP_WAVE_SIZE_ERROR         = 423,

    IDS_CAP_VIDEO_OPEN_ERROR        = 424,
    IDS_CAP_VIDEO_ALLOC_ERROR       = 425,
    IDS_CAP_VIDEO_PREPARE_ERROR     = 426,
    IDS_CAP_VIDEO_ADD_ERROR         = 427,
    IDS_CAP_VIDEO_SIZE_ERROR        = 428,

    IDS_CAP_FILE_OPEN_ERROR         = 429,
    IDS_CAP_FILE_WRITE_ERROR        = 430,
    IDS_CAP_RECORDING_ERROR         = 431,
    IDS_CAP_RECORDING_ERROR2        = 432,
    IDS_CAP_AVI_INIT_ERROR          = 433,
    IDS_CAP_NO_FRAME_CAP_ERROR      = 434,
    IDS_CAP_NO_PALETTE_WARN         = 435,
    IDS_CAP_MCI_CONTROL_ERROR       = 436,
    IDS_CAP_MCI_CANT_STEP_ERROR     = 437,
    IDS_CAP_NO_AUDIO_CAP_ERROR      = 438,
    IDS_CAP_AVI_DRAWDIB_ERROR       = 439,
    IDS_CAP_COMPRESSOR_ERROR        = 440,
    IDS_CAP_AUDIO_DROP_ERROR        = 441,
    IDS_CAP_AUDIO_DROP_COMPERROR    = 442,

    IDS_CAP_STAT_LIVE_MODE          = 500,
    IDS_CAP_STAT_OVERLAY_MODE       = 501,
    IDS_CAP_STAT_CAP_INIT           = 502,
    IDS_CAP_STAT_CAP_FINI           = 503,
    IDS_CAP_STAT_PALETTE_BUILD      = 504,
    IDS_CAP_STAT_OPTPAL_BUILD       = 505,
    IDS_CAP_STAT_I_FRAMES           = 506,
    IDS_CAP_STAT_L_FRAMES           = 507,
    IDS_CAP_STAT_CAP_L_FRAMES       = 508,
    IDS_CAP_STAT_CAP_AUDIO          = 509,
    IDS_CAP_STAT_VIDEOCURRENT       = 510,
    IDS_CAP_STAT_VIDEOAUDIO         = 511,
    IDS_CAP_STAT_VIDEOONLY          = 512,
    IDS_CAP_STAT_FRAMESDROPPED      = 513,
}

/**
 * FilePreview dialog.
 */

extern (Windows) {
    BOOL GetOpenFileNamePreviewA(LPOPENFILENAMEA lpofn);
    BOOL GetSaveFileNamePreviewA(LPOPENFILENAMEA lpofn);
    BOOL GetOpenFileNamePreviewW(LPOPENFILENAMEW lpofn);
    BOOL GetSaveFileNamePreviewW(LPOPENFILENAMEW lpofn);
}

version (Unicode) {
    alias GetOpenFileNamePreviewW   GetOpenFileNamePreview;
    alias GetSaveFileNamePreviewW   GetSaveFileNamePreview;
} else { // Unicode
    alias GetOpenFileNamePreviewA   GetOpenFileNamePreview;
    alias GetSaveFileNamePreviewA   GetSaveFileNamePreview;
}

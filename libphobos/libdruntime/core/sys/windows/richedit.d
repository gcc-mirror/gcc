/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_richedit.d)
 */
module core.sys.windows.richedit;
version (Windows):

version (ANSI) {} else version = Unicode;

private import core.sys.windows.windef, core.sys.windows.winuser;
private import core.sys.windows.wingdi; // for LF_FACESIZE

align(4):

version (Unicode) {
const wchar[] RICHEDIT_CLASS = "RichEdit20W";
} else {
const char[] RICHEDIT_CLASS  = "RichEdit20A";
}

enum RICHEDIT_CLASS10A = "RICHEDIT";

const TCHAR[]
    CF_RTF       = "Rich Text Format",
    CF_RTFNOOBJS = "Rich Text Format Without Objects",
    CF_RETEXTOBJ = "RichEdit Text and Objects";

enum DWORD
    CFM_BOLD        = 1,
    CFM_ITALIC      = 2,
    CFM_UNDERLINE   = 4,
    CFM_STRIKEOUT   = 8,
    CFM_PROTECTED   = 16,
    CFM_LINK        = 32,
    CFM_SIZE        = 0x80000000,
    CFM_COLOR       = 0x40000000,
    CFM_FACE        = 0x20000000,
    CFM_OFFSET      = 0x10000000,
    CFM_CHARSET     = 0x08000000,
    CFM_SUBSCRIPT   = 0x00030000,
    CFM_SUPERSCRIPT = 0x00030000;

enum DWORD
    CFE_BOLD        = 1,
    CFE_ITALIC      = 2,
    CFE_UNDERLINE   = 4,
    CFE_STRIKEOUT   = 8,
    CFE_PROTECTED   = 16,
    CFE_SUBSCRIPT   = 0x00010000,
    CFE_SUPERSCRIPT = 0x00020000,
    CFE_AUTOCOLOR   = 0x40000000;

enum CFM_EFFECTS = CFM_BOLD | CFM_ITALIC | CFM_UNDERLINE | CFM_COLOR
  | CFM_STRIKEOUT | CFE_PROTECTED | CFM_LINK;

// flags for EM_SETIMEOPTIONS
enum LPARAM
    IMF_FORCENONE         = 1,
    IMF_FORCEENABLE       = 2,
    IMF_FORCEDISABLE      = 4,
    IMF_CLOSESTATUSWINDOW = 8,
    IMF_VERTICAL          = 32,
    IMF_FORCEACTIVE       = 64,
    IMF_FORCEINACTIVE     = 128,
    IMF_FORCEREMEMBER     = 256;

enum SEL_EMPTY=0;
enum SEL_TEXT=1;
enum SEL_OBJECT=2;
enum SEL_MULTICHAR=4;
enum SEL_MULTIOBJECT=8;

enum MAX_TAB_STOPS=32;

enum PFM_ALIGNMENT=8;
enum PFM_NUMBERING=32;
enum PFM_OFFSET=4;
enum PFM_OFFSETINDENT=0x80000000;
enum PFM_RIGHTINDENT=2;
enum PFM_STARTINDENT=1;
enum PFM_TABSTOPS=16;
enum PFM_BORDER=2048;
enum PFM_LINESPACING=256;
enum PFM_NUMBERINGSTART=32768;
enum PFM_NUMBERINGSTYLE=8192;
enum PFM_NUMBERINGTAB=16384;
enum PFM_SHADING=4096;
enum PFM_SPACEAFTER=128;
enum PFM_SPACEBEFORE=64;
enum PFM_STYLE=1024;
enum PFM_DONOTHYPHEN=4194304;
enum PFM_KEEP=131072;
enum PFM_KEEPNEXT=262144;
enum PFM_NOLINENUMBER=1048576;
enum PFM_NOWIDOWCONTROL=2097152;
enum PFM_PAGEBREAKBEFORE=524288;
enum PFM_RTLPARA=65536;
enum PFM_SIDEBYSIDE=8388608;
enum PFM_TABLE=1073741824;
enum PFN_BULLET=1;

enum PFE_DONOTHYPHEN=64;
enum PFE_KEEP=2;
enum PFE_KEEPNEXT=4;
enum PFE_NOLINENUMBER=16;
enum PFE_NOWIDOWCONTROL=32;
enum PFE_PAGEBREAKBEFORE=8;
enum PFE_RTLPARA=1;
enum PFE_SIDEBYSIDE=128;
enum PFE_TABLE=16384;
enum PFA_LEFT=1;
enum PFA_RIGHT=2;
enum PFA_CENTER=3;
enum PFA_JUSTIFY=4;
enum PFA_FULL_INTERWORD=4;

enum SF_TEXT=1;
enum SF_RTF=2;
enum SF_RTFNOOBJS=3;
enum SF_TEXTIZED=4;
enum SF_UNICODE=16;
enum SF_USECODEPAGE=32;
enum SF_NCRFORNONASCII=64;
enum SF_RTFVAL=0x0700;

enum SFF_PWD=0x0800;
enum SFF_KEEPDOCINFO=0x1000;
enum SFF_PERSISTVIEWSCALE=0x2000;
enum SFF_PLAINRTF=0x4000;
enum SFF_SELECTION=0x8000;

enum WB_CLASSIFY      = 3;
enum WB_MOVEWORDLEFT  = 4;
enum WB_MOVEWORDRIGHT = 5;
enum WB_LEFTBREAK     = 6;
enum WB_RIGHTBREAK    = 7;
enum WB_MOVEWORDPREV  = 4;
enum WB_MOVEWORDNEXT  = 5;
enum WB_PREVBREAK     = 6;
enum WB_NEXTBREAK     = 7;

enum WBF_WORDWRAP  = 16;
enum WBF_WORDBREAK = 32;
enum WBF_OVERFLOW  = 64;
enum WBF_LEVEL1    = 128;
enum WBF_LEVEL2    = 256;
enum WBF_CUSTOM    = 512;

enum ES_DISABLENOSCROLL  = 8192;
enum ES_SUNKEN           = 16384;
enum ES_SAVESEL          = 32768;
enum ES_EX_NOCALLOLEINIT = 16777216;
enum ES_NOIME            = 524288;
enum ES_NOOLEDRAGDROP    = 8;
enum ES_SELECTIONBAR     = 16777216;
enum ES_SELFIME          = 262144;
enum ES_VERTICAL         = 4194304;

enum EM_CANPASTE = WM_USER+50;
enum EM_DISPLAYBAND = WM_USER+51;
enum EM_EXGETSEL = WM_USER+52;
enum EM_EXLIMITTEXT = WM_USER+53;
enum EM_EXLINEFROMCHAR = WM_USER+54;
enum EM_EXSETSEL = WM_USER+55;
enum EM_FINDTEXT = WM_USER+56;
enum EM_FORMATRANGE = WM_USER+57;
enum EM_GETCHARFORMAT = WM_USER+58;
enum EM_GETEVENTMASK = WM_USER+59;
enum EM_GETOLEINTERFACE = WM_USER+60;
enum EM_GETPARAFORMAT = WM_USER+61;
enum EM_GETSELTEXT = WM_USER+62;
enum EM_HIDESELECTION = WM_USER+63;
enum EM_PASTESPECIAL = WM_USER+64;
enum EM_REQUESTRESIZE = WM_USER+65;
enum EM_SELECTIONTYPE = WM_USER+66;
enum EM_SETBKGNDCOLOR = WM_USER+67;
enum EM_SETCHARFORMAT = WM_USER+68;
enum EM_SETEVENTMASK = WM_USER+69;
enum EM_SETOLECALLBACK = WM_USER+70;
enum EM_SETPARAFORMAT = WM_USER+71;
enum EM_SETTARGETDEVICE = WM_USER+72;
enum EM_STREAMIN = WM_USER+73;
enum EM_STREAMOUT = WM_USER+74;
enum EM_GETTEXTRANGE = WM_USER+75;
enum EM_FINDWORDBREAK = WM_USER+76;
enum EM_SETOPTIONS = WM_USER+77;
enum EM_GETOPTIONS = WM_USER+78;
enum EM_FINDTEXTEX = WM_USER+79;
enum EM_GETWORDBREAKPROCEX = WM_USER+80;
enum EM_SETWORDBREAKPROCEX = WM_USER+81;
/* RichEdit 2.0 messages */
enum EM_SETUNDOLIMIT = WM_USER+82;
enum EM_REDO = WM_USER+84;
enum EM_CANREDO = WM_USER+85;
enum EM_GETUNDONAME = WM_USER+86;
enum EM_GETREDONAME = WM_USER+87;
enum EM_STOPGROUPTYPING = WM_USER+88;
enum EM_SETTEXTMODE = WM_USER+89;
enum EM_GETTEXTMODE = WM_USER+90;
enum EM_AUTOURLDETECT = WM_USER+91;
enum EM_GETAUTOURLDETECT = WM_USER + 92;
enum EM_SETPALETTE = WM_USER + 93;
enum EM_GETTEXTEX = WM_USER+94;
enum EM_GETTEXTLENGTHEX = WM_USER+95;
enum EM_SHOWSCROLLBAR = WM_USER+96;
enum EM_SETTEXTEX = WM_USER + 97;
enum EM_SETPUNCTUATION = WM_USER + 100;
enum EM_GETPUNCTUATION = WM_USER + 101;
enum EM_SETWORDWRAPMODE = WM_USER + 102;
enum EM_GETWORDWRAPMODE = WM_USER + 103;
enum EM_SETIMECOLOR = WM_USER + 104;
enum EM_GETIMECOLOR = WM_USER + 105;
enum EM_SETIMEOPTIONS = WM_USER + 106;
enum EM_GETIMEOPTIONS = WM_USER + 107;
enum EM_SETLANGOPTIONS = WM_USER+120;
enum EM_GETLANGOPTIONS = WM_USER+121;
enum EM_GETIMECOMPMODE = WM_USER+122;
enum EM_FINDTEXTW = WM_USER + 123;
enum EM_FINDTEXTEXW = WM_USER + 124;
enum EM_RECONVERSION = WM_USER + 125;
enum EM_SETBIDIOPTIONS = WM_USER + 200;
enum EM_GETBIDIOPTIONS = WM_USER + 201;
enum EM_SETTYPOGRAPHYOPTIONS = WM_USER+202;
enum EM_GETTYPOGRAPHYOPTIONS = WM_USER+203;
enum EM_SETEDITSTYLE = WM_USER + 204;
enum EM_GETEDITSTYLE = WM_USER + 205;
enum EM_GETSCROLLPOS = WM_USER+221;
enum EM_SETSCROLLPOS = WM_USER+222;
enum EM_SETFONTSIZE = WM_USER+223;
enum EM_GETZOOM = WM_USER+224;
enum EM_SETZOOM = WM_USER+225;

enum EN_MSGFILTER     = 1792;
enum EN_REQUESTRESIZE = 1793;
enum EN_SELCHANGE     = 1794;
enum EN_DROPFILES     = 1795;
enum EN_PROTECTED     = 1796;
enum EN_CORRECTTEXT   = 1797;
enum EN_STOPNOUNDO    = 1798;
enum EN_IMECHANGE     = 1799;
enum EN_SAVECLIPBOARD = 1800;
enum EN_OLEOPFAILED   = 1801;
enum EN_LINK          = 1803;

enum ENM_NONE            = 0;
enum ENM_CHANGE          = 1;
enum ENM_UPDATE          = 2;
enum ENM_SCROLL          = 4;
enum ENM_SCROLLEVENTS    = 8;
enum ENM_DRAGDROPDONE    = 16;
enum ENM_KEYEVENTS       = 65536;
enum ENM_MOUSEEVENTS     = 131072;
enum ENM_REQUESTRESIZE   = 262144;
enum ENM_SELCHANGE       = 524288;
enum ENM_DROPFILES       = 1048576;
enum ENM_PROTECTED       = 2097152;
enum ENM_CORRECTTEXT     = 4194304;
enum ENM_IMECHANGE       = 8388608;
enum ENM_LANGCHANGE      = 16777216;
enum ENM_OBJECTPOSITIONS = 33554432;
enum ENM_LINK            = 67108864;

enum ECO_AUTOWORDSELECTION=1;
enum ECO_AUTOVSCROLL=64;
enum ECO_AUTOHSCROLL=128;
enum ECO_NOHIDESEL=256;
enum ECO_READONLY=2048;
enum ECO_WANTRETURN=4096;
enum ECO_SAVESEL=0x8000;
enum ECO_SELECTIONBAR=0x1000000;
enum ECO_VERTICAL=0x400000;

enum {
    ECOOP_SET = 1,
    ECOOP_OR,
    ECOOP_AND,
    ECOOP_XOR
}

enum SCF_DEFAULT    = 0;
enum SCF_SELECTION  = 1;
enum SCF_WORD       = 2;
enum SCF_ALL        = 4;
enum SCF_USEUIRULES = 8;

alias DWORD TEXTMODE;
enum TM_PLAINTEXT=1;
enum TM_RICHTEXT=2;
enum TM_SINGLELEVELUNDO=4;
enum TM_MULTILEVELUNDO=8;
enum TM_SINGLECODEPAGE=16;
enum TM_MULTICODEPAGE=32;

enum GT_DEFAULT=0;
enum GT_USECRLF=1;

enum yHeightCharPtsMost=1638;
enum lDefaultTab=720;

alias DWORD UNDONAMEID;
enum UID_UNKNOWN    = 0;
enum UID_TYPING     = 1;
enum UID_DELETE     = 2;
enum UID_DRAGDROP   = 3;
enum UID_CUT        = 4;
enum UID_PASTE      = 5;

struct CHARFORMATA {
    UINT cbSize = this.sizeof;
    DWORD dwMask;
    DWORD dwEffects;
    LONG yHeight;
    LONG yOffset;
    COLORREF crTextColor;
    BYTE bCharSet;
    BYTE bPitchAndFamily;
    char[LF_FACESIZE] szFaceName = 0;
}
struct CHARFORMATW {
    UINT cbSize = this.sizeof;
    DWORD dwMask;
    DWORD dwEffects;
    LONG yHeight;
    LONG yOffset;
    COLORREF crTextColor;
    BYTE bCharSet;
    BYTE bPitchAndFamily;
    WCHAR[LF_FACESIZE] szFaceName = 0;
}

struct CHARFORMAT2A {
    UINT cbSize = this.sizeof;
    DWORD dwMask;
    DWORD dwEffects;
    LONG yHeight;
    LONG yOffset;
    COLORREF crTextColor;
    BYTE bCharSet;
    BYTE bPitchAndFamily;
    char[LF_FACESIZE] szFaceName = 0;
    WORD wWeight;
    SHORT sSpacing;
    COLORREF crBackColor;
    LCID lcid;
    DWORD dwReserved;
    SHORT sStyle;
    WORD wKerning;
    BYTE bUnderlineType;
    BYTE bAnimation;
    BYTE bRevAuthor;
}

struct CHARFORMAT2W {
    UINT cbSize = this.sizeof;
    DWORD dwMask;
    DWORD dwEffects;
    LONG yHeight;
    LONG yOffset;
    COLORREF crTextColor;
    BYTE bCharSet;
    BYTE bPitchAndFamily;
    WCHAR[LF_FACESIZE] szFaceName = 0;
    WORD wWeight;
    SHORT sSpacing;
    COLORREF crBackColor;
    LCID lcid;
    DWORD dwReserved;
    SHORT sStyle;
    WORD wKerning;
    BYTE bUnderlineType;
    BYTE bAnimation;
    BYTE bRevAuthor;
}

struct CHARRANGE {
    LONG cpMin;
    LONG cpMax;
}

struct COMPCOLOR {
    COLORREF crText;
    COLORREF crBackground;
    DWORD dwEffects;
}

extern (Windows) {
    alias DWORD function(DWORD_PTR,PBYTE,LONG,LONG*) EDITSTREAMCALLBACK;
}

struct EDITSTREAM {
align(4):
    DWORD_PTR dwCookie;
    DWORD dwError;
    EDITSTREAMCALLBACK pfnCallback;
}

struct ENCORRECTTEXT {
align(4):
    NMHDR nmhdr;
    CHARRANGE chrg;
    WORD seltyp;
}

struct ENDROPFILES {
align(4):
    NMHDR nmhdr;
    HANDLE hDrop;
    LONG cp;
    BOOL fProtected;
}

struct ENLINK {
align(4):
    NMHDR nmhdr;
    UINT msg;
    WPARAM wParam;
    LPARAM lParam;
    CHARRANGE chrg;
}

struct ENOLEOPFAILED {
align(4):
    NMHDR nmhdr;
    LONG iob;
    LONG lOper;
    HRESULT hr;
}

struct ENPROTECTED {
align(4):
    NMHDR nmhdr;
    UINT msg;
    WPARAM wParam;
    LPARAM lParam;
    CHARRANGE chrg;
}
alias ENPROTECTED* LPENPROTECTED;

struct ENSAVECLIPBOARD {
align(4):
    NMHDR nmhdr;
    LONG cObjectCount;
    LONG cch;
}

struct FINDTEXTA {
    CHARRANGE chrg;
    LPSTR lpstrText;
}

struct FINDTEXTW {
    CHARRANGE chrg;
    LPWSTR lpstrText;
}

struct FINDTEXTEXA {
    CHARRANGE chrg;
    LPSTR lpstrText;
    CHARRANGE chrgText;
}

struct FINDTEXTEXW {
    CHARRANGE chrg;
    LPWSTR lpstrText;
    CHARRANGE chrgText;
}

struct FORMATRANGE {
    HDC hdc;
    HDC hdcTarget;
    RECT rc;
    RECT rcPage;
    CHARRANGE chrg;
}

struct MSGFILTER {
align(4):
    NMHDR nmhdr;
    UINT msg;
    WPARAM wParam;
    LPARAM lParam;
}

struct PARAFORMAT {
    UINT cbSize = this.sizeof;
    DWORD dwMask;
    WORD wNumbering;
    WORD wReserved;
    LONG dxStartIndent;
    LONG dxRightIndent;
    LONG dxOffset;
    WORD wAlignment;
    SHORT cTabCount;
    LONG[MAX_TAB_STOPS] rgxTabs;
}

struct PARAFORMAT2 {
    UINT cbSize = this.sizeof;
    DWORD dwMask;
    WORD wNumbering;
    WORD wEffects;
    LONG dxStartIndent;
    LONG dxRightIndent;
    LONG dxOffset;
    WORD wAlignment;
    SHORT cTabCount;
    LONG[MAX_TAB_STOPS] rgxTabs;
    LONG dySpaceBefore;
    LONG dySpaceAfter;
    LONG dyLineSpacing;
    SHORT sStype;
    BYTE bLineSpacingRule;
    BYTE bOutlineLevel;
    WORD wShadingWeight;
    WORD wShadingStyle;
    WORD wNumberingStart;
    WORD wNumberingStyle;
    WORD wNumberingTab;
    WORD wBorderSpace;
    WORD wBorderWidth;
    WORD wBorders;
}

struct SELCHANGE {
    NMHDR nmhdr;
    CHARRANGE chrg;
    WORD seltyp;
}

struct TEXTRANGEA {
    CHARRANGE chrg;
    LPSTR lpstrText;
}

struct TEXTRANGEW {
    CHARRANGE chrg;
    LPWSTR lpstrText;
}

struct REQRESIZE {
    NMHDR nmhdr;
    RECT rc;
}

struct REPASTESPECIAL {
align(4):
    DWORD dwAspect;
    DWORD_PTR dwParam;
}

struct PUNCTUATION {
align(4):
    UINT iSize;
    LPSTR szPunctuation;
}

struct GETTEXTEX {
align(4):
    DWORD cb;
    DWORD flags;
    UINT codepage;
    LPCSTR lpDefaultChar;
    LPBOOL lpUsedDefChar;
}

extern (Windows) {
alias LONG function(char*,LONG,BYTE,INT) EDITWORDBREAKPROCEX;
}

/* Defines for EM_SETTYPOGRAPHYOPTIONS */
enum TO_ADVANCEDTYPOGRAPHY = 1;
enum TO_SIMPLELINEBREAK    = 2;

/* Defines for GETTEXTLENGTHEX */
enum GTL_DEFAULT  = 0;
enum GTL_USECRLF  = 1;
enum GTL_PRECISE  = 2;
enum GTL_CLOSE    = 4;
enum GTL_NUMCHARS = 8;
enum GTL_NUMBYTES = 16;

struct GETTEXTLENGTHEX {
align(4):
    DWORD flags;
    UINT codepage;
}

version (Unicode) {
    alias CHARFORMATW CHARFORMAT;
    alias CHARFORMAT2W CHARFORMAT2;
    alias FINDTEXTW FINDTEXT;
    alias FINDTEXTEXW FINDTEXTEX;
    alias TEXTRANGEW TEXTRANGE;
} else {
    alias CHARFORMATA CHARFORMAT;
    alias CHARFORMAT2A CHARFORMAT2;
    alias FINDTEXTA FINDTEXT;
    alias FINDTEXTEXA FINDTEXTEX;
    alias TEXTRANGEA TEXTRANGE;
}

/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_imagehlp.d)
 */
module core.sys.windows.imagehlp;
version (Windows):

version (ANSI) {} else version = Unicode;

/* Comment from MinGW
    NOTE: This strictly does not belong in the Win32 API since it's
    really part of Platform SDK. However, GDB needs it and we might
    as well provide it here.
*/

private import core.sys.windows.winbase, core.sys.windows.windef;

// FIXME: check types of constants

enum API_VERSION_NUMBER = 7;

enum BIND_NO_BOUND_IMPORTS  = 1;
enum BIND_NO_UPDATE         = 2;
enum BIND_ALL_IMAGES        = 4;
enum BIND_CACHE_IMPORT_DLLS = 8;

enum {
    CBA_DEFERRED_SYMBOL_LOAD_START = 1,
    CBA_DEFERRED_SYMBOL_LOAD_COMPLETE,
    CBA_DEFERRED_SYMBOL_LOAD_FAILURE,
    CBA_SYMBOLS_UNLOADED,
    CBA_DUPLICATE_SYMBOL
}

enum CERT_PE_IMAGE_DIGEST_DEBUG_INFO      = 1;
enum CERT_PE_IMAGE_DIGEST_RESOURCES       = 2;
enum CERT_PE_IMAGE_DIGEST_ALL_IMPORT_INFO = 4;
enum CERT_PE_IMAGE_DIGEST_NON_PE_INFO     = 8;

enum CERT_SECTION_TYPE_ANY = 255;

enum {
    CHECKSUM_SUCCESS = 0,
    CHECKSUM_OPEN_FAILURE,
    CHECKSUM_MAP_FAILURE,
    CHECKSUM_MAPVIEW_FAILURE,
    CHECKSUM_UNICODE_FAILURE
}

enum IMAGE_SEPARATION = 65536;

enum SPLITSYM_REMOVE_PRIVATE    = 1;
enum SPLITSYM_EXTRACT_ALL       = 2;
enum SPLITSYM_SYMBOLPATH_IS_SRC = 4;

enum SYMF_OMAP_GENERATED = 1;
enum SYMF_OMAP_MODIFIED  = 2;

enum SYMOPT_CASE_INSENSITIVE  =  1;
enum SYMOPT_UNDNAME           =  2;
enum SYMOPT_DEFERRED_LOADS    =  4;
enum SYMOPT_NO_CPP            =  8;
//const SYMOPT_LOAD_LINES        = 16;
//const SYMOPT_OMAP_FIND_NEAREST = 32;
public import core.sys.windows.dbghelp_types :
    SYMOPT_DEFERRED_LOAD,
    SYMOPT_FAIL_CRITICAL_ERRORS,
    SYMOPT_LOAD_LINES,
    SYMOPT_DEBUG;

enum UNDNAME_COMPLETE               =     0;
enum UNDNAME_NO_LEADING_UNDERSCORES =     1;
enum UNDNAME_NO_MS_KEYWORDS         =     2;
enum UNDNAME_NO_FUNCTION_RETURNS    =     4;
enum UNDNAME_NO_ALLOCATION_MODEL    =     8;
enum UNDNAME_NO_ALLOCATION_LANGUAGE =    16;
enum UNDNAME_NO_MS_THISTYPE         =    32;
enum UNDNAME_NO_CV_THISTYPE         =    64;
enum UNDNAME_NO_THISTYPE            =    96;
enum UNDNAME_NO_ACCESS_SPECIFIERS   =   128;
enum UNDNAME_NO_THROW_SIGNATURES    =   256;
enum UNDNAME_NO_MEMBER_TYPE         =   512;
enum UNDNAME_NO_RETURN_UDT_MODEL    =  1024;
enum UNDNAME_32_BIT_DECODE          =  2048;
enum UNDNAME_NAME_ONLY              =  4096;
enum UNDNAME_NO_ARGUMENTS           =  8192;
enum UNDNAME_NO_SPECIAL_SYMS        = 16384;

enum IMAGEHLP_STATUS_REASON {
    BindOutOfMemory,
    BindRvaToVaFailed,
    BindNoRoomInImage,
    BindImportModuleFailed,
    BindImportProcedureFailed,
    BindImportModule,
    BindImportProcedure,
    BindForwarder,
    BindForwarderNOT,
    BindImageModified,
    BindExpandFileHeaders,
    BindImageComplete,
    BindMismatchedSymbols,
    BindSymbolsNotUpdated
}

struct LOADED_IMAGE {
    LPSTR                 ModuleName;
    HANDLE                hFile;
    PUCHAR                MappedAddress;
    PIMAGE_NT_HEADERS     FileHeader;
    PIMAGE_SECTION_HEADER LastRvaSection;
    ULONG                 NumberOfSections;
    PIMAGE_SECTION_HEADER Sections;
    ULONG                 Characteristics;
    BOOLEAN               fSystemImage;
    BOOLEAN               fDOSImage;
    LIST_ENTRY            Links;
    ULONG                 SizeOfImage;
}
alias LOADED_IMAGE* PLOADED_IMAGE;

struct IMAGE_DEBUG_INFORMATION {
    LIST_ENTRY                 List;
    DWORD                      Size;
    PVOID                      MappedBase;
    USHORT                     Machine;
    USHORT                     Characteristics;
    DWORD                      CheckSum;
    DWORD                      ImageBase;
    DWORD                      SizeOfImage;
    DWORD                      NumberOfSections;
    PIMAGE_SECTION_HEADER      Sections;
    DWORD                      ExportedNamesSize;
    LPSTR                      ExportedNames;
    DWORD                      NumberOfFunctionTableEntries;
    PIMAGE_FUNCTION_ENTRY      FunctionTableEntries;
    DWORD                      LowestFunctionStartingAddress;
    DWORD                      HighestFunctionEndingAddress;
    DWORD                      NumberOfFpoTableEntries;
    PFPO_DATA                  FpoTableEntries;
    DWORD                      SizeOfCoffSymbols;
    PIMAGE_COFF_SYMBOLS_HEADER CoffSymbols;
    DWORD                      SizeOfCodeViewSymbols;
    PVOID                      CodeViewSymbols;
    LPSTR                      ImageFilePath;
    LPSTR                      ImageFileName;
    LPSTR                      DebugFilePath;
    DWORD                      TimeDateStamp;
    BOOL                       RomImage;
    PIMAGE_DEBUG_DIRECTORY     DebugDirectory;
    DWORD                      NumberOfDebugDirectories;
    DWORD[3]                   Reserved;
}
alias IMAGE_DEBUG_INFORMATION* PIMAGE_DEBUG_INFORMATION;

enum ADDRESS_MODE {
    AddrMode1616,
    AddrMode1632,
    AddrModeReal,
    AddrModeFlat
}

struct ADDRESS {
    DWORD        Offset;
    WORD         Segment;
    ADDRESS_MODE Mode;
}
alias ADDRESS* LPADDRESS;

struct KDHELP {
    DWORD Thread;
    DWORD ThCallbackStack;
    DWORD NextCallback;
    DWORD FramePointer;
    DWORD KiCallUserMode;
    DWORD KeUserCallbackDispatcher;
    DWORD SystemRangeStart;
    DWORD ThCallbackBStore;
    DWORD KiUserExceptionDispatcher;
    DWORD StackBase;
    DWORD StackLimit;
    DWORD[5] Reserved;
}
alias KDHELP* PKDHELP;

struct STACKFRAME {
    ADDRESS  AddrPC;
    ADDRESS  AddrReturn;
    ADDRESS  AddrFrame;
    ADDRESS  AddrStack;
    LPVOID   FuncTableEntry;
    DWORD[4] Params;
    BOOL     Far;
    BOOL     Virtual;
    DWORD[3] Reserved;
    KDHELP   KdHelp;
    ADDRESS  AddrBStore;
}
alias STACKFRAME* LPSTACKFRAME;

/*
struct API_VERSION {
    USHORT MajorVersion;
    USHORT MinorVersion;
    USHORT Revision;
    USHORT Reserved;
}
*/
public import core.sys.windows.dbghelp_types : API_VERSION;
alias API_VERSION* LPAPI_VERSION;

enum SYM_TYPE {
    SymNone,
    SymCoff,
    SymCv,
    SymPdb,
    SymExport,
    SymDeferred,
    SymSym
}

struct IMAGEHLP_SYMBOL {
    DWORD   SizeOfStruct;
    DWORD   Address;
    DWORD   Size;
    DWORD   Flags;
    DWORD   MaxNameLength;
    CHAR[1] Name = 0;
}
alias IMAGEHLP_SYMBOL* PIMAGEHLP_SYMBOL;

struct IMAGEHLP_MODULE {
    DWORD     SizeOfStruct;
    DWORD     BaseOfImage;
    DWORD     ImageSize;
    DWORD     TimeDateStamp;
    DWORD     CheckSum;
    DWORD     NumSyms;
    SYM_TYPE  SymType;
    CHAR[32]  ModuleName = 0;
    CHAR[256] ImageName = 0;
    CHAR[256] LoadedImageName = 0;
}
alias IMAGEHLP_MODULE* PIMAGEHLP_MODULE;

struct IMAGEHLP_LINE {
    DWORD SizeOfStruct;
    DWORD Key;
    DWORD LineNumber;
    PCHAR FileName;
    DWORD Address;
}
alias IMAGEHLP_LINE* PIMAGEHLP_LINE;

struct IMAGEHLP_DEFERRED_SYMBOL_LOAD {
    DWORD          SizeOfStruct;
    DWORD          BaseOfImage;
    DWORD          CheckSum;
    DWORD          TimeDateStamp;
    CHAR[MAX_PATH] FileName = 0;
    BOOLEAN        Reparse;
}
alias IMAGEHLP_DEFERRED_SYMBOL_LOAD* PIMAGEHLP_DEFERRED_SYMBOL_LOAD;

struct IMAGEHLP_DUPLICATE_SYMBOL {
    DWORD            SizeOfStruct;
    DWORD            NumberOfDups;
    PIMAGEHLP_SYMBOL Symbol;
    ULONG            SelectedSymbol;
}
alias IMAGEHLP_DUPLICATE_SYMBOL* PIMAGEHLP_DUPLICATE_SYMBOL;

mixin DECLARE_HANDLE!("DIGEST_HANDLE");

extern (Windows) {
    alias BOOL function(IMAGEHLP_STATUS_REASON, LPSTR, LPSTR, ULONG_PTR, ULONG_PTR)
      PIMAGEHLP_STATUS_ROUTINE;
    alias BOOL function(HANDLE , LPCVOID, LPVOID, DWORD, LPDWORD)
      PREAD_PROCESS_MEMORY_ROUTINE;
    alias LPVOID function(HANDLE, DWORD) PFUNCTION_TABLE_ACCESS_ROUTINE;
    alias DWORD function(HANDLE, DWORD) PGET_MODULE_BASE_ROUTINE;
    alias DWORD function(HANDLE, HANDLE, LPADDRESS)
      PTRANSLATE_ADDRESS_ROUTINE;
    alias BOOL function(LPSTR, ULONG, PVOID) PSYM_ENUMMODULES_CALLBACK;
    alias BOOL function(LPSTR, ULONG, ULONG, PVOID) PSYM_ENUMSYMBOLS_CALLBACK;
    alias BOOL function(LPSTR, ULONG, ULONG, PVOID)
      PENUMLOADED_MODULES_CALLBACK;
    alias BOOL function(HANDLE, ULONG, PVOID, PVOID)
      PSYMBOL_REGISTERED_CALLBACK;
    alias BOOL function(DIGEST_HANDLE refdata, PBYTE pData, DWORD dwLength)
      DIGEST_FUNCTION;

    PIMAGE_NT_HEADERS CheckSumMappedFile(LPVOID, DWORD, LPDWORD, LPDWORD);
    DWORD MapFileAndCheckSumA(LPSTR, LPDWORD, LPDWORD);
    DWORD MapFileAndCheckSumW(PWSTR, LPDWORD, LPDWORD);
    BOOL TouchFileTimes(HANDLE, LPSYSTEMTIME);
    BOOL SplitSymbols(LPSTR, LPSTR, LPSTR, DWORD);
    HANDLE FindDebugInfoFile(LPSTR, LPSTR, LPSTR);
    HANDLE FindExecutableImage(LPSTR, LPSTR, LPSTR);
    BOOL UpdateDebugInfoFile(LPSTR, LPSTR, LPSTR, PIMAGE_NT_HEADERS);
    BOOL UpdateDebugInfoFileEx(LPSTR, LPSTR, LPSTR, PIMAGE_NT_HEADERS, DWORD);
    BOOL BindImage(LPSTR, LPSTR, LPSTR);
    BOOL BindImageEx(DWORD, LPSTR, LPSTR, LPSTR, PIMAGEHLP_STATUS_ROUTINE);
    BOOL ReBaseImage(LPSTR, LPSTR, BOOL, BOOL, BOOL, ULONG, ULONG*, ULONG_PTR*,
      ULONG*, ULONG_PTR*, ULONG);
    PLOADED_IMAGE ImageLoad(LPSTR, LPSTR);
    BOOL ImageUnload(PLOADED_IMAGE);
    PIMAGE_NT_HEADERS ImageNtHeader(PVOID);
    PVOID ImageDirectoryEntryToData(PVOID, BOOLEAN, USHORT, PULONG);
    PIMAGE_SECTION_HEADER ImageRvaToSection(PIMAGE_NT_HEADERS, PVOID, ULONG);
    PVOID ImageRvaToVa(PIMAGE_NT_HEADERS, PVOID, ULONG,
      PIMAGE_SECTION_HEADER*);
    BOOL MapAndLoad(LPSTR, LPSTR, PLOADED_IMAGE, BOOL, BOOL);
    BOOL GetImageConfigInformation(PLOADED_IMAGE,
      PIMAGE_LOAD_CONFIG_DIRECTORY);
    DWORD GetImageUnusedHeaderBytes(PLOADED_IMAGE, LPDWORD);
    BOOL SetImageConfigInformation(PLOADED_IMAGE,
      PIMAGE_LOAD_CONFIG_DIRECTORY);
    BOOL UnMapAndLoad(PLOADED_IMAGE);
    PIMAGE_DEBUG_INFORMATION MapDebugInformation(HANDLE, LPSTR, LPSTR, DWORD);
    BOOL UnmapDebugInformation(PIMAGE_DEBUG_INFORMATION);
    HANDLE FindExecutableImage(LPSTR, LPSTR, LPSTR);
    BOOL SearchTreeForFile(LPSTR, LPSTR, LPSTR);
    BOOL MakeSureDirectoryPathExists(LPCSTR);
    DWORD UnDecorateSymbolName(LPCSTR, LPSTR, DWORD, DWORD);
    BOOL StackWalk(DWORD, HANDLE, HANDLE, LPSTACKFRAME, LPVOID,
      PREAD_PROCESS_MEMORY_ROUTINE, PFUNCTION_TABLE_ACCESS_ROUTINE,
      PGET_MODULE_BASE_ROUTINE, PTRANSLATE_ADDRESS_ROUTINE);
    LPAPI_VERSION ImagehlpApiVersion();
    LPAPI_VERSION ImagehlpApiVersionEx(LPAPI_VERSION);
    DWORD GetTimestampForLoadedLibrary(HMODULE);
    BOOL RemovePrivateCvSymbolic(PCHAR, PCHAR*, ULONG*);
    VOID RemoveRelocations(PCHAR);
    DWORD SymSetOptions(DWORD);
    DWORD SymGetOptions();
    BOOL SymCleanup(HANDLE);
    BOOL SymEnumerateModules(HANDLE, PSYM_ENUMMODULES_CALLBACK, PVOID);
    BOOL SymEnumerateSymbols(HANDLE, DWORD, PSYM_ENUMSYMBOLS_CALLBACK, PVOID);
    BOOL EnumerateLoadedModules(HANDLE, PENUMLOADED_MODULES_CALLBACK, PVOID);
    LPVOID SymFunctionTableAccess(HANDLE, DWORD);
    BOOL SymGetModuleInfo(HANDLE, DWORD, PIMAGEHLP_MODULE);
    DWORD SymGetModuleBase(HANDLE, DWORD);
    BOOL SymGetSymFromAddr(HANDLE, DWORD, PDWORD, PIMAGEHLP_SYMBOL);
    BOOL SymGetSymFromName(HANDLE, LPSTR, PIMAGEHLP_SYMBOL);
    BOOL SymGetSymNext(HANDLE, PIMAGEHLP_SYMBOL);
    BOOL SymGetSymPrev(HANDLE, PIMAGEHLP_SYMBOL);
    BOOL SymGetLineFromAddr(HANDLE, DWORD, PDWORD, PIMAGEHLP_LINE);
    BOOL SymGetLineFromName(HANDLE, LPSTR, LPSTR, DWORD, PLONG,
      PIMAGEHLP_LINE);
    BOOL SymGetLineNext(HANDLE, PIMAGEHLP_LINE);
    BOOL SymGetLinePrev(HANDLE, PIMAGEHLP_LINE);
    BOOL SymMatchFileName(LPSTR, LPSTR, LPSTR*, LPSTR*);
    BOOL SymInitialize(HANDLE, LPSTR, BOOL);
    BOOL SymGetSearchPath(HANDLE, LPSTR, DWORD);
    BOOL SymSetSearchPath(HANDLE, LPSTR);
    BOOL SymLoadModule(HANDLE, HANDLE, PSTR, PSTR, DWORD, DWORD);
    BOOL SymUnloadModule(HANDLE, DWORD);
    BOOL SymUnDName(PIMAGEHLP_SYMBOL, LPSTR, DWORD);
    BOOL SymRegisterCallback(HANDLE, PSYMBOL_REGISTERED_CALLBACK, PVOID);
    BOOL ImageGetDigestStream(HANDLE, DWORD, DIGEST_FUNCTION, DIGEST_HANDLE);
    BOOL ImageAddCertificate(HANDLE, LPWIN_CERTIFICATE, PDWORD);
    BOOL ImageRemoveCertificate(HANDLE, DWORD);
    BOOL ImageEnumerateCertificates(HANDLE, WORD, PDWORD, PDWORD, DWORD);
    BOOL ImageGetCertificateData(HANDLE, DWORD, LPWIN_CERTIFICATE, PDWORD);
    BOOL ImageGetCertificateHeader(HANDLE, DWORD, LPWIN_CERTIFICATE);
    BOOL CopyPdb(CHAR*, CHAR*, BOOL);
    BOOL RemovePrivateCvSymbolicEx(PCHAR, ULONG, PCHAR*, ULONG*);
}

version (Unicode) {
    alias MapFileAndCheckSumW MapFileAndCheckSum;
} else {
    alias MapFileAndCheckSumA MapFileAndCheckSum;
}

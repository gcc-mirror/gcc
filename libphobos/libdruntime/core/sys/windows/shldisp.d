/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_shldisp.d)
 */
module core.sys.windows.shldisp;
version (Windows):

import core.sys.windows.unknwn, core.sys.windows.windef, core.sys.windows.wtypes;

// options for IAutoComplete2
enum DWORD ACO_AUTOSUGGEST = 0x01;

interface IAutoComplete : IUnknown {
    HRESULT Init(HWND, IUnknown, LPCOLESTR, LPCOLESTR);
    HRESULT Enable(BOOL);
}
alias IAutoComplete LPAUTOCOMPLETE;

interface IAutoComplete2 : IAutoComplete {
    HRESULT SetOptions(DWORD);
    HRESULT GetOptions(DWORD*);
}
alias IAutoComplete2 LPAUTOCOMPLETE2;

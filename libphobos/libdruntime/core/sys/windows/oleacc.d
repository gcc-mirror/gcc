/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_oleacc.d)
 */
module core.sys.windows.oleacc;
version (Windows):
@system:

version (ANSI) {} else version = Unicode;
pragma(lib, "oleacc");

import core.sys.windows.basetyps, core.sys.windows.oaidl, core.sys.windows.unknwn, core.sys.windows.wtypes,
  core.sys.windows.windef;

enum {
    DISPID_ACC_PARENT           = -5000,
    DISPID_ACC_CHILDCOUNT       = -5001,
    DISPID_ACC_CHILD            = -5002,
    DISPID_ACC_NAME             = -5003,
    DISPID_ACC_VALUE            = -5004,
    DISPID_ACC_DESCRIPTION      = -5005,
    DISPID_ACC_ROLE             = -5006,
    DISPID_ACC_STATE            = -5007,
    DISPID_ACC_HELP             = -5008,
    DISPID_ACC_HELPTOPIC        = -5009,
    DISPID_ACC_KEYBOARDSHORTCUT = -5010,
    DISPID_ACC_FOCUS            = -5011,
    DISPID_ACC_SELECTION        = -5012,
    DISPID_ACC_DEFAULTACTION    = -5013,
    DISPID_ACC_SELECT           = -5014,
    DISPID_ACC_LOCATION         = -5015,
    DISPID_ACC_NAVIGATE         = -5016,
    DISPID_ACC_HITTEST          = -5017,
    DISPID_ACC_DODEFAULTACTION  = -5018
}

enum {
    NAVDIR_UP = 1,
    NAVDIR_DOWN,
    NAVDIR_LEFT,
    NAVDIR_RIGHT,
    NAVDIR_NEXT,
    NAVDIR_PREVIOUS,
    NAVDIR_FIRSTCHILD,
    NAVDIR_LASTCHILD // = 8
}

enum {
    ROLE_SYSTEM_TITLEBAR = 1,
    ROLE_SYSTEM_MENUBAR,
    ROLE_SYSTEM_SCROLLBAR,
    ROLE_SYSTEM_GRIP,
    ROLE_SYSTEM_SOUND,
    ROLE_SYSTEM_CURSOR,
    ROLE_SYSTEM_CARET,
    ROLE_SYSTEM_ALERT,
    ROLE_SYSTEM_WINDOW,
    ROLE_SYSTEM_CLIENT,
    ROLE_SYSTEM_MENUPOPUP,
    ROLE_SYSTEM_MENUITEM,
    ROLE_SYSTEM_TOOLTIP,
    ROLE_SYSTEM_APPLICATION,
    ROLE_SYSTEM_DOCUMENT,
    ROLE_SYSTEM_PANE,
    ROLE_SYSTEM_CHART,
    ROLE_SYSTEM_DIALOG,
    ROLE_SYSTEM_BORDER,
    ROLE_SYSTEM_GROUPING,
    ROLE_SYSTEM_SEPARATOR,
    ROLE_SYSTEM_TOOLBAR,
    ROLE_SYSTEM_STATUSBAR,
    ROLE_SYSTEM_TABLE,
    ROLE_SYSTEM_COLUMNHEADER,
    ROLE_SYSTEM_ROWHEADER,
    ROLE_SYSTEM_COLUMN,
    ROLE_SYSTEM_ROW,
    ROLE_SYSTEM_CELL,
    ROLE_SYSTEM_LINK,
    ROLE_SYSTEM_HELPBALLOON,
    ROLE_SYSTEM_CHARACTER,
    ROLE_SYSTEM_LIST,
    ROLE_SYSTEM_LISTITEM,
    ROLE_SYSTEM_OUTLINE,
    ROLE_SYSTEM_OUTLINEITEM,
    ROLE_SYSTEM_PAGETAB,
    ROLE_SYSTEM_PROPERTYPAGE,
    ROLE_SYSTEM_INDICATOR,
    ROLE_SYSTEM_GRAPHIC,
    ROLE_SYSTEM_STATICTEXT,
    ROLE_SYSTEM_TEXT,
    ROLE_SYSTEM_PUSHBUTTON,
    ROLE_SYSTEM_CHECKBUTTON,
    ROLE_SYSTEM_RADIOBUTTON,
    ROLE_SYSTEM_COMBOBOX,
    ROLE_SYSTEM_DROPLIST,
    ROLE_SYSTEM_PROGRESSBAR,
    ROLE_SYSTEM_DIAL,
    ROLE_SYSTEM_HOTKEYFIELD,
    ROLE_SYSTEM_SLIDER,
    ROLE_SYSTEM_SPINBUTTON,
    ROLE_SYSTEM_DIAGRAM,
    ROLE_SYSTEM_ANIMATION,
    ROLE_SYSTEM_EQUATION,
    ROLE_SYSTEM_BUTTONDROPDOWN,
    ROLE_SYSTEM_BUTTONMENU,
    ROLE_SYSTEM_BUTTONDROPDOWNGRID,
    ROLE_SYSTEM_WHITESPACE,
    ROLE_SYSTEM_PAGETABLIST,
    ROLE_SYSTEM_CLOCK // = 61
}

enum {
    STATE_SYSTEM_UNAVAILABLE     = 0x00000001,
    STATE_SYSTEM_SELECTED        = 0x00000002,
    STATE_SYSTEM_FOCUSED         = 0x00000004,
    STATE_SYSTEM_PRESSED         = 0x00000008,
    STATE_SYSTEM_CHECKED         = 0x00000010,
    STATE_SYSTEM_MIXED           = 0x00000020,
    STATE_SYSTEM_READONLY        = 0x00000040,
    STATE_SYSTEM_HOTTRACKED      = 0x00000080,
    STATE_SYSTEM_DEFAULT         = 0x00000100,
    STATE_SYSTEM_EXPANDED        = 0x00000200,
    STATE_SYSTEM_COLLAPSED       = 0x00000400,
    STATE_SYSTEM_BUSY            = 0x00000800,
    STATE_SYSTEM_FLOATING        = 0x00001000,
    STATE_SYSTEM_MARQUEED        = 0x00002000,
    STATE_SYSTEM_ANIMATED        = 0x00004000,
    STATE_SYSTEM_INVISIBLE       = 0x00008000,
    STATE_SYSTEM_OFFSCREEN       = 0x00010000,
    STATE_SYSTEM_SIZEABLE        = 0x00020000,
    STATE_SYSTEM_MOVEABLE        = 0x00040000,
    STATE_SYSTEM_SELFVOICING     = 0x00080000,
    STATE_SYSTEM_FOCUSABLE       = 0x00100000,
    STATE_SYSTEM_SELECTABLE      = 0x00200000,
    STATE_SYSTEM_LINKED          = 0x00400000,
    STATE_SYSTEM_TRAVERSED       = 0x00800000,
    STATE_SYSTEM_MULTISELECTABLE = 0x01000000,
    STATE_SYSTEM_EXTSELECTABLE   = 0x02000000,
    STATE_SYSTEM_ALERT_LOW       = 0x04000000,
    STATE_SYSTEM_ALERT_MEDIUM    = 0x08000000,
    STATE_SYSTEM_ALERT_HIGH      = 0x10000000,
    STATE_SYSTEM_VALID           = 0x1fffffff
}

enum SELFLAG
{
    SELFLAG_NONE            = 0,
    SELFLAG_TAKEFOCUS       = 1,
    SELFLAG_TAKESELECTION   = 2,
    SELFLAG_EXTENDSELECTION = 4,
    SELFLAG_ADDSELECTION    = 8,
    SELFLAG_REMOVESELECTION = 16
}

enum SELFLAG_VALID = 0x0000001F;


interface IAccessible : IDispatch {
    HRESULT get_accParent(IDispatch*);
    HRESULT get_accChildCount(int*);
    HRESULT get_accChild(VARIANT, IDispatch*);
    HRESULT get_accName(VARIANT, BSTR*);
    HRESULT get_accValue(VARIANT, BSTR*);
    HRESULT get_accDescription(VARIANT, BSTR*);
    HRESULT get_accRole(VARIANT, VARIANT*);
    HRESULT get_accState(VARIANT, VARIANT*);
    HRESULT get_accHelp(VARIANT, BSTR*);
    HRESULT get_accHelpTopic(BSTR*, VARIANT, int*);
    HRESULT get_accKeyboardShortcut(VARIANT, BSTR*);
    HRESULT get_accFocus(VARIANT*);
    HRESULT get_accSelection(VARIANT*);
    HRESULT get_accDefaultAction(VARIANT, BSTR*);

    HRESULT accSelect(int, VARIANT);
    HRESULT accLocation(int*, int*, int*, int*, VARIANT);
    HRESULT accNavigate(int, VARIANT, VARIANT*);
    HRESULT accHitTest(int, int, VARIANT*);
    HRESULT accDoDefaultAction(VARIANT);

    HRESULT put_accName(VARIANT, BSTR);
    HRESULT put_accValue(VARIANT, BSTR);
}

alias IAccessible LPACCESSIBLE;

extern (Windows) {
    HRESULT AccessibleChildren(IAccessible, LONG, LONG, VARIANT*, LONG*);
    HRESULT AccessibleObjectFromEvent(HWND, DWORD, DWORD, IAccessible, VARIANT*);
    HRESULT AccessibleObjectFromPoint(POINT, IAccessible*, VARIANT*);
    HRESULT AccessibleObjectFromWindow(HWND, DWORD, REFIID, void**);
    HRESULT CreateStdAccessibleObject(HWND, LONG, REFIID, void**);
    HRESULT CreateStdAccessibleProxyA(HWND, LPCSTR, LONG, REFIID, void**);
    HRESULT CreateStdAccessibleProxyW(HWND, LPCWSTR, LONG, REFIID, void**);

    void GetOleaccVersionInfo(DWORD*, DWORD*);
    UINT GetRoleTextA(DWORD, LPSTR, UINT);
    UINT GetRoleTextW(DWORD, LPWSTR, UINT);
    UINT GetStateTextA(DWORD, LPSTR, UINT);
    UINT GetStateTextW(DWORD, LPWSTR, UINT);
    LRESULT LresultFromObject(REFIID, WPARAM, LPUNKNOWN);
    HRESULT ObjectFromLresult(LRESULT, REFIID, WPARAM, void**);
    HRESULT WindowFromAccessibleObject(IAccessible, HWND*);
}

version (Unicode) {
    alias CreateStdAccessibleProxyW CreateStdAccessibleProxy;
    alias GetRoleTextW GetRoleText;
    alias GetStateTextW GetStateText;
} else {
    alias CreateStdAccessibleProxyA CreateStdAccessibleProxy;
    alias GetRoleTextA GetRoleText;
    alias GetStateTextA GetStateText;
}

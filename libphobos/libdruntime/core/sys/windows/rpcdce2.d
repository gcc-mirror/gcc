/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_rpcdce2.d)
 */
module core.sys.windows.rpcdce2;
version (Windows):

version (ANSI) {} else version = Unicode;

import core.sys.windows.rpcdce;
private import core.sys.windows.basetyps;

// FIXME: deal with RPC_UNICODE_SUPPORTED
// FIXME: check types of constants

enum {
    RPC_C_EP_ALL_ELTS,
    RPC_C_EP_MATCH_BY_IF,
    RPC_C_EP_MATCH_BY_OBJ,
    RPC_C_EP_MATCH_BY_BOTH
}

enum {
    RPC_C_VERS_ALL = 1,
    RPC_C_VERS_COMPATIBLE,
    RPC_C_VERS_EXACT,
    RPC_C_VERS_MAJOR_ONLY,
    RPC_C_VERS_UPTO
}

enum size_t DCE_C_ERROR_STRING_LEN = 256;

enum {
    RPC_C_MGMT_INQ_IF_IDS,
    RPC_C_MGMT_INQ_PRINC_NAME,
    RPC_C_MGMT_INQ_STATS,
    RPC_C_MGMT_IS_SERVER_LISTEN,
    RPC_C_MGMT_STOP_SERVER_LISTEN
}

extern (Windows) {
    int UuidCompare(UUID*, UUID*, RPC_STATUS*);
    RPC_STATUS UuidCreateNil(UUID*);
    int UuidEqual(UUID*, UUID*, RPC_STATUS*);
    ushort UuidHash(UUID*, RPC_STATUS*);
    int UuidIsNil(UUID*, RPC_STATUS*);

    RPC_STATUS RpcMgmtEpEltInqBegin(RPC_BINDING_HANDLE, uint, RPC_IF_ID*,
      uint, UUID*, RPC_EP_INQ_HANDLE*);
    RPC_STATUS RpcMgmtEpEltInqDone(RPC_EP_INQ_HANDLE*);
    RPC_STATUS RpcMgmtEpUnregister(RPC_BINDING_HANDLE, RPC_IF_ID*,
      RPC_BINDING_HANDLE, UUID*);
    RPC_STATUS RpcMgmtSetAuthorizationFn(RPC_MGMT_AUTHORIZATION_FN);
}


//#ifdef RPC_UNICODE_SUPPORTED
extern (Windows) {
    RPC_STATUS DceErrorInqTextA(RPC_STATUS, char*);
    RPC_STATUS DceErrorInqTextW(RPC_STATUS, wchar*);
    RPC_STATUS RpcMgmtEpEltInqNextA(RPC_EP_INQ_HANDLE, RPC_IF_ID*,
      RPC_BINDING_HANDLE*, UUID*, char**);
    RPC_STATUS RpcMgmtEpEltInqNextW(RPC_EP_INQ_HANDLE, RPC_IF_ID*,
      RPC_BINDING_HANDLE*, UUID*, wchar**);
}

version (Unicode) {
    alias RpcMgmtEpEltInqNextW RpcMgmtEpEltInqNext;
    alias DceErrorInqTextW DceErrorInqText;
} else {
    alias RpcMgmtEpEltInqNextA RpcMgmtEpEltInqNext;
    alias DceErrorInqTextA DceErrorInqText;
}
/+
#else /* RPC_UNICODE_SUPPORTED */
    RPC_STATUS RPC_ENTRY DceErrorInqText(RPC_STATUS,unsigned char*);
    RPC_STATUS RPC_ENTRY RpcMgmtEpEltInqNext(RPC_EP_INQ_HANDLE,RPC_IF_ID*,RPC_BINDING_HANDLE*,UUID*,unsigned char**);
#endif
+/

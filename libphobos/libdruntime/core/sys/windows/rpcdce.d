/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_rpcdce.d)
 */
module core.sys.windows.rpcdce;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "Rpcrt4");

// TODO: I think MinGW got this wrong. RPC_UNICODE_SUPPORTED should be
// replaced aliases for version (Unicode)

public import core.sys.windows.rpcdcep;
private import core.sys.windows.basetyps, core.sys.windows.w32api, core.sys.windows.windef;

// FIXME: clean up Windows version support

alias UUID uuid_t;
alias UUID_VECTOR uuid_vector_t;
alias void RPC_MGR_EPV;

// for RpcMgmtSetComTimeout()
enum : uint {
    RPC_C_BINDING_MIN_TIMEOUT      = 0,
    RPC_C_BINDING_DEFAULT_TIMEOUT  = 5,
    RPC_C_BINDING_MAX_TIMEOUT      = 9,
    RPC_C_BINDING_INFINITE_TIMEOUT = 10
}

enum RPC_C_CANCEL_INFINITE_TIMEOUT= -1;
enum RPC_C_LISTEN_MAX_CALLS_DEFAULT=1234;
enum RPC_C_PROTSEQ_MAX_REQS_DEFAULT=10;
enum RPC_C_BIND_TO_ALL_NICS=1;
enum RPC_C_USE_INTERNET_PORT=1;
enum RPC_C_USE_INTRANET_PORT=2;

// for RPC_STATS_VECTOR, used by RpcMgmyInqStats
enum : uint {
    RPC_C_STATS_CALLS_IN  = 0,
    RPC_C_STATS_CALLS_OUT,
    RPC_C_STATS_PKTS_IN,
    RPC_C_STATS_PKTS_OUT
}

enum RPC_IF_AUTOLISTEN=0x0001;
enum RPC_IF_OLE=2;
enum RPC_C_MGMT_INQ_IF_IDS=0;
enum RPC_C_MGMT_INQ_PRINC_NAME=1;
enum RPC_C_MGMT_INQ_STATS=2;
enum RPC_C_MGMT_IS_SERVER_LISTEN=3;
enum RPC_C_MGMT_STOP_SERVER_LISTEN=4;

// Inquiry Type for RpcMgmtEpEltInqBegin()
enum : uint {
    RPC_C_EP_ALL_ELTS = 0,
    RPC_C_EP_MATCH_BY_IF,
    RPC_C_EP_MATCH_BY_OBJ,
    RPC_C_EP_MATCH_BY_BOTH
}

// for RpcMgmtEpEltInqNext()
enum : uint {
    RPC_C_VERS_ALL = 1,
    RPC_C_VERS_COMPATIBLE,
    RPC_C_VERS_EXACT,
    RPC_C_VERS_MAJOR_ONLY,
    RPC_C_VERS_UPTO
}

enum DCE_C_ERROR_STRING_LEN=256;
enum RPC_C_PARM_MAX_PACKET_LENGTH=1;
enum RPC_C_PARM_BUFFER_LENGTH=2;
enum RPC_C_AUTHN_LEVEL_DEFAULT=0;
enum RPC_C_AUTHN_LEVEL_NONE=1;
enum RPC_C_AUTHN_LEVEL_CONNECT=2;
enum RPC_C_AUTHN_LEVEL_CALL=3;
enum RPC_C_AUTHN_LEVEL_PKT=4;
enum RPC_C_AUTHN_LEVEL_PKT_INTEGRITY=5;
enum RPC_C_AUTHN_LEVEL_PKT_PRIVACY=6;
enum RPC_C_IMP_LEVEL_ANONYMOUS=1;
enum RPC_C_IMP_LEVEL_IDENTIFY=2;
enum RPC_C_IMP_LEVEL_IMPERSONATE=3;
enum RPC_C_IMP_LEVEL_DELEGATE=4;
enum RPC_C_QOS_IDENTITY_STATIC=0;
enum RPC_C_QOS_IDENTITY_DYNAMIC=1;
enum RPC_C_QOS_CAPABILITIES_DEFAULT=0;
enum RPC_C_QOS_CAPABILITIES_MUTUAL_AUTH=1;

// These enums were buggy in MinGW !
enum RPC_C_PROTECT_LEVEL_DEFAULT = RPC_C_AUTHN_LEVEL_DEFAULT;
enum RPC_C_PROTECT_LEVEL_NONE = RPC_C_AUTHN_LEVEL_NONE;
enum RPC_C_PROTECT_LEVEL_CONNECT = RPC_C_AUTHN_LEVEL_CONNECT;
enum RPC_C_PROTECT_LEVEL_CALL = RPC_C_AUTHN_LEVEL_CALL;
enum RPC_C_PROTECT_LEVEL_PKT = RPC_C_AUTHN_LEVEL_PKT;
enum RPC_C_PROTECT_LEVEL_PKT_INTEGRITY = RPC_C_AUTHN_LEVEL_PKT_INTEGRITY;
enum RPC_C_PROTECT_LEVEL_PKT_PRIVACY = RPC_C_AUTHN_LEVEL_PKT_PRIVACY;

enum RPC_C_AUTHN_NONE=0;
enum RPC_C_AUTHN_DCE_PRIVATE=1;
enum RPC_C_AUTHN_DCE_PUBLIC=2;
enum RPC_C_AUTHN_DEC_PUBLIC=4;
enum RPC_C_AUTHN_WINNT=10;
enum RPC_C_AUTHN_DEFAULT=0xFFFFFFFF;
//const RPC_C_SECURITY_QOS_VERSION=L; // FIXME(MinGW): This is nonsense!
enum SEC_WINNT_AUTH_IDENTITY_ANSI=0x1;
enum SEC_WINNT_AUTH_IDENTITY_UNICODE=0x2;
enum RPC_C_AUTHZ_NONE=0;
enum RPC_C_AUTHZ_NAME=1;
enum RPC_C_AUTHZ_DCE=2;
enum RPC_C_AUTHZ_DEFAULT=0xFFFFFFFF;

alias I_RPC_HANDLE RPC_BINDING_HANDLE;
alias RPC_BINDING_HANDLE handle_t;

struct RPC_BINDING_VECTOR {
    uint Count;
    RPC_BINDING_HANDLE[1] BindingH;
}

alias RPC_BINDING_HANDLE rpc_binding_handle_t;
alias RPC_BINDING_VECTOR rpc_binding_vector_t;


struct UUID_VECTOR {
    uint Count;
    UUID*[1] Uuid;
}

alias void* RPC_IF_HANDLE;

struct RPC_IF_ID {
    UUID Uuid;
    ushort VersMajor;
    ushort VersMinor;
}

struct RPC_POLICY {
    uint Length;
    uint EndpointFlags;
    uint NICFlags;
}
alias RPC_POLICY* PRPC_POLICY;

extern (Windows) {
    alias void function(UUID*, UUID*, RPC_STATUS*) RPC_OBJECT_INQ_FN;
    alias RPC_STATUS function(RPC_IF_HANDLE, void*) RPC_IF_CALLBACK_FN;
}

struct RPC_STATS_VECTOR {
    uint    Count;
    uint[1] Stats;
}

struct RPC_IF_ID_VECTOR {
    uint          Count;
    RPC_IF_ID*[1] IfId;
}
mixin DECLARE_HANDLE!("RPC_AUTH_IDENTITY_HANDLE");
mixin DECLARE_HANDLE!("RPC_AUTHZ_HANDLE");

struct RPC_SECURITY_QOS {
    uint Version;
    uint Capabilities;
    uint IdentityTracking;
    uint ImpersonationType;
}
alias RPC_SECURITY_QOS* PRPC_SECURITY_QOS;

struct SEC_WINNT_AUTH_IDENTITY_W {
    ushort* User;
    uint UserLength;
    ushort* Domain;
    uint DomainLength;
    ushort* Password;
    uint PasswordLength;
    uint Flags;
}
alias SEC_WINNT_AUTH_IDENTITY_W* PSEC_WINNT_AUTH_IDENTITY_W;

struct SEC_WINNT_AUTH_IDENTITY_A {
    ubyte* User;
    uint UserLength;
    ubyte* Domain;
    uint DomainLength;
    ubyte* Password;
    uint PasswordLength;
    uint Flags;
}
alias SEC_WINNT_AUTH_IDENTITY_A* PSEC_WINNT_AUTH_IDENTITY_A;

struct RPC_CLIENT_INFORMATION1 {
    ubyte* UserName;
    ubyte* ComputerName;
    ushort Privilege;
    uint AuthFlags;
}
alias RPC_CLIENT_INFORMATION1* PRPC_CLIENT_INFORMATION1;
alias I_RPC_HANDLE* RPC_EP_INQ_HANDLE;
extern (Windows) {
    alias int function(RPC_BINDING_HANDLE, uint, RPC_STATUS*) RPC_MGMT_AUTHORIZATION_FN;
}

struct RPC_PROTSEQ_VECTORA {
    uint Count;
    ubyte*[1] Protseq;
}

struct RPC_PROTSEQ_VECTORW {
    uint Count;
    ushort*[1] Protseq;
}

extern (Windows) {
    RPC_STATUS RpcBindingFromStringBindingA(char*, RPC_BINDING_HANDLE*);
    RPC_STATUS RpcBindingFromStringBindingW(wchar*, RPC_BINDING_HANDLE*);
    RPC_STATUS RpcBindingToStringBindingA(RPC_BINDING_HANDLE, char**);
    RPC_STATUS RpcBindingToStringBindingW(RPC_BINDING_HANDLE, wchar**);
    RPC_STATUS RpcStringBindingComposeA(char*, char*, char*, char*, char*, char**);
    RPC_STATUS RpcStringBindingComposeW(wchar*, wchar*, wchar*, wchar*, wchar*, wchar**);
    RPC_STATUS RpcStringBindingParseA(char*, char**, char**, char**, char**, char**);
    RPC_STATUS RpcStringBindingParseW(wchar*, wchar**, wchar**, wchar**, wchar**, wchar**);
    RPC_STATUS RpcStringFreeA(char**);
    RPC_STATUS RpcStringFreeW(wchar**);
    RPC_STATUS RpcNetworkIsProtseqValidA(char*);
    RPC_STATUS RpcNetworkIsProtseqValidW(wchar*);
    RPC_STATUS RpcNetworkInqProtseqsA(RPC_PROTSEQ_VECTORA**);
    RPC_STATUS RpcNetworkInqProtseqsW(RPC_PROTSEQ_VECTORW**);
    RPC_STATUS RpcProtseqVectorFreeA(RPC_PROTSEQ_VECTORA**);
    RPC_STATUS RpcProtseqVectorFreeW(RPC_PROTSEQ_VECTORW**);
    RPC_STATUS RpcServerUseProtseqA(char*, uint, void*);
    RPC_STATUS RpcServerUseProtseqW(wchar*, uint, void*);
    RPC_STATUS RpcServerUseProtseqExA(char*, uint MaxCalls, void*, PRPC_POLICY);
    RPC_STATUS RpcServerUseProtseqExW(wchar*, uint, void*, PRPC_POLICY);
    RPC_STATUS RpcServerUseProtseqEpA(char*, uint, char*, void*);
    RPC_STATUS RpcServerUseProtseqEpExA(char*, uint, char*, void*, PRPC_POLICY);
    RPC_STATUS RpcServerUseProtseqEpW(wchar*, uint, wchar*, void*);
    RPC_STATUS RpcServerUseProtseqEpExW(wchar*, uint, wchar*, void*, PRPC_POLICY);
    RPC_STATUS RpcServerUseProtseqIfA(char*, uint, RPC_IF_HANDLE, void*);
    RPC_STATUS RpcServerUseProtseqIfExA(char*, uint, RPC_IF_HANDLE, void*, PRPC_POLICY);
    RPC_STATUS RpcServerUseProtseqIfW(wchar*, uint, RPC_IF_HANDLE, void*);
    RPC_STATUS RpcServerUseProtseqIfExW(wchar*, uint, RPC_IF_HANDLE, void*, PRPC_POLICY);
    RPC_STATUS RpcMgmtInqServerPrincNameA(RPC_BINDING_HANDLE, uint, char**);
    RPC_STATUS RpcMgmtInqServerPrincNameW(RPC_BINDING_HANDLE, uint, wchar**);
    RPC_STATUS RpcServerInqDefaultPrincNameA(uint, char**);
    RPC_STATUS RpcServerInqDefaultPrincNameW(uint, wchar**);
    RPC_STATUS RpcNsBindingInqEntryNameA(RPC_BINDING_HANDLE, uint, char**);
    RPC_STATUS RpcNsBindingInqEntryNameW(RPC_BINDING_HANDLE, uint, wchar**);
    RPC_STATUS RpcBindingInqAuthClientA(RPC_BINDING_HANDLE, RPC_AUTHZ_HANDLE*, char**, uint*, uint*, uint*);
    RPC_STATUS RpcBindingInqAuthClientW(RPC_BINDING_HANDLE, RPC_AUTHZ_HANDLE*, wchar**, uint*, uint*, uint*);
    RPC_STATUS RpcBindingInqAuthInfoA(RPC_BINDING_HANDLE, char**, uint*, uint*, RPC_AUTH_IDENTITY_HANDLE*, uint*);
    RPC_STATUS RpcBindingInqAuthInfoW(RPC_BINDING_HANDLE, wchar**, uint*, uint*, RPC_AUTH_IDENTITY_HANDLE*, uint*);
    RPC_STATUS RpcBindingSetAuthInfoA(RPC_BINDING_HANDLE, char*, uint, uint, RPC_AUTH_IDENTITY_HANDLE, uint);
    RPC_STATUS RpcBindingSetAuthInfoExA(RPC_BINDING_HANDLE, char*, uint, uint, RPC_AUTH_IDENTITY_HANDLE, uint, RPC_SECURITY_QOS*);
    RPC_STATUS RpcBindingSetAuthInfoW(RPC_BINDING_HANDLE, wchar*, uint, uint, RPC_AUTH_IDENTITY_HANDLE, uint);
    RPC_STATUS RpcBindingSetAuthInfoExW(RPC_BINDING_HANDLE, wchar*, uint, uint, RPC_AUTH_IDENTITY_HANDLE, uint, RPC_SECURITY_QOS*);
    RPC_STATUS RpcBindingInqAuthInfoExA(RPC_BINDING_HANDLE, char**, uint*, uint*, RPC_AUTH_IDENTITY_HANDLE*, uint*, uint, RPC_SECURITY_QOS*);
    RPC_STATUS RpcBindingInqAuthInfoExW(RPC_BINDING_HANDLE, wchar**, uint*, uint*, RPC_AUTH_IDENTITY_HANDLE*, uint*, uint, RPC_SECURITY_QOS*);
    alias void function(void*, wchar*, uint, void**, RPC_STATUS*) RPC_AUTH_KEY_RETRIEVAL_FN;
    RPC_STATUS RpcServerRegisterAuthInfoA(char*, uint, RPC_AUTH_KEY_RETRIEVAL_FN, void*);
    RPC_STATUS RpcServerRegisterAuthInfoW(wchar*, uint, RPC_AUTH_KEY_RETRIEVAL_FN, void*);
    RPC_STATUS UuidToStringA(UUID*, char**);
    RPC_STATUS UuidFromStringA(char*, UUID*);
    RPC_STATUS UuidToStringW(UUID*, wchar**);
    RPC_STATUS UuidFromStringW(wchar*, UUID*);
    RPC_STATUS RpcEpRegisterNoReplaceA(RPC_IF_HANDLE, RPC_BINDING_VECTOR*, UUID_VECTOR*, char*);
    RPC_STATUS RpcEpRegisterNoReplaceW(RPC_IF_HANDLE, RPC_BINDING_VECTOR*, UUID_VECTOR*, wchar*);
    RPC_STATUS RpcEpRegisterA(RPC_IF_HANDLE, RPC_BINDING_VECTOR*, UUID_VECTOR*, char*);
    RPC_STATUS RpcEpRegisterW(RPC_IF_HANDLE, RPC_BINDING_VECTOR*, UUID_VECTOR*, wchar*);
    RPC_STATUS DceErrorInqTextA(RPC_STATUS, char*);
    RPC_STATUS DceErrorInqTextW(RPC_STATUS, wchar*);
    RPC_STATUS RpcMgmtEpEltInqNextA(RPC_EP_INQ_HANDLE, RPC_IF_ID*, RPC_BINDING_HANDLE*, UUID*, char**);
    RPC_STATUS RpcMgmtEpEltInqNextW(RPC_EP_INQ_HANDLE, RPC_IF_ID*, RPC_BINDING_HANDLE*, UUID*, wchar**);

    // MinGW erroneously had these in rpc.h
    RPC_STATUS RpcImpersonateClient(RPC_BINDING_HANDLE);
    RPC_STATUS RpcRevertToSelf();
}

version (Unicode) {
    alias RPC_PROTSEQ_VECTORW RPC_PROTSEQ_VECTOR;
    alias SEC_WINNT_AUTH_IDENTITY_W SEC_WINNT_AUTH_IDENTITY;
    alias PSEC_WINNT_AUTH_IDENTITY_W PSEC_WINNT_AUTH_IDENTITY;
    alias RpcMgmtEpEltInqNextW RpcMgmtEpEltInqNext;
    alias RpcBindingFromStringBindingW RpcBindingFromStringBinding;
    alias RpcBindingToStringBindingW RpcBindingToStringBinding;
    alias RpcStringBindingComposeW RpcStringBindingCompose;
    alias RpcStringBindingParseW RpcStringBindingParse;
    alias RpcStringFreeW RpcStringFree;
    alias RpcNetworkIsProtseqValidW RpcNetworkIsProtseqValid;
    alias RpcNetworkInqProtseqsW RpcNetworkInqProtseqs;
    alias RpcProtseqVectorFreeW RpcProtseqVectorFree;
    alias RpcServerUseProtseqW RpcServerUseProtseq;
    alias RpcServerUseProtseqExW RpcServerUseProtseqEx;
    alias RpcServerUseProtseqEpW RpcServerUseProtseqEp;
    alias RpcServerUseProtseqEpExW RpcServerUseProtseqEpEx;
    alias RpcServerUseProtseqIfW RpcServerUseProtseqIf;
    alias RpcServerUseProtseqIfExW RpcServerUseProtseqIfEx;
    alias RpcMgmtInqServerPrincNameW RpcMgmtInqServerPrincName;
    alias RpcServerInqDefaultPrincNameW RpcServerInqDefaultPrincName;
    alias RpcNsBindingInqEntryNameW RpcNsBindingInqEntryName;
    alias RpcBindingInqAuthClientW RpcBindingInqAuthClient;
    alias RpcBindingInqAuthInfoW RpcBindingInqAuthInfo;
    alias RpcBindingSetAuthInfoW RpcBindingSetAuthInfo;
    alias RpcServerRegisterAuthInfoW RpcServerRegisterAuthInfo;
    alias RpcBindingInqAuthInfoExW RpcBindingInqAuthInfoEx;
    alias RpcBindingSetAuthInfoExW RpcBindingSetAuthInfoEx;
    alias UuidFromStringW UuidFromString;
    alias UuidToStringW UuidToString;
    alias RpcEpRegisterNoReplaceW RpcEpRegisterNoReplace;
    alias RpcEpRegisterW RpcEpRegister;
    alias DceErrorInqTextW DceErrorInqText;
} else { // Ansi
    alias RPC_PROTSEQ_VECTORA RPC_PROTSEQ_VECTOR;
    alias SEC_WINNT_AUTH_IDENTITY_A SEC_WINNT_AUTH_IDENTITY;
    alias PSEC_WINNT_AUTH_IDENTITY_A PSEC_WINNT_AUTH_IDENTITY;
    alias RpcMgmtEpEltInqNextA RpcMgmtEpEltInqNext;
    alias RpcBindingFromStringBindingA RpcBindingFromStringBinding;
    alias RpcBindingToStringBindingA RpcBindingToStringBinding;
    alias RpcStringBindingComposeA RpcStringBindingCompose;
    alias RpcStringBindingParseA RpcStringBindingParse;
    alias RpcStringFreeA RpcStringFree;
    alias RpcNetworkIsProtseqValidA RpcNetworkIsProtseqValid;
    alias RpcNetworkInqProtseqsA RpcNetworkInqProtseqs;
    alias RpcProtseqVectorFreeA RpcProtseqVectorFree;
    alias RpcServerUseProtseqA RpcServerUseProtseq;
    alias RpcServerUseProtseqExA RpcServerUseProtseqEx;
    alias RpcServerUseProtseqEpA RpcServerUseProtseqEp;
    alias RpcServerUseProtseqEpExA RpcServerUseProtseqEpEx;
    alias RpcServerUseProtseqIfA RpcServerUseProtseqIf;
    alias RpcServerUseProtseqIfExA RpcServerUseProtseqIfEx;
    alias RpcMgmtInqServerPrincNameA RpcMgmtInqServerPrincName;
    alias RpcServerInqDefaultPrincNameA RpcServerInqDefaultPrincName;
    alias RpcNsBindingInqEntryNameA RpcNsBindingInqEntryName;
    alias RpcBindingInqAuthClientA RpcBindingInqAuthClient;
    alias RpcBindingInqAuthInfoA RpcBindingInqAuthInfo;
    alias RpcBindingSetAuthInfoA RpcBindingSetAuthInfo;
    alias RpcServerRegisterAuthInfoA RpcServerRegisterAuthInfo;
    alias RpcBindingInqAuthInfoExA RpcBindingInqAuthInfoEx;
    alias RpcBindingSetAuthInfoExA RpcBindingSetAuthInfoEx;
    alias UuidFromStringA UuidFromString;
    alias UuidToStringA UuidToString;
    alias RpcEpRegisterNoReplaceA RpcEpRegisterNoReplace;
    alias RpcEpRegisterA RpcEpRegister;
    alias DceErrorInqTextA DceErrorInqText;
} //#endif // UNICODE

extern (Windows) {
    RPC_STATUS RpcBindingCopy(RPC_BINDING_HANDLE, RPC_BINDING_HANDLE*);
    RPC_STATUS RpcBindingFree(RPC_BINDING_HANDLE*);
    RPC_STATUS RpcBindingInqObject(RPC_BINDING_HANDLE, UUID*);
    RPC_STATUS RpcBindingReset(RPC_BINDING_HANDLE);
    RPC_STATUS RpcBindingSetObject(RPC_BINDING_HANDLE, UUID*);
    RPC_STATUS RpcMgmtInqDefaultProtectLevel(uint, uint*);
    RPC_STATUS RpcBindingVectorFree(RPC_BINDING_VECTOR**);
    RPC_STATUS RpcIfInqId(RPC_IF_HANDLE, RPC_IF_ID*);
    RPC_STATUS RpcMgmtInqComTimeout(RPC_BINDING_HANDLE, uint*);
    RPC_STATUS RpcMgmtSetComTimeout(RPC_BINDING_HANDLE, uint);
    RPC_STATUS RpcMgmtSetCancelTimeout(int Timeout);
    RPC_STATUS RpcObjectInqType(UUID*, UUID*);
    RPC_STATUS RpcObjectSetInqFn(RPC_OBJECT_INQ_FN*);
    RPC_STATUS RpcObjectSetType(UUID*, UUID*);
    RPC_STATUS RpcProtseqVectorFree(RPC_PROTSEQ_VECTOR**);
    RPC_STATUS RpcServerInqIf(RPC_IF_HANDLE, UUID*, RPC_MGR_EPV**);
    RPC_STATUS RpcServerListen(uint, uint, uint);
    RPC_STATUS RpcServerRegisterIf(RPC_IF_HANDLE, UUID*, RPC_MGR_EPV*);
    RPC_STATUS RpcServerRegisterIfEx(RPC_IF_HANDLE, UUID*, RPC_MGR_EPV*, uint, uint, RPC_IF_CALLBACK_FN*);
    RPC_STATUS RpcServerRegisterIf2(RPC_IF_HANDLE, UUID*, RPC_MGR_EPV*, uint, uint, uint, RPC_IF_CALLBACK_FN*);
    RPC_STATUS RpcServerUnregisterIf(RPC_IF_HANDLE, UUID*, uint);
    RPC_STATUS RpcServerUseAllProtseqs(uint, void*);
    RPC_STATUS RpcServerUseAllProtseqsEx(uint, void*, PRPC_POLICY);
    RPC_STATUS RpcServerUseAllProtseqsIf(uint, RPC_IF_HANDLE, void*);
    RPC_STATUS RpcServerUseAllProtseqsIfEx(uint, RPC_IF_HANDLE, void*, PRPC_POLICY);
    RPC_STATUS RpcMgmtStatsVectorFree(RPC_STATS_VECTOR**);
    RPC_STATUS RpcMgmtInqStats(RPC_BINDING_HANDLE, RPC_STATS_VECTOR**);
    RPC_STATUS RpcMgmtIsServerListening(RPC_BINDING_HANDLE);
    RPC_STATUS RpcMgmtStopServerListening(RPC_BINDING_HANDLE);
    RPC_STATUS RpcMgmtWaitServerListen();
    RPC_STATUS RpcMgmtSetServerStackSize(uint);
    void RpcSsDontSerializeContext();
    RPC_STATUS RpcMgmtEnableIdleCleanup();
    RPC_STATUS RpcMgmtInqIfIds(RPC_BINDING_HANDLE, RPC_IF_ID_VECTOR**);
    RPC_STATUS RpcIfIdVectorFree(RPC_IF_ID_VECTOR**);
    RPC_STATUS RpcEpResolveBinding(RPC_BINDING_HANDLE, RPC_IF_HANDLE);
    RPC_STATUS RpcBindingServerFromClient(RPC_BINDING_HANDLE, RPC_BINDING_HANDLE*);

    // never returns
    void RpcRaiseException(RPC_STATUS);
    RPC_STATUS RpcTestCancel();
    RPC_STATUS RpcCancelThread(void*);
    RPC_STATUS UuidCreate(UUID*);
    int UuidCompare(UUID*, UUID*, RPC_STATUS*);
    RPC_STATUS UuidCreateNil(UUID*);
    int UuidEqual(UUID*, UUID*, RPC_STATUS*);
    ushort UuidHash(UUID*, RPC_STATUS*);
    int UuidIsNil(UUID*, RPC_STATUS*);
    RPC_STATUS RpcEpUnregister(RPC_IF_HANDLE, RPC_BINDING_VECTOR*, UUID_VECTOR*);
    RPC_STATUS RpcMgmtEpEltInqBegin(RPC_BINDING_HANDLE, uint, RPC_IF_ID*, uint, UUID*, RPC_EP_INQ_HANDLE*);
    RPC_STATUS RpcMgmtEpEltInqDone(RPC_EP_INQ_HANDLE*);
    RPC_STATUS RpcMgmtEpUnregister(RPC_BINDING_HANDLE, RPC_IF_ID*, RPC_BINDING_HANDLE, UUID*);
    RPC_STATUS RpcMgmtSetAuthorizationFn(RPC_MGMT_AUTHORIZATION_FN);
    RPC_STATUS RpcMgmtInqParameter(uint, uint*);
    RPC_STATUS RpcMgmtSetParameter(uint, uint);
    RPC_STATUS RpcMgmtBindingInqParameter(RPC_BINDING_HANDLE, uint, uint*);
    RPC_STATUS RpcMgmtBindingSetParameter(RPC_BINDING_HANDLE, uint, uint);

//static if (_WIN32_WINNT >= 0x500) {
    RPC_STATUS UuidCreateSequential(UUID*);
//}
}

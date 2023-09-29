/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_rpcdcep.d)
 */
module core.sys.windows.rpcdcep;
version (Windows):

version (ANSI) {} else version = Unicode;

import core.sys.windows.basetyps;
import core.sys.windows.w32api;
import core.sys.windows.windef;

mixin DECLARE_HANDLE!("I_RPC_HANDLE");
alias long RPC_STATUS;

enum RPC_NCA_FLAGS_DEFAULT=0;
enum RPC_NCA_FLAGS_IDEMPOTENT=1;
enum RPC_NCA_FLAGS_BROADCAST=2;
enum RPC_NCA_FLAGS_MAYBE=4;
enum RPCFLG_ASYNCHRONOUS=0x40000000;
enum RPCFLG_INPUT_SYNCHRONOUS=0x20000000;
enum RPC_FLAGS_VALID_BIT=0x8000;

enum TRANSPORT_TYPE_CN=1;
enum TRANSPORT_TYPE_DG=2;
enum TRANSPORT_TYPE_LPC=4;
enum TRANSPORT_TYPE_WMSG=8;

struct RPC_VERSION {
    ushort MajorVersion;
    ushort MinorVersion;
}
struct RPC_SYNTAX_IDENTIFIER {
    GUID        SyntaxGUID;
    RPC_VERSION SyntaxVersion;
}
alias RPC_SYNTAX_IDENTIFIER* PRPC_SYNTAX_IDENTIFIER;

struct RPC_MESSAGE {
    HANDLE Handle;
    uint  DataRepresentation;
    void* Buffer;
    uint  BufferLength;
    uint  ProcNum;
    PRPC_SYNTAX_IDENTIFIER TransferSyntax;
    void* RpcInterfaceInformation;
    void* ReservedForRuntime;
    void* ManagerEpv;
    void* ImportContext;
    uint  RpcFlags;
}
alias RPC_MESSAGE* PRPC_MESSAGE;

extern (Windows) {
alias void function (PRPC_MESSAGE Message) RPC_DISPATCH_FUNCTION;
}

struct RPC_DISPATCH_TABLE {
    uint DispatchTableCount;
    RPC_DISPATCH_FUNCTION* DispatchTable;
    LONG_PTR Reserved;
}
alias RPC_DISPATCH_TABLE* PRPC_DISPATCH_TABLE;

struct RPC_PROTSEQ_ENDPOINT {
    ubyte* RpcProtocolSequence;
    ubyte* Endpoint;
}
alias RPC_PROTSEQ_ENDPOINT* PRPC_PROTSEQ_ENDPOINT;

struct RPC_SERVER_INTERFACE {
    uint                  Length;
    RPC_SYNTAX_IDENTIFIER InterfaceId;
    RPC_SYNTAX_IDENTIFIER TransferSyntax;
    PRPC_DISPATCH_TABLE   DispatchTable;
    uint                  RpcProtseqEndpointCount;
    PRPC_PROTSEQ_ENDPOINT RpcProtseqEndpoint;
    void*                 DefaultManagerEpv;
const(void)*          InterpreterInfo;
}
alias RPC_SERVER_INTERFACE* PRPC_SERVER_INTERFACE;

struct RPC_CLIENT_INTERFACE {
    uint                  Length;
    RPC_SYNTAX_IDENTIFIER InterfaceId;
    RPC_SYNTAX_IDENTIFIER TransferSyntax;
    PRPC_DISPATCH_TABLE   DispatchTable;
    uint                  RpcProtseqEndpointCount;
    PRPC_PROTSEQ_ENDPOINT RpcProtseqEndpoint;
    ULONG_PTR             Reserved;
const(void)*          InterpreterInfo;
}
alias RPC_CLIENT_INTERFACE* PRPC_CLIENT_INTERFACE;

alias void* I_RPC_MUTEX;

struct RPC_TRANSFER_SYNTAX {
    GUID   Uuid;
    ushort VersMajor;
    ushort VersMinor;
}
alias RPC_STATUS function(void*, void*, void*) RPC_BLOCKING_FN;

extern (Windows) {
    alias void function(void*) PRPC_RUNDOWN;

    int    I_RpcGetBuffer(RPC_MESSAGE*);
    int    I_RpcSendReceive(RPC_MESSAGE*);
    int    I_RpcSend(RPC_MESSAGE*);
    int    I_RpcFreeBuffer(RPC_MESSAGE*);
    void   I_RpcRequestMutex(I_RPC_MUTEX*);
    void   I_RpcClearMutex(I_RPC_MUTEX);
    void   I_RpcDeleteMutex(I_RPC_MUTEX);
    void*  I_RpcAllocate(uint);
    void   I_RpcFree(void*);
    void   I_RpcPauseExecution(uint);
    int    I_RpcMonitorAssociation(HANDLE, PRPC_RUNDOWN, void*);
    int    I_RpcStopMonitorAssociation(HANDLE);
    HANDLE I_RpcGetCurrentCallHandle();
    int    I_RpcGetAssociationContext(void**);
    int    I_RpcSetAssociationContext(void*);
    int    I_RpcNsBindingSetEntryName(HANDLE, uint, wchar*);
    int    I_RpcBindingInqDynamicEndpoint(HANDLE, wchar**);
    int    I_RpcBindingInqTransportType(HANDLE, uint*);
    int    I_RpcIfInqTransferSyntaxes(HANDLE, RPC_TRANSFER_SYNTAX*, uint,
             uint*);
    int    I_UuidCreate(GUID*);
    int    I_RpcBindingCopy(HANDLE, HANDLE*);
    int    I_RpcBindingIsClientLocal(HANDLE, uint*);
    void   I_RpcSsDontSerializeContext();
    int    I_RpcServerRegisterForwardFunction(int function (GUID*,
             RPC_VERSION*, GUID*, ubyte*, void**));
    int    I_RpcConnectionInqSockBuffSize(uint*, uint*);
    int    I_RpcConnectionSetSockBuffSize(uint, uint);
    int    I_RpcBindingSetAsync(HANDLE, RPC_BLOCKING_FN);
    int    I_RpcAsyncSendReceive(RPC_MESSAGE*, void*);
    int    I_RpcGetThreadWindowHandle(void**);
    int    I_RpcServerThreadPauseListening();
    int    I_RpcServerThreadContinueListening();
    int    I_RpcServerUnregisterEndpointA(ubyte*, ubyte*);
    int    I_RpcServerUnregisterEndpointW(ushort*, ushort*);
}

version (Unicode) {
    alias I_RpcServerUnregisterEndpointW I_RpcServerUnregisterEndpoint;
} else {
    alias I_RpcServerUnregisterEndpointA I_RpcServerUnregisterEndpoint;
}

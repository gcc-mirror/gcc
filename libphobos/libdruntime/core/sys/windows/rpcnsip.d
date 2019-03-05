/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_rpcnsip.d)
 */
module core.sys.windows.rpcnsip;
version (Windows):

private import core.sys.windows.rpcdce, core.sys.windows.rpcdcep, core.sys.windows.rpcnsi;

struct RPC_IMPORT_CONTEXT_P {
    RPC_NS_HANDLE       LookupContext;
    RPC_BINDING_HANDLE  ProposedHandle;
    RPC_BINDING_VECTOR* Bindings;
}
alias RPC_IMPORT_CONTEXT_P* PRPC_IMPORT_CONTEXT_P;

extern(Windows) {
    RPC_STATUS I_RpcNsGetBuffer(PRPC_MESSAGE);
    RPC_STATUS I_RpcNsSendReceive(PRPC_MESSAGE, RPC_BINDING_HANDLE*);
    void I_RpcNsRaiseException(PRPC_MESSAGE, RPC_STATUS);
    RPC_STATUS I_RpcReBindBuffer(PRPC_MESSAGE);
    RPC_STATUS I_NsServerBindSearch();
    RPC_STATUS I_NsClientBindSearch();
    void I_NsClientBindDone();
}

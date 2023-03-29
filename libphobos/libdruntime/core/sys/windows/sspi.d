/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Ellery Newcomer
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_sspi.d)
 */
module core.sys.windows.sspi;
version (Windows):

version (ANSI) {} else version = Unicode;

import core.sys.windows.windef;
import core.sys.windows.ntdef;
import core.sys.windows.w32api;
import core.sys.windows.security;
import core.sys.windows.ntsecapi;
import core.sys.windows.subauth;

enum :ULONG{
    SECPKG_CRED_INBOUND = 1,
    SECPKG_CRED_OUTBOUND = 2,
    SECPKG_CRED_BOTH = (SECPKG_CRED_OUTBOUND|SECPKG_CRED_INBOUND),
    SECPKG_CRED_ATTR_NAMES = 1,
}

enum :ULONG{
    SECPKG_FLAG_INTEGRITY = 1,
    SECPKG_FLAG_PRIVACY = 2,
    SECPKG_FLAG_TOKEN_ONLY = 4,
    SECPKG_FLAG_DATAGRAM = 8,
    SECPKG_FLAG_CONNECTION = 16,
    SECPKG_FLAG_MULTI_REQUIRED = 32,
    SECPKG_FLAG_CLIENT_ONLY = 64,
    SECPKG_FLAG_EXTENDED_ERROR = 128,
    SECPKG_FLAG_IMPERSONATION = 256,
    SECPKG_FLAG_ACCEPT_WIN32_NAME = 512,
    SECPKG_FLAG_STREAM = 1024,
}

enum :ULONG{
    SECPKG_ATTR_AUTHORITY = 6,
    SECPKG_ATTR_CONNECTION_INFO = 90,
    SECPKG_ATTR_ISSUER_LIST = 80,
    SECPKG_ATTR_ISSUER_LIST_EX = 89,
    SECPKG_ATTR_KEY_INFO = 5,
    SECPKG_ATTR_LIFESPAN = 2,
    SECPKG_ATTR_LOCAL_CERT_CONTEXT = 84,
    SECPKG_ATTR_LOCAL_CRED = 82,
    SECPKG_ATTR_NAMES = 1,
    SECPKG_ATTR_PROTO_INFO = 7,
    SECPKG_ATTR_REMOTE_CERT_CONTEXT = 83,
    SECPKG_ATTR_REMOTE_CRED = 81,
    SECPKG_ATTR_SIZES = 0,
    SECPKG_ATTR_STREAM_SIZES = 4,
}

enum :ULONG{
    SECBUFFER_EMPTY = 0,
    SECBUFFER_DATA = 1,
    SECBUFFER_TOKEN = 2,
    SECBUFFER_PKG_PARAMS = 3,
    SECBUFFER_MISSING = 4,
    SECBUFFER_EXTRA = 5,
    SECBUFFER_STREAM_TRAILER = 6,
    SECBUFFER_STREAM_HEADER = 7,
    SECBUFFER_PADDING = 9,
    SECBUFFER_STREAM = 10,
    SECBUFFER_READONLY = 0x80000000,
    SECBUFFER_ATTRMASK = 0xf0000000,
}

enum UNISP_NAME_A = "Microsoft Unified Security Protocol Provider";
enum UNISP_NAME_W = "Microsoft Unified Security Protocol Provider"w;
enum SECBUFFER_VERSION = 0;

alias UNICODE_STRING SECURITY_STRING;
alias UNICODE_STRING* PSECURITY_STRING;

extern(Windows):

struct SecHandle {
    ULONG_PTR dwLower;
    ULONG_PTR dwUpper;
}
alias SecHandle* PSecHandle;
struct SecBuffer {
    ULONG cbBuffer;
    ULONG BufferType;
    PVOID pvBuffer;
}
alias SecBuffer* PSecBuffer;
alias SecHandle CredHandle;
alias PSecHandle PCredHandle;
alias SecHandle CtxtHandle;
alias PSecHandle PCtxtHandle;
struct SECURITY_INTEGER {
    uint LowPart;
    int HighPart;
}
alias SECURITY_INTEGER TimeStamp;
alias SECURITY_INTEGER* PTimeStamp;
struct SecBufferDesc {
    ULONG ulVersion;
    ULONG cBuffers;
    PSecBuffer pBuffers;
}
alias SecBufferDesc* PSecBufferDesc;
struct SecPkgContext_StreamSizes {
    ULONG cbHeader;
    ULONG cbTrailer;
    ULONG cbMaximumMessage;
    ULONG cBuffers;
    ULONG cbBlockSize;
}
alias SecPkgContext_StreamSizes* PSecPkgContext_StreamSizes;
struct SecPkgContext_Sizes {
    ULONG cbMaxToken;
    ULONG cbMaxSignature;
    ULONG cbBlockSize;
    ULONG cbSecurityTrailer;
}
alias SecPkgContext_Sizes* PSecPkgContext_Sizes;
struct SecPkgContext_AuthorityW {
    SEC_WCHAR* sAuthorityName;
}
alias SecPkgContext_AuthorityW* PSecPkgContext_AuthorityW;
struct SecPkgContext_AuthorityA {
    SEC_CHAR* sAuthorityName;
}
alias SecPkgContext_AuthorityA* PSecPkgContext_AuthorityA;
struct SecPkgContext_KeyInfoW {
    SEC_WCHAR* sSignatureAlgorithmName;
    SEC_WCHAR* sEncryptAlgorithmName;
    ULONG KeySize;
    ULONG SignatureAlgorithm;
    ULONG EncryptAlgorithm;
}
alias SecPkgContext_KeyInfoW* PSecPkgContext_KeyInfoW;
struct SecPkgContext_KeyInfoA {
    SEC_CHAR* sSignatureAlgorithmName;
    SEC_CHAR* sEncryptAlgorithmName;
    ULONG KeySize;
    ULONG SignatureAlgorithm;
    ULONG EncryptAlgorithm;
}
alias SecPkgContext_KeyInfoA* PSecPkgContext_KeyInfoA;
struct SecPkgContext_LifeSpan {
    TimeStamp tsStart;
    TimeStamp tsExpiry;
}
alias SecPkgContext_LifeSpan* PSecPkgContext_LifeSpan;
struct SecPkgContext_NamesW {
    SEC_WCHAR* sUserName;
}
alias SecPkgContext_NamesW* PSecPkgContext_NamesW;
struct SecPkgContext_NamesA {
    SEC_CHAR* sUserName;
}
alias SecPkgContext_NamesA* PSecPkgContext_NamesA;
struct SecPkgInfoW {
    ULONG fCapabilities;
    USHORT wVersion;
    USHORT wRPCID;
    ULONG cbMaxToken;
    SEC_WCHAR* Name;
    SEC_WCHAR* Comment;
}
alias SecPkgInfoW* PSecPkgInfoW;
struct SecPkgInfoA {
    ULONG fCapabilities;
    USHORT wVersion;
    USHORT wRPCID;
    ULONG cbMaxToken;
    SEC_CHAR* Name;
    SEC_CHAR* Comment;
}
alias SecPkgInfoA* PSecPkgInfoA;
/* supported only in win2k+, so it should be a PSecPkgInfoW */
/* PSDK does not say it has ANSI/Unicode versions */
struct SecPkgContext_PackageInfo {
    PSecPkgInfoW PackageInfo;
}
alias SecPkgContext_PackageInfo* PSecPkgContext_PackageInfo;
struct SecPkgCredentials_NamesW {
    SEC_WCHAR* sUserName;
}
alias SecPkgCredentials_NamesW* PSecPkgCredentials_NamesW;
struct SecPkgCredentials_NamesA {
    SEC_CHAR* sUserName;
}
alias SecPkgCredentials_NamesA* PSecPkgCredentials_NamesA;

/* TODO: missing type in SDK */
alias void function() SEC_GET_KEY_FN;

alias SECURITY_STATUS function(PULONG,PSecPkgInfoW*) ENUMERATE_SECURITY_PACKAGES_FN_W;
alias SECURITY_STATUS function(PULONG,PSecPkgInfoA*) ENUMERATE_SECURITY_PACKAGES_FN_A;
alias SECURITY_STATUS function(PCredHandle,ULONG,PVOID) QUERY_CREDENTIALS_ATTRIBUTES_FN_W;
alias SECURITY_STATUS function(PCredHandle,ULONG,PVOID) QUERY_CREDENTIALS_ATTRIBUTES_FN_A;
alias SECURITY_STATUS function(SEC_WCHAR*,SEC_WCHAR*,ULONG,PLUID,PVOID,SEC_GET_KEY_FN,PVOID,PCredHandle,PTimeStamp) ACQUIRE_CREDENTIALS_HANDLE_FN_W;
alias SECURITY_STATUS function(SEC_CHAR*,SEC_CHAR*,ULONG,PLUID,PVOID,SEC_GET_KEY_FN,PVOID,PCredHandle,PTimeStamp) ACQUIRE_CREDENTIALS_HANDLE_FN_A;
alias SECURITY_STATUS function(PCredHandle) FREE_CREDENTIALS_HANDLE_FN;
alias SECURITY_STATUS function(PCredHandle,PCtxtHandle,SEC_WCHAR*,ULONG,ULONG,ULONG,PSecBufferDesc,ULONG,PCtxtHandle,PSecBufferDesc,PULONG,PTimeStamp) INITIALIZE_SECURITY_CONTEXT_FN_W;
alias SECURITY_STATUS function(PCredHandle,PCtxtHandle,SEC_CHAR*,ULONG,ULONG,ULONG,PSecBufferDesc,ULONG,PCtxtHandle,PSecBufferDesc,PULONG,PTimeStamp) INITIALIZE_SECURITY_CONTEXT_FN_A;
alias SECURITY_STATUS function(PCredHandle,PCtxtHandle,PSecBufferDesc,ULONG,ULONG,PCtxtHandle,PSecBufferDesc,PULONG,PTimeStamp) ACCEPT_SECURITY_CONTEXT_FN;
alias SECURITY_STATUS function(PCtxtHandle,PSecBufferDesc) COMPLETE_AUTH_TOKEN_FN;
alias SECURITY_STATUS function(PCtxtHandle) DELETE_SECURITY_CONTEXT_FN;
alias SECURITY_STATUS function(PCtxtHandle,PSecBufferDesc) APPLY_CONTROL_TOKEN_FN_W;
alias SECURITY_STATUS function(PCtxtHandle,PSecBufferDesc) APPLY_CONTROL_TOKEN_FN_A;
alias SECURITY_STATUS function(PCtxtHandle,ULONG,PVOID) QUERY_CONTEXT_ATTRIBUTES_FN_A;
alias SECURITY_STATUS function(PCtxtHandle,ULONG,PVOID) QUERY_CONTEXT_ATTRIBUTES_FN_W;
alias SECURITY_STATUS function(PCtxtHandle) IMPERSONATE_SECURITY_CONTEXT_FN;
alias SECURITY_STATUS function(PCtxtHandle) REVERT_SECURITY_CONTEXT_FN;
alias SECURITY_STATUS function(PCtxtHandle,ULONG,PSecBufferDesc,ULONG) MAKE_SIGNATURE_FN;
alias SECURITY_STATUS function(PCtxtHandle,PSecBufferDesc,ULONG,PULONG) VERIFY_SIGNATURE_FN;
alias SECURITY_STATUS function(PVOID) FREE_CONTEXT_BUFFER_FN;
alias SECURITY_STATUS function(SEC_CHAR*,PSecPkgInfoA*) QUERY_SECURITY_PACKAGE_INFO_FN_A;
alias SECURITY_STATUS function(PCtxtHandle,HANDLE*) QUERY_SECURITY_CONTEXT_TOKEN_FN;
alias SECURITY_STATUS function(SEC_WCHAR*,PSecPkgInfoW*) QUERY_SECURITY_PACKAGE_INFO_FN_W;
alias SECURITY_STATUS function(PCtxtHandle,ULONG,PSecBufferDesc,ULONG) ENCRYPT_MESSAGE_FN;
alias SECURITY_STATUS function(PCtxtHandle,PSecBufferDesc,ULONG,PULONG) DECRYPT_MESSAGE_FN;

/* No, it really is FreeCredentialsHandle, see the thread beginning
 * http://sourceforge.net/mailarchive/message.php?msg_id=4321080 for a
 * discovery discussion. */
struct SecurityFunctionTableW{
    uint dwVersion;
    ENUMERATE_SECURITY_PACKAGES_FN_W EnumerateSecurityPackagesW;
    QUERY_CREDENTIALS_ATTRIBUTES_FN_W QueryCredentialsAttributesW;
    ACQUIRE_CREDENTIALS_HANDLE_FN_W AcquireCredentialsHandleW;
    FREE_CREDENTIALS_HANDLE_FN FreeCredentialsHandle;
    void* Reserved2;
    INITIALIZE_SECURITY_CONTEXT_FN_W InitializeSecurityContextW;
    ACCEPT_SECURITY_CONTEXT_FN AcceptSecurityContext;
    COMPLETE_AUTH_TOKEN_FN CompleteAuthToken;
    DELETE_SECURITY_CONTEXT_FN DeleteSecurityContext;
    APPLY_CONTROL_TOKEN_FN_W ApplyControlTokenW;
    QUERY_CONTEXT_ATTRIBUTES_FN_W QueryContextAttributesW;
    IMPERSONATE_SECURITY_CONTEXT_FN ImpersonateSecurityContext;
    REVERT_SECURITY_CONTEXT_FN RevertSecurityContext;
    MAKE_SIGNATURE_FN MakeSignature;
    VERIFY_SIGNATURE_FN VerifySignature;
    FREE_CONTEXT_BUFFER_FN FreeContextBuffer;
    QUERY_SECURITY_PACKAGE_INFO_FN_W QuerySecurityPackageInfoW;
    void* Reserved3;
    void* Reserved4;
    void* Reserved5;
    void* Reserved6;
    void* Reserved7;
    void* Reserved8;
    QUERY_SECURITY_CONTEXT_TOKEN_FN QuerySecurityContextToken;
    ENCRYPT_MESSAGE_FN EncryptMessage;
    DECRYPT_MESSAGE_FN DecryptMessage;
}
alias SecurityFunctionTableW* PSecurityFunctionTableW;
struct SecurityFunctionTableA{
    uint dwVersion;
    ENUMERATE_SECURITY_PACKAGES_FN_A EnumerateSecurityPackagesA;
    QUERY_CREDENTIALS_ATTRIBUTES_FN_A QueryCredentialsAttributesA;
    ACQUIRE_CREDENTIALS_HANDLE_FN_A AcquireCredentialsHandleA;
    FREE_CREDENTIALS_HANDLE_FN FreeCredentialsHandle;
    void* Reserved2;
    INITIALIZE_SECURITY_CONTEXT_FN_A InitializeSecurityContextA;
    ACCEPT_SECURITY_CONTEXT_FN AcceptSecurityContext;
    COMPLETE_AUTH_TOKEN_FN CompleteAuthToken;
    DELETE_SECURITY_CONTEXT_FN DeleteSecurityContext;
    APPLY_CONTROL_TOKEN_FN_A ApplyControlTokenA;
    QUERY_CONTEXT_ATTRIBUTES_FN_A QueryContextAttributesA;
    IMPERSONATE_SECURITY_CONTEXT_FN ImpersonateSecurityContext;
    REVERT_SECURITY_CONTEXT_FN RevertSecurityContext;
    MAKE_SIGNATURE_FN MakeSignature;
    VERIFY_SIGNATURE_FN VerifySignature;
    FREE_CONTEXT_BUFFER_FN FreeContextBuffer;
    QUERY_SECURITY_PACKAGE_INFO_FN_A QuerySecurityPackageInfoA;
    void* Reserved3;
    void* Reserved4;
    void* Unknown1;
    void* Unknown2;
    void* Unknown3;
    void* Unknown4;
    void* Unknown5;
    ENCRYPT_MESSAGE_FN EncryptMessage;
    DECRYPT_MESSAGE_FN DecryptMessage;
}
alias SecurityFunctionTableA* PSecurityFunctionTableA;
alias PSecurityFunctionTableA function() INIT_SECURITY_INTERFACE_A;
alias PSecurityFunctionTableW function() INIT_SECURITY_INTERFACE_W;

SECURITY_STATUS FreeCredentialsHandle(PCredHandle);
SECURITY_STATUS EnumerateSecurityPackagesA(PULONG,PSecPkgInfoA*);
SECURITY_STATUS EnumerateSecurityPackagesW(PULONG,PSecPkgInfoW*);
SECURITY_STATUS AcquireCredentialsHandleA(SEC_CHAR*,SEC_CHAR*,ULONG,PLUID,PVOID,SEC_GET_KEY_FN,PVOID,PCredHandle,PTimeStamp);
SECURITY_STATUS AcquireCredentialsHandleW(SEC_WCHAR*,SEC_WCHAR*,ULONG,PLUID,PVOID,SEC_GET_KEY_FN,PVOID,PCredHandle,PTimeStamp);
SECURITY_STATUS AcceptSecurityContext(PCredHandle,PCtxtHandle,PSecBufferDesc,ULONG,ULONG,PCtxtHandle,PSecBufferDesc,PULONG,PTimeStamp);
SECURITY_STATUS InitializeSecurityContextA(PCredHandle,PCtxtHandle,SEC_CHAR*,ULONG,ULONG,ULONG,PSecBufferDesc,ULONG,PCtxtHandle,PSecBufferDesc,PULONG,PTimeStamp);
SECURITY_STATUS InitializeSecurityContextW(PCredHandle,PCtxtHandle,SEC_WCHAR*,ULONG,ULONG,ULONG,PSecBufferDesc,ULONG,PCtxtHandle,PSecBufferDesc,PULONG,PTimeStamp);
SECURITY_STATUS FreeContextBuffer(PVOID);
SECURITY_STATUS QueryContextAttributesA(PCtxtHandle,ULONG,PVOID);
SECURITY_STATUS QueryContextAttributesW(PCtxtHandle,ULONG,PVOID);
SECURITY_STATUS QueryCredentialsAttributesA(PCredHandle,ULONG,PVOID);
SECURITY_STATUS QueryCredentialsAttributesW(PCredHandle,ULONG,PVOID);
static if (_WIN32_WINNT >= 0x500){
    SECURITY_STATUS QuerySecurityContextToken(PCtxtHandle,HANDLE*);
}
SECURITY_STATUS DecryptMessage(PCtxtHandle,PSecBufferDesc,ULONG,PULONG);
SECURITY_STATUS EncryptMessage(PCtxtHandle,ULONG,PSecBufferDesc,ULONG);
SECURITY_STATUS DeleteSecurityContext(PCtxtHandle);
SECURITY_STATUS CompleteAuthToken(PCtxtHandle,PSecBufferDesc);
SECURITY_STATUS ApplyControlTokenA(PCtxtHandle,PSecBufferDesc);
SECURITY_STATUS ApplyControlTokenW(PCtxtHandle,PSecBufferDesc);
SECURITY_STATUS ImpersonateSecurityContext(PCtxtHandle);
SECURITY_STATUS RevertSecurityContext(PCtxtHandle);
SECURITY_STATUS MakeSignature(PCtxtHandle,ULONG,PSecBufferDesc,ULONG);
SECURITY_STATUS VerifySignature(PCtxtHandle,PSecBufferDesc,ULONG,PULONG);
SECURITY_STATUS QuerySecurityPackageInfoA(SEC_CHAR*,PSecPkgInfoA*);
SECURITY_STATUS QuerySecurityPackageInfoW(SEC_WCHAR*,PSecPkgInfoW*);
PSecurityFunctionTableA InitSecurityInterfaceA();
PSecurityFunctionTableW InitSecurityInterfaceW();

version (Unicode) {
    alias UNISP_NAME_W UNISP_NAME;
    alias SecPkgInfoW SecPkgInfo;
    alias PSecPkgInfoW PSecPkgInfo;
    alias SecPkgCredentials_NamesW SecPkgCredentials_Names;
    alias PSecPkgCredentials_NamesW PSecPkgCredentials_Names;
    alias SecPkgContext_AuthorityW SecPkgContext_Authority;
    alias PSecPkgContext_AuthorityW PSecPkgContext_Authority;
    alias SecPkgContext_KeyInfoW SecPkgContext_KeyInfo;
    alias PSecPkgContext_KeyInfoW PSecPkgContext_KeyInfo;
    alias SecPkgContext_NamesW SecPkgContext_Names;
    alias PSecPkgContext_NamesW PSecPkgContext_Names;
    alias SecurityFunctionTableW SecurityFunctionTable;
    alias PSecurityFunctionTableW PSecurityFunctionTable;
    alias AcquireCredentialsHandleW AcquireCredentialsHandle;
    alias EnumerateSecurityPackagesW EnumerateSecurityPackages;
    alias InitializeSecurityContextW InitializeSecurityContext;
    alias QueryContextAttributesW QueryContextAttributes;
    alias QueryCredentialsAttributesW QueryCredentialsAttributes;
    alias QuerySecurityPackageInfoW QuerySecurityPackageInfo;
    alias ApplyControlTokenW ApplyControlToken;
    alias ENUMERATE_SECURITY_PACKAGES_FN_W ENUMERATE_SECURITY_PACKAGES_FN;
    alias QUERY_CREDENTIALS_ATTRIBUTES_FN_W QUERY_CREDENTIALS_ATTRIBUTES_FN;
    alias ACQUIRE_CREDENTIALS_HANDLE_FN_W ACQUIRE_CREDENTIALS_HANDLE_FN;
    alias INITIALIZE_SECURITY_CONTEXT_FN_W INITIALIZE_SECURITY_CONTEXT_FN;
    alias APPLY_CONTROL_TOKEN_FN_W APPLY_CONTROL_TOKEN_FN;
    alias QUERY_CONTEXT_ATTRIBUTES_FN_W QUERY_CONTEXT_ATTRIBUTES_FN;
    alias QUERY_SECURITY_PACKAGE_INFO_FN_W QUERY_SECURITY_PACKAGE_INFO_FN;
    alias INIT_SECURITY_INTERFACE_W INIT_SECURITY_INTERFACE;
}else{
    alias UNISP_NAME_A UNISP_NAME;
    alias SecPkgInfoA SecPkgInfo;
    alias PSecPkgInfoA PSecPkgInfo;
    alias SecPkgCredentials_NamesA SecPkgCredentials_Names;
    alias PSecPkgCredentials_NamesA PSecPkgCredentials_Names;
    alias SecPkgContext_AuthorityA SecPkgContext_Authority;
    alias PSecPkgContext_AuthorityA PSecPkgContext_Authority;
    alias SecPkgContext_KeyInfoA SecPkgContext_KeyInfo;
    alias PSecPkgContext_KeyInfoA PSecPkgContext_KeyInfo;
    alias SecPkgContext_NamesA SecPkgContext_Names;
    alias PSecPkgContext_NamesA PSecPkgContext_Names;
    alias SecurityFunctionTableA SecurityFunctionTable;
    alias PSecurityFunctionTableA PSecurityFunctionTable;
    alias AcquireCredentialsHandleA AcquireCredentialsHandle;
    alias EnumerateSecurityPackagesA EnumerateSecurityPackages;
    alias InitializeSecurityContextA InitializeSecurityContext;
    alias QueryContextAttributesA QueryContextAttributes;
    alias QueryCredentialsAttributesA QueryCredentialsAttributes;
    alias QuerySecurityPackageInfoA QuerySecurityPackageInfo;
    alias ApplyControlTokenA ApplyControlToken;
    alias ENUMERATE_SECURITY_PACKAGES_FN_A ENUMERATE_SECURITY_PACKAGES_FN;
    alias QUERY_CREDENTIALS_ATTRIBUTES_FN_A QUERY_CREDENTIALS_ATTRIBUTES_FN;
    alias ACQUIRE_CREDENTIALS_HANDLE_FN_A ACQUIRE_CREDENTIALS_HANDLE_FN;
    alias INITIALIZE_SECURITY_CONTEXT_FN_A INITIALIZE_SECURITY_CONTEXT_FN;
    alias APPLY_CONTROL_TOKEN_FN_A APPLY_CONTROL_TOKEN_FN;
    alias QUERY_CONTEXT_ATTRIBUTES_FN_A QUERY_CONTEXT_ATTRIBUTES_FN;
    alias QUERY_SECURITY_PACKAGE_INFO_FN_A QUERY_SECURITY_PACKAGE_INFO_FN;
    alias INIT_SECURITY_INTERFACE_A INIT_SECURITY_INTERFACE;
}

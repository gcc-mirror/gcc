/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_schannel.d)
 */
module core.sys.windows.schannel;
version (Windows):
@system:

import core.sys.windows.wincrypt;
import core.sys.windows.windef;

enum DWORD SCHANNEL_CRED_VERSION = 4;
enum SCHANNEL_SHUTDOWN           = 1;
/* Comment from MinGW
    ? Do these belong here or in wincrypt.h
 */
enum : DWORD {
    AUTHTYPE_CLIENT = 1,
    AUTHTYPE_SERVER = 2
}

enum DWORD
    SP_PROT_PCT1_SERVER = 0x01,
    SP_PROT_PCT1_CLIENT = 0x02,
    SP_PROT_SSL2_SERVER = 0x04,
    SP_PROT_SSL2_CLIENT = 0x08,
    SP_PROT_SSL3_SERVER = 0x10,
    SP_PROT_SSL3_CLIENT = 0x20,
    SP_PROT_TLS1_SERVER = 0x40,
    SP_PROT_TLS1_CLIENT = 0x80,
    SP_PROT_PCT1        = SP_PROT_PCT1_CLIENT | SP_PROT_PCT1_SERVER,
    SP_PROT_TLS1        = SP_PROT_TLS1_CLIENT | SP_PROT_TLS1_SERVER,
    SP_PROT_SSL2        = SP_PROT_SSL2_CLIENT | SP_PROT_SSL2_SERVER,
    SP_PROT_SSL3        = SP_PROT_SSL3_CLIENT | SP_PROT_SSL3_SERVER;

enum DWORD
    SCH_CRED_NO_SYSTEM_MAPPER                    = 0x0002,
    SCH_CRED_NO_SERVERNAME_CHECK                 = 0x0004,
    SCH_CRED_MANUAL_CRED_VALIDATION              = 0x0008,
    SCH_CRED_NO_DEFAULT_CREDS                    = 0x0010,
    SCH_CRED_AUTO_CRED_VALIDATION                = 0x0020,
    SCH_CRED_USE_DEFAULT_CREDS                   = 0x0040,
    SCH_CRED_REVOCATION_CHECK_END_CERT           = 0x0100,
    SCH_CRED_REVOCATION_CHECK_CHAIN              = 0x0200,
    SCH_CRED_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT = 0x0400,
    SCH_CRED_IGNORE_NO_REVOCATION_CHECK          = 0x0800,
    SCH_CRED_IGNORE_REVOCATION_OFFLINE           = 0x1000;

// No definition - presumably an opaque structure
struct _HMAPPER;

struct SCHANNEL_CRED {
    DWORD           dwVersion = SCHANNEL_CRED_VERSION;
    DWORD           cCreds;
    PCCERT_CONTEXT* paCred;
    HCERTSTORE      hRootStore;
    DWORD           cMappers;
    _HMAPPER**      aphMappers;
    DWORD           cSupportedAlgs;
    ALG_ID*         palgSupportedAlgs;
    DWORD           grbitEnabledProtocols;
    DWORD           dwMinimumCypherStrength;
    DWORD           dwMaximumCypherStrength;
    DWORD           dwSessionLifespan;
    DWORD           dwFlags;
    DWORD           reserved;
}
alias SCHANNEL_CRED* PSCHANNEL_CRED;

struct SecPkgCred_SupportedAlgs {
    DWORD   cSupportedAlgs;
    ALG_ID* palgSupportedAlgs;
}
alias SecPkgCred_SupportedAlgs* PSecPkgCred_SupportedAlgs;

struct SecPkgCred_CypherStrengths {
    DWORD dwMinimumCypherStrength;
    DWORD dwMaximumCypherStrength;
}
alias SecPkgCred_CypherStrengths* PSecPkgCred_CypherStrengths;

struct SecPkgCred_SupportedProtocols {
    DWORD grbitProtocol;
}
alias SecPkgCred_SupportedProtocols* PSecPkgCred_SupportedProtocols;

struct SecPkgContext_IssuerListInfoEx {
    PCERT_NAME_BLOB aIssuers;
    DWORD           cIssuers;
}
alias SecPkgContext_IssuerListInfoEx* PSecPkgContext_IssuerListInfoEx;

struct SecPkgContext_ConnectionInfo {
    DWORD  dwProtocol;
    ALG_ID aiCipher;
    DWORD  dwCipherStrength;
    ALG_ID aiHash;
    DWORD  dwHashStrength;
    ALG_ID aiExch;
    DWORD  dwExchStrength;
}
alias SecPkgContext_ConnectionInfo* PSecPkgContext_ConnectionInfo;

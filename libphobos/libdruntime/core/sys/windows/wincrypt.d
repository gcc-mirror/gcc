/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_wincrypt.d)
 */
module core.sys.windows.wincrypt;
version (Windows):
@system:
pragma(lib, "advapi32");

version (ANSI) {} else version = Unicode;

import core.sys.windows.w32api, core.sys.windows.winbase, core.sys.windows.windef;

/* FIXME:
 *  Types of some constants
 *  Types of macros
 *  Inits of various "size" and "version" members
 *  Why are some #ifdefs commented out?
 */

const TCHAR[]
    MS_DEF_PROV = "Microsoft Base Cryptographic Provider v1.0",
    MS_ENHANCED_PROV = "Microsoft Enhanced Cryptographic Provider v1.0",
    MS_STRONG_PROV = "Microsoft Strong Cryptographic Provider",
    MS_DEF_RSA_SIG_PROV = "Microsoft RSA Signature Cryptographic Provider",
    MS_DEF_RSA_SCHANNEL_PROV = "Microsoft RSA SChannel Cryptographic Provider",
    MS_DEF_DSS_PROV = "Microsoft Base DSS Cryptographic Provider",
    MS_DEF_DSS_DH_PROV
      = "Microsoft Base DSS and Diffie-Hellman Cryptographic Provider",
    MS_ENH_DSS_DH_PROV
      = "Microsoft Enhanced DSS and Diffie-Hellman Cryptographic Provider",
    MS_DEF_DH_SCHANNEL_PROV = "Microsoft DH SChannel Cryptographic Provider",
    MS_SCARD_PROV = "Microsoft Base Smart Card Crypto Provider";

static if (_WIN32_WINNT > 0x501) {
const TCHAR[] MS_ENH_RSA_AES_PROV
      = "Microsoft Enhanced RSA and AES Cryptographic Provider";
} else static if (_WIN32_WINNT == 0x501) {
const TCHAR[] MS_ENH_RSA_AES_PROV
      = "Microsoft Enhanced RSA and AES Cryptographic Provider (Prototype)";
}

ALG_ID GET_ALG_CLASS(ALG_ID x) { return x & 0xE000; }
ALG_ID GET_ALG_TYPE (ALG_ID x) { return x & 0x1E00; }
ALG_ID GET_ALG_SID  (ALG_ID x) { return x & 0x01FF; }

enum : ALG_ID {
    ALG_CLASS_ANY           = 0,
    ALG_CLASS_SIGNATURE     = 0x2000,
    ALG_CLASS_MSG_ENCRYPT   = 0x4000,
    ALG_CLASS_DATA_ENCRYPT  = 0x6000,
    ALG_CLASS_HASH          = 0x8000,
    ALG_CLASS_KEY_EXCHANGE  = 0xA000,
    ALG_CLASS_ALL           = 0xE000
}

enum : ALG_ID {
    ALG_TYPE_ANY           = 0,
    ALG_TYPE_DSS           = 0x0200,
    ALG_TYPE_RSA           = 0x0400,
    ALG_TYPE_BLOCK         = 0x0600,
    ALG_TYPE_STREAM        = 0x0800,
    ALG_TYPE_DH            = 0x0A00,
    ALG_TYPE_SECURECHANNEL = 0x0C00
}

enum : ALG_ID {
    ALG_SID_ANY          =  0,
    ALG_SID_RSA_ANY      =  0,
    ALG_SID_RSA_PKCS,
    ALG_SID_RSA_MSATWORK,
    ALG_SID_RSA_ENTRUST,
    ALG_SID_RSA_PGP,  // =  4
    ALG_SID_DSS_ANY      =  0,
    ALG_SID_DSS_PKCS,
    ALG_SID_DSS_DMS,  // =  2
    ALG_SID_DES          =  1,
    ALG_SID_3DES         =  3,
    ALG_SID_DESX,
    ALG_SID_IDEA,
    ALG_SID_CAST,
    ALG_SID_SAFERSK64,
    ALG_SID_SAFERSK128,
    ALG_SID_3DES_112,
    ALG_SID_SKIPJACK,
    ALG_SID_TEK,
    ALG_SID_CYLINK_MEK,
    ALG_SID_RC5,      // = 13
    ALG_SID_RC2          =  2,
    ALG_SID_RC4          =  1,
    ALG_SID_SEAL         =  2,
    ALG_SID_MD2          =  1,
    ALG_SID_MD4,
    ALG_SID_MD5,
    ALG_SID_SHA,
    ALG_SID_MAC,
    ALG_SID_RIPEMD,
    ALG_SID_RIPEMD160,
    ALG_SID_SSL3SHAMD5,
    ALG_SID_HMAC,
    ALG_SID_TLS1PRF,  // = 10
    ALG_SID_AES_128      = 14,
    ALG_SID_AES_192,
    ALG_SID_AES_256,
    ALG_SID_AES,      // = 17
    ALG_SID_EXAMPLE      = 80
}

enum : ALG_ID {
    CALG_MD2        = ALG_CLASS_HASH | ALG_TYPE_ANY | ALG_SID_MD2,
    CALG_MD4        = ALG_CLASS_HASH | ALG_TYPE_ANY | ALG_SID_MD4,
    CALG_MD5        = ALG_CLASS_HASH | ALG_TYPE_ANY | ALG_SID_MD5,
    CALG_SHA        = ALG_CLASS_HASH | ALG_TYPE_ANY | ALG_SID_SHA,
    CALG_SHA1       = CALG_SHA,
    CALG_MAC        = ALG_CLASS_HASH | ALG_TYPE_ANY | ALG_SID_MAC,
    CALG_3DES       = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_BLOCK | 3,
    CALG_CYLINK_MEK = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_BLOCK | 12,
    CALG_SKIPJACK   = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_BLOCK | 10,
    CALG_KEA_KEYX   = ALG_CLASS_KEY_EXCHANGE | ALG_TYPE_STREAM | ALG_TYPE_DSS | 4,
    CALG_RSA_SIGN   = ALG_CLASS_SIGNATURE | ALG_TYPE_RSA | ALG_SID_RSA_ANY,
    CALG_DSS_SIGN   = ALG_CLASS_SIGNATURE | ALG_TYPE_DSS | ALG_SID_DSS_ANY,
    CALG_RSA_KEYX   = ALG_CLASS_KEY_EXCHANGE | ALG_TYPE_RSA | ALG_SID_RSA_ANY,
    CALG_DES        = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_BLOCK | ALG_SID_DES,
    CALG_RC2        = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_BLOCK | ALG_SID_RC2,
    CALG_RC4        = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_STREAM | ALG_SID_RC4,
    CALG_SEAL       = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_STREAM | ALG_SID_SEAL,
    CALG_DH_EPHEM   = ALG_CLASS_KEY_EXCHANGE | ALG_TYPE_STREAM | ALG_TYPE_DSS
                      | ALG_SID_DSS_DMS,
    CALG_DESX       = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_BLOCK | ALG_SID_DESX,
// is undefined ALG_CLASS_DHASH in MinGW - presuming typo
    CALG_TLS1PRF    = ALG_CLASS_HASH | ALG_TYPE_ANY | ALG_SID_TLS1PRF,
    CALG_AES_128    = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_BLOCK | ALG_SID_AES_128,
    CALG_AES_192    = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_BLOCK | ALG_SID_AES_192,
    CALG_AES_256    = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_BLOCK | ALG_SID_AES_256,
    CALG_AES        = ALG_CLASS_DATA_ENCRYPT | ALG_TYPE_BLOCK | ALG_SID_AES,
}

enum {
    CRYPT_VERIFYCONTEXT = 0xF0000000,
}

enum {
    CRYPT_NEWKEYSET = 8,
    CRYPT_DELETEKEYSET = 16,
    CRYPT_MACHINE_KEYSET = 32,
    CRYPT_SILENT = 64,
}

enum {
    CRYPT_EXPORTABLE = 1,
    CRYPT_USER_PROTECTED = 2,
    CRYPT_CREATE_SALT = 4,
    CRYPT_UPDATE_KEY = 8,
}

enum {
    SIMPLEBLOB = 1,
    PUBLICKEYBLOB = 6,
    PRIVATEKEYBLOB = 7,
    PLAINTEXTKEYBLOB = 8,
    OPAQUEKEYBLOB = 9,
    PUBLICKEYBLOBEX = 10,
    SYMMETRICWRAPKEYBLOB = 11,
}

enum {
    AT_KEYEXCHANGE = 1,
    AT_SIGNATURE = 2,
}

enum {
    CRYPT_USERDATA = 1,
}

enum {
    PKCS5_PADDING = 1,
}

enum {
    CRYPT_MODE_CBC = 1,
    CRYPT_MODE_ECB = 2,
    CRYPT_MODE_OFB = 3,
    CRYPT_MODE_CFB = 4,
    CRYPT_MODE_CTS = 5,
    CRYPT_MODE_CBCI = 6,
    CRYPT_MODE_CFBP = 7,
    CRYPT_MODE_OFBP = 8,
    CRYPT_MODE_CBCOFM = 9,
    CRYPT_MODE_CBCOFMI = 10,
}

enum {
    CRYPT_ENCRYPT = 1,
    CRYPT_DECRYPT = 2,
    CRYPT_EXPORT = 4,
    CRYPT_READ = 8,
    CRYPT_WRITE = 16,
    CRYPT_MAC = 32,
}

enum {
    HP_ALGID = 1,
    HP_HASHVAL = 2,
    HP_HASHSIZE = 4,
    HP_HMAC_INFO = 5,
}

enum {
    CRYPT_FAILED = FALSE,
    CRYPT_SUCCEED = TRUE,
}

bool RCRYPT_SUCCEEDED(BOOL r) { return r==CRYPT_SUCCEED; }
bool RCRYPT_FAILED(BOOL r) { return r==CRYPT_FAILED; }

enum {
    PP_ENUMALGS = 1,
    PP_ENUMCONTAINERS = 2,
    PP_IMPTYPE = 3,
    PP_NAME = 4,
    PP_VERSION = 5,
    PP_CONTAINER = 6,
    PP_CHANGE_PASSWORD  = 7,
    PP_KEYSET_SEC_DESCR = 8,
    PP_CERTCHAIN    = 9,
    PP_KEY_TYPE_SUBTYPE = 10,
    PP_PROVTYPE = 16,
    PP_KEYSTORAGE   = 17,
    PP_APPLI_CERT   = 18,
    PP_SYM_KEYSIZE  = 19,
    PP_SESSION_KEYSIZE  = 20,
    PP_UI_PROMPT    = 21,
    PP_ENUMALGS_EX  = 22,
    PP_ENUMMANDROOTS = 25,
    PP_ENUMELECTROOTS = 26,
    PP_KEYSET_TYPE = 27,
    PP_ADMIN_PIN = 31,
    PP_KEYEXCHANGE_PIN = 32,
    PP_SIGNATURE_PIN = 33,
    PP_SIG_KEYSIZE_INC = 34,
    PP_KEYX_KEYSIZE_INC = 35,
    PP_UNIQUE_CONTAINER = 36,
    PP_SGC_INFO = 37,
    PP_USE_HARDWARE_RNG = 38,
    PP_KEYSPEC = 39,
    PP_ENUMEX_SIGNING_PROT = 40,
}

enum {
    CRYPT_FIRST = 1,
    CRYPT_NEXT = 2,
}

enum {
    CRYPT_IMPL_HARDWARE = 1,
    CRYPT_IMPL_SOFTWARE = 2,
    CRYPT_IMPL_MIXED = 3,
    CRYPT_IMPL_UNKNOWN = 4,
}

enum {
    PROV_RSA_FULL = 1,
    PROV_RSA_SIG = 2,
    PROV_DSS = 3,
    PROV_FORTEZZA = 4,
    PROV_MS_MAIL = 5,
    PROV_SSL = 6,
    PROV_STT_MER = 7,
    PROV_STT_ACQ = 8,
    PROV_STT_BRND = 9,
    PROV_STT_ROOT = 10,
    PROV_STT_ISS = 11,
    PROV_RSA_SCHANNEL = 12,
    PROV_DSS_DH = 13,
    PROV_EC_ECDSA_SIG = 14,
    PROV_EC_ECNRA_SIG = 15,
    PROV_EC_ECDSA_FULL = 16,
    PROV_EC_ECNRA_FULL = 17,
    PROV_DH_SCHANNEL = 18,
    PROV_SPYRUS_LYNKS = 20,
    PROV_RNG = 21,
    PROV_INTEL_SEC = 22,
    PROV_RSA_AES = 24,
    MAXUIDLEN = 64,
}

enum {
    CUR_BLOB_VERSION = 2,
}

enum {
    X509_ASN_ENCODING = 1,
    PKCS_7_ASN_ENCODING  = 65536,
}

enum {
    CERT_V1 = 0,
    CERT_V2 = 1,
    CERT_V3 = 2,
}

enum {
    CERT_E_CHAINING = (-2146762486),
    CERT_E_CN_NO_MATCH = (-2146762481),
    CERT_E_EXPIRED = (-2146762495),
    CERT_E_PURPOSE = (-2146762490),
    CERT_E_REVOCATION_FAILURE = (-2146762482),
    CERT_E_REVOKED = (-2146762484),
    CERT_E_ROLE = (-2146762493),
    CERT_E_UNTRUSTEDROOT = (-2146762487),
    CERT_E_UNTRUSTEDTESTROOT = (-2146762483),
    CERT_E_VALIDITYPERIODNESTING = (-2146762494),
    CERT_E_WRONG_USAGE = (-2146762480),
    CERT_E_PATHLENCONST = (-2146762492),
    CERT_E_CRITICAL = (-2146762491),
    CERT_E_ISSUERCHAINING = (-2146762489),
    CERT_E_MALFORMED = (-2146762488),
    CRYPT_E_REVOCATION_OFFLINE = (-2146885613),
    CRYPT_E_REVOKED = (-2146885616),
    TRUST_E_BASIC_CONSTRAINTS = (-2146869223),
    TRUST_E_CERT_SIGNATURE = (-2146869244),
    TRUST_E_FAIL = (-2146762485),
}

enum {
    CERT_TRUST_NO_ERROR = 0,
    CERT_TRUST_IS_NOT_TIME_VALID = 1,
    CERT_TRUST_IS_NOT_TIME_NESTED = 2,
    CERT_TRUST_IS_REVOKED = 4,
    CERT_TRUST_IS_NOT_SIGNATURE_VALID = 8,
    CERT_TRUST_IS_NOT_VALID_FOR_USAGE = 16,
    CERT_TRUST_IS_UNTRUSTED_ROOT = 32,
    CERT_TRUST_REVOCATION_STATUS_UNKNOWN = 64,
    CERT_TRUST_IS_CYCLIC = 128,
    CERT_TRUST_IS_PARTIAL_CHAIN = 65536,
    CERT_TRUST_CTL_IS_NOT_TIME_VALID = 131072,
    CERT_TRUST_CTL_IS_NOT_SIGNATURE_VALID = 262144,
    CERT_TRUST_CTL_IS_NOT_VALID_FOR_USAGE = 524288,
}

enum {
    CERT_TRUST_HAS_EXACT_MATCH_ISSUER = 1,
    CERT_TRUST_HAS_KEY_MATCH_ISSUER = 2,
    CERT_TRUST_HAS_NAME_MATCH_ISSUER = 4,
    CERT_TRUST_IS_SELF_SIGNED = 8,
    CERT_TRUST_IS_COMPLEX_CHAIN = 65536,
}

enum {
    CERT_CHAIN_POLICY_BASE              = cast(LPCSTR) 1,
    CERT_CHAIN_POLICY_AUTHENTICODE      = cast(LPCSTR) 2,
    CERT_CHAIN_POLICY_AUTHENTICODE_TS   = cast(LPCSTR) 3,
    CERT_CHAIN_POLICY_SSL               = cast(LPCSTR) 4,
    CERT_CHAIN_POLICY_BASIC_CONSTRAINTS = cast(LPCSTR) 5,
    CERT_CHAIN_POLICY_NT_AUTH           = cast(LPCSTR) 6,
}

enum {
    USAGE_MATCH_TYPE_AND = 0,
    USAGE_MATCH_TYPE_OR = 1,
}

enum {
    CERT_SIMPLE_NAME_STR = 1,
    CERT_OID_NAME_STR = 2,
    CERT_X500_NAME_STR = 3,
}
enum {
    CERT_NAME_STR_SEMICOLON_FLAG = 1073741824,
    CERT_NAME_STR_CRLF_FLAG = 134217728,
    CERT_NAME_STR_NO_PLUS_FLAG = 536870912,
    CERT_NAME_STR_NO_QUOTING_FLAG = 268435456,
    CERT_NAME_STR_REVERSE_FLAG = 33554432,
    CERT_NAME_STR_ENABLE_T61_UNICODE_FLAG = 131072,
}

enum {
    CERT_FIND_ANY = 0,
    CERT_FIND_CERT_ID = 1048576,
    CERT_FIND_CTL_USAGE = 655360,
    CERT_FIND_ENHKEY_USAGE = 655360,
    CERT_FIND_EXISTING = 851968,
    CERT_FIND_HASH = 65536,
    CERT_FIND_ISSUER_ATTR = 196612,
    CERT_FIND_ISSUER_NAME = 131076,
    CERT_FIND_ISSUER_OF = 786432,
    CERT_FIND_KEY_IDENTIFIER = 983040,
    CERT_FIND_KEY_SPEC = 589824,
    CERT_FIND_MD5_HASH = 262144,
    CERT_FIND_PROPERTY = 327680,
    CERT_FIND_PUBLIC_KEY = 393216,
    CERT_FIND_SHA1_HASH = 65536,
    CERT_FIND_SIGNATURE_HASH = 917504,
    CERT_FIND_SUBJECT_ATTR = 196615,
    CERT_FIND_SUBJECT_CERT = 720896,
    CERT_FIND_SUBJECT_NAME = 131079,
    CERT_FIND_SUBJECT_STR_A = 458759,
    CERT_FIND_SUBJECT_STR_W = 524295,
    CERT_FIND_ISSUER_STR_A = 458756,
    CERT_FIND_ISSUER_STR_W = 524292,
}

enum {
    CERT_FIND_OR_ENHKEY_USAGE_FLAG = 16,
    CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG  = 1,
    CERT_FIND_NO_ENHKEY_USAGE_FLAG  = 8,
    CERT_FIND_VALID_ENHKEY_USAGE_FLAG  = 32,
    CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG  = 2,
}

enum {
    CERT_CASE_INSENSITIVE_IS_RDN_ATTRS_FLAG  = 2,
    CERT_UNICODE_IS_RDN_ATTRS_FLAG = 1,
    CERT_CHAIN_FIND_BY_ISSUER = 1,
}

enum {
    CERT_CHAIN_FIND_BY_ISSUER_COMPARE_KEY_FLAG = 1,
    CERT_CHAIN_FIND_BY_ISSUER_COMPLEX_CHAIN_FLAG = 2,
    CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_URL_FLAG = 4,
    CERT_CHAIN_FIND_BY_ISSUER_LOCAL_MACHINE_FLAG = 8,
    CERT_CHAIN_FIND_BY_ISSUER_NO_KEY_FLAG = 16384,
    CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_FLAG = 32768,
}

enum {
    CERT_STORE_PROV_SYSTEM = 10,
    CERT_SYSTEM_STORE_LOCAL_MACHINE = 131072,
}

enum {
    szOID_PKIX_KP_SERVER_AUTH = "4235600",
    szOID_SERVER_GATED_CRYPTO = "4235658",
    szOID_SGC_NETSCAPE = "2.16.840.1.113730.4.1",
    szOID_PKIX_KP_CLIENT_AUTH = "1.3.6.1.5.5.7.3.2",
}

enum {
    CRYPT_NOHASHOID = 0x00000001,
    CRYPT_NO_SALT = 0x10,
    CRYPT_PREGEN = 0x40,
}

enum {
    CRYPT_RECIPIENT = 0x10,
    CRYPT_INITIATOR = 0x40,
    CRYPT_ONLINE = 0x80,
    CRYPT_SF = 0x100,
    CRYPT_CREATE_IV = 0x200,
    CRYPT_KEK = 0x400,
    CRYPT_DATA_KEY = 0x800,
    CRYPT_VOLATILE = 0x1000,
    CRYPT_SGCKEY = 0x2000,
}

enum {
    KP_IV               = 0x00000001,
    KP_SALT             = 0x00000002,
    KP_PADDING          = 0x00000003,
    KP_MODE             = 0x00000004,
    KP_MODE_BITS        = 0x00000005,
    KP_PERMISSIONS      = 0x00000006,
    KP_ALGID            = 0x00000007,
    KP_BLOCKLEN         = 0x00000008,
    KP_KEYLEN           = 0x00000009,
    KP_SALT_EX          = 0x0000000a,
    KP_P                = 0x0000000b,
    KP_G                = 0x0000000c,
    KP_Q                = 0x0000000d,
    KP_X                = 0x0000000e,
    KP_Y                = 0x0000000f,
    KP_RA               = 0x00000010,
    KP_RB               = 0x00000011,
    KP_INFO             = 0x00000012,
    KP_EFFECTIVE_KEYLEN = 0x00000013,
    KP_SCHANNEL_ALG     = 0x00000014,
    KP_PUB_PARAMS       = 0x00000027,
}

enum {
    CRYPT_FLAG_PCT1    = 0x0001,
    CRYPT_FLAG_SSL2    = 0x0002,
    CRYPT_FLAG_SSL3    = 0x0004,
    CRYPT_FLAG_TLS1    = 0x0008,
    CRYPT_FLAG_IPSEC   = 0x0010,
    CRYPT_FLAG_SIGNING = 0x0020,
}

enum {
    SCHANNEL_MAC_KEY    = 0x00000000,
    SCHANNEL_ENC_KEY    = 0x00000001,
}

enum {
    INTERNATIONAL_USAGE = 0x00000001,
}


alias UINT ALG_ID;
alias ULONG_PTR HCRYPTPROV, HCRYPTKEY, HCRYPTHASH;
alias PVOID HCERTSTORE, HCRYPTMSG, HCERTCHAINENGINE;

struct VTableProvStruc {
    FARPROC FuncVerifyImage;
}
alias VTableProvStruc* PVTableProvStruc;

struct _CRYPTOAPI_BLOB {
    DWORD cbData;
    BYTE* pbData;
}
alias _CRYPTOAPI_BLOB CRYPT_INTEGER_BLOB, CRYPT_UINT_BLOB,
  CRYPT_OBJID_BLOB, CERT_NAME_BLOB, CERT_RDN_VALUE_BLOB, CERT_BLOB,
  CRL_BLOB, DATA_BLOB, CRYPT_DATA_BLOB, CRYPT_HASH_BLOB,
  CRYPT_DIGEST_BLOB, CRYPT_DER_BLOB, CRYPT_ATTR_BLOB;
alias _CRYPTOAPI_BLOB* PCRYPT_INTEGER_BLOB, PCRYPT_UINT_BLOB,
  PCRYPT_OBJID_BLOB, PCERT_NAME_BLOB, PCERT_RDN_VALUE_BLOB, PCERT_BLOB,
  PCRL_BLOB, PDATA_BLOB, PCRYPT_DATA_BLOB, PCRYPT_HASH_BLOB,
  PCRYPT_DIGEST_BLOB, PCRYPT_DER_BLOB, PCRYPT_ATTR_BLOB;

// not described in SDK; has the same layout as HTTPSPolicyCallbackData
struct SSL_EXTRA_CERT_CHAIN_POLICY_PARA {
    DWORD  cbStruct;
    DWORD  dwAuthType;
    DWORD  fdwChecks;
    LPWSTR pwszServerName;
}
alias SSL_EXTRA_CERT_CHAIN_POLICY_PARA HTTPSPolicyCallbackData;
alias SSL_EXTRA_CERT_CHAIN_POLICY_PARA* PSSL_EXTRA_CERT_CHAIN_POLICY_PARA,
  PHTTPSPolicyCallbackData;

/* #if (_WIN32_WINNT>=0x500) */
struct CERT_CHAIN_POLICY_PARA {
    DWORD cbSize = CERT_CHAIN_POLICY_PARA.sizeof;
    DWORD dwFlags;
    void* pvExtraPolicyPara;
}
alias CERT_CHAIN_POLICY_PARA* PCERT_CHAIN_POLICY_PARA;

struct CERT_CHAIN_POLICY_STATUS {
    DWORD cbSize = CERT_CHAIN_POLICY_STATUS.sizeof;
    DWORD dwError;
    LONG  lChainIndex;
    LONG  lElementIndex;
    void* pvExtraPolicyStatus;
}
alias CERT_CHAIN_POLICY_STATUS* PCERT_CHAIN_POLICY_STATUS;
/* #endif */

struct CRYPT_ALGORITHM_IDENTIFIER {
    LPSTR pszObjId;
    CRYPT_OBJID_BLOB Parameters;
}
alias CRYPT_ALGORITHM_IDENTIFIER* PCRYPT_ALGORITHM_IDENTIFIER;

struct CRYPT_BIT_BLOB {
    DWORD cbData;
    BYTE* pbData;
    DWORD cUnusedBits;
}
alias CRYPT_BIT_BLOB* PCRYPT_BIT_BLOB;

struct CERT_PUBLIC_KEY_INFO {
    CRYPT_ALGORITHM_IDENTIFIER Algorithm;
    CRYPT_BIT_BLOB             PublicKey;
}
alias CERT_PUBLIC_KEY_INFO* PCERT_PUBLIC_KEY_INFO;

struct CERT_EXTENSION {
    LPSTR            pszObjId;
    BOOL             fCritical;
    CRYPT_OBJID_BLOB Value;
}
alias CERT_EXTENSION* PCERT_EXTENSION;

struct CERT_INFO {
    DWORD dwVersion;
    CRYPT_INTEGER_BLOB SerialNumber;
    CRYPT_ALGORITHM_IDENTIFIER SignatureAlgorithm;
    CERT_NAME_BLOB Issuer;
    FILETIME NotBefore;
    FILETIME NotAfter;
    CERT_NAME_BLOB Subject;
    CERT_PUBLIC_KEY_INFO SubjectPublicKeyInfo;
    CRYPT_BIT_BLOB IssuerUniqueId;
    CRYPT_BIT_BLOB SubjectUniqueId;
    DWORD cExtension;
    PCERT_EXTENSION rgExtension;
}
alias CERT_INFO* PCERT_INFO;

struct CERT_CONTEXT {
    DWORD      dwCertEncodingType;
    BYTE*      pbCertEncoded;
    DWORD      cbCertEncoded;
    PCERT_INFO pCertInfo;
    HCERTSTORE hCertStore;
}
alias CERT_CONTEXT*        PCERT_CONTEXT;
alias const(CERT_CONTEXT)* PCCERT_CONTEXT;

struct CTL_USAGE {
    DWORD  cUsageIdentifier;
    LPSTR* rgpszUsageIdentifier;
}
alias CTL_USAGE CERT_ENHKEY_USAGE;
alias CTL_USAGE* PCTRL_USAGE, PCERT_ENHKEY_USAGE;

struct CERT_USAGE_MATCH {
    DWORD             dwType;
    CERT_ENHKEY_USAGE Usage;
}
alias CERT_USAGE_MATCH* PCERT_USAGE_MATCH;
/* #if (_WIN32_WINNT>=0x500) */

struct CERT_CHAIN_PARA {
    DWORD            cbSize = CERT_CHAIN_PARA.sizeof;
    CERT_USAGE_MATCH RequestedUsage;
//#if CERT_CHAIN_PARA_HAS_EXTRA_FIELDS
    CERT_USAGE_MATCH RequestedIssuancePolicy;
    DWORD            dwUrlRetrievalTimeout;
    BOOL             fCheckRevocationFreshnessTime;
    DWORD            dwRevocationFreshnessTime;
//#endif
}
alias CERT_CHAIN_PARA* PCERT_CHAIN_PARA;

extern (Windows) alias BOOL function(PCCERT_CONTEXT, void*)
  PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK;

struct CERT_CHAIN_FIND_BY_ISSUER_PARA {
    DWORD  cbSize = CERT_CHAIN_FIND_BY_ISSUER_PARA.sizeof;
    LPCSTR pszUsageIdentifier;
    DWORD  dwKeySpec;
    DWORD  dwAcquirePrivateKeyFlags;
    DWORD  cIssuer;
    CERT_NAME_BLOB* rgIssuer;
    PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK pfnFIndCallback;
    void*  pvFindArg;
    DWORD* pdwIssuerChainIndex;
    DWORD* pdwIssuerElementIndex;
}
alias CERT_CHAIN_FIND_BY_ISSUER_PARA* PCERT_CHAIN_FIND_BY_ISSUER_PARA;
/* #endif */

struct CERT_TRUST_STATUS {
    DWORD dwErrorStatus;
    DWORD dwInfoStatus;
}
alias CERT_TRUST_STATUS* PCERT_TRUST_STATUS;

struct CRL_ENTRY {
    CRYPT_INTEGER_BLOB SerialNumber;
    FILETIME           RevocationDate;
    DWORD              cExtension;
    PCERT_EXTENSION    rgExtension;
}
alias CRL_ENTRY* PCRL_ENTRY;

struct CRL_INFO {
    DWORD           dwVersion;
    CRYPT_ALGORITHM_IDENTIFIER SignatureAlgorithm;
    CERT_NAME_BLOB  Issuer;
    FILETIME        ThisUpdate;
    FILETIME        NextUpdate;
    DWORD           cCRLEntry;
    PCRL_ENTRY      rgCRLEntry;
    DWORD           cExtension;
    PCERT_EXTENSION rgExtension;
}
alias CRL_INFO* PCRL_INFO;

struct CRL_CONTEXT {
    DWORD      dwCertEncodingType;
    BYTE*      pbCrlEncoded;
    DWORD      cbCrlEncoded;
    PCRL_INFO  pCrlInfo;
    HCERTSTORE hCertStore;
}
alias CRL_CONTEXT*        PCRL_CONTEXT;
alias const(CRL_CONTEXT)* PCCRL_CONTEXT;

struct CERT_REVOCATION_CRL_INFO {
    DWORD         cbSize = CERT_REVOCATION_CRL_INFO.sizeof;
    PCCRL_CONTEXT pBaseCRLContext;
    PCCRL_CONTEXT pDeltaCRLContext;
    PCRL_ENTRY    pCrlEntry;
    BOOL          fDeltaCrlEntry;
}
alias CERT_REVOCATION_CRL_INFO* PCERT_REVOCATION_CRL_INFO;

struct CERT_REVOCATION_INFO {
    DWORD  cbSize = CERT_REVOCATION_INFO.sizeof;
    DWORD  dwRevocationResult;
    LPCSTR pszRevocationOid;
    LPVOID pvOidSpecificInfo;
    BOOL   fHasFreshnessTime;
    DWORD  dwFreshnessTime;
    PCERT_REVOCATION_CRL_INFO pCrlInfo;
}
alias CERT_REVOCATION_INFO* PCERT_REVOCATION_INFO;

/* #if (_WIN32_WINNT>=0x500) */
struct CERT_CHAIN_ELEMENT {
    DWORD                 cbSize = CERT_CHAIN_ELEMENT.sizeof;
    PCCERT_CONTEXT        pCertContext;
    CERT_TRUST_STATUS     TrustStatus;
    PCERT_REVOCATION_INFO pRevocationInfo;
    PCERT_ENHKEY_USAGE    pIssuanceUsage;
    PCERT_ENHKEY_USAGE    pApplicationUsage;
}
alias CERT_CHAIN_ELEMENT* PCERT_CHAIN_ELEMENT;
/* #endif */

struct CRYPT_ATTRIBUTE {
    LPSTR            pszObjId;
    DWORD            cValue;
    PCRYPT_ATTR_BLOB rgValue;
}
alias CRYPT_ATTRIBUTE* PCRYPT_ATTRIBUTE;

struct CTL_ENTRY {
    CRYPT_DATA_BLOB  SubjectIdentifier;
    DWORD            cAttribute;
    PCRYPT_ATTRIBUTE rgAttribute;
}
alias CTL_ENTRY* PCTL_ENTRY;

struct CTL_INFO {
    DWORD              dwVersion;
    CTL_USAGE          SubjectUsage;
    CRYPT_DATA_BLOB    ListIdentifier;
    CRYPT_INTEGER_BLOB SequenceNumber;
    FILETIME           ThisUpdate;
    FILETIME           NextUpdate;
    CRYPT_ALGORITHM_IDENTIFIER SubjectAlgorithm;
    DWORD              cCTLEntry;
    PCTL_ENTRY         rgCTLEntry;
    DWORD              cExtension;
    PCERT_EXTENSION    rgExtension;
}
alias CTL_INFO* PCTL_INFO;

struct CTL_CONTEXT {
    DWORD      dwMsgAndCertEncodingType;
    BYTE*      pbCtlEncoded;
    DWORD      cbCtlEncoded;
    PCTL_INFO  pCtlInfo;
    HCERTSTORE hCertStore;
    HCRYPTMSG  hCryptMsg;
    BYTE*      pbCtlContent;
    DWORD      cbCtlContent;
}
alias CTL_CONTEXT*        PCTL_CONTEXT;
alias const(CTL_CONTEXT)* PCCTL_CONTEXT;

struct CERT_TRUST_LIST_INFO {
    DWORD         cbSize = CERT_TRUST_LIST_INFO.sizeof;
    PCTL_ENTRY    pCtlEntry;
    PCCTL_CONTEXT pCtlContext;
}
alias CERT_TRUST_LIST_INFO* PCERT_TRUST_LIST_INFO;

struct CERT_SIMPLE_CHAIN {
    DWORD                 cbSize = CERT_SIMPLE_CHAIN.sizeof;
    CERT_TRUST_STATUS     TrustStatus;
    DWORD                 cElement;
    PCERT_CHAIN_ELEMENT*  rgpElement;
    PCERT_TRUST_LIST_INFO pTrustListInfo;
    BOOL                  fHasRevocationFreshnessTime;
    DWORD                 dwRevocationFreshnessTime;
}
alias CERT_SIMPLE_CHAIN* PCERT_SIMPLE_CHAIN;

/* #if (_WIN32_WINNT>=0x500) */
alias const(CERT_CHAIN_CONTEXT)* PCCERT_CHAIN_CONTEXT;
struct CERT_CHAIN_CONTEXT {
    DWORD                 cbSize = CERT_CHAIN_CONTEXT.sizeof;
    CERT_TRUST_STATUS     TrustStatus;
    DWORD                 cChain;
    PCERT_SIMPLE_CHAIN*   rgpChain;
    DWORD                 cLowerQualityChainContext;
    PCCERT_CHAIN_CONTEXT* rgpLowerQualityChainContext;
    BOOL                  fHasRevocationFreshnessTime;
    DWORD                 dwRevocationFreshnessTime;
}
alias CERT_CHAIN_CONTEXT* PCERT_CHAIN_CONTEXT;
/* #endif */

struct PROV_ENUMALGS {
    ALG_ID   aiAlgid;
    DWORD    dwBitLen;
    DWORD    dwNameLen;
    CHAR[20] szName = 0;
}

struct PUBLICKEYSTRUC {
    BYTE   bType;
    BYTE   bVersion;
    WORD   reserved;
    ALG_ID aiKeyAlg;
}
alias PUBLICKEYSTRUC BLOBHEADER;

struct RSAPUBKEY {
    DWORD magic;
    DWORD bitlen;
    DWORD pubexp;
}

struct HMAC_INFO {
    ALG_ID HashAlgid;
    BYTE*  pbInnerString;
    DWORD  cbInnerString;
    BYTE*  pbOuterString;
    DWORD  cbOuterString;
}
alias HMAC_INFO* PHMAC_INFO;

extern (Windows) @nogc nothrow {
    BOOL CertCloseStore(HCERTSTORE, DWORD);
    BOOL CertGetCertificateChain(HCERTCHAINENGINE, PCCERT_CONTEXT, LPFILETIME,
      HCERTSTORE, PCERT_CHAIN_PARA, DWORD, LPVOID, PCCERT_CHAIN_CONTEXT*);
    BOOL CertVerifyCertificateChainPolicy(LPCSTR, PCCERT_CHAIN_CONTEXT,
      PCERT_CHAIN_POLICY_PARA, PCERT_CHAIN_POLICY_STATUS);
    void CertFreeCertificateChain(PCCERT_CHAIN_CONTEXT);
    DWORD CertNameToStrA(DWORD, PCERT_NAME_BLOB, DWORD, LPSTR, DWORD);
    DWORD CertNameToStrW(DWORD, PCERT_NAME_BLOB, DWORD, LPWSTR, DWORD);
    HCERTSTORE CertOpenSystemStoreA(HCRYPTPROV, LPCSTR);
    HCERTSTORE CertOpenSystemStoreW(HCRYPTPROV, LPCWSTR);
    HCERTSTORE CertOpenStore(LPCSTR, DWORD, HCRYPTPROV, DWORD, const(void)*);
    PCCERT_CONTEXT CertFindCertificateInStore(HCERTSTORE, DWORD, DWORD, DWORD,
const(void)*, PCCERT_CONTEXT);
    BOOL CertFreeCertificateContext(PCCERT_CONTEXT);
    PCCERT_CONTEXT CertGetIssuerCertificateFromStore(HCERTSTORE,
      PCCERT_CONTEXT, PCCERT_CONTEXT, DWORD*);
    PCCERT_CHAIN_CONTEXT CertFindChainInStore(HCERTSTORE, DWORD, DWORD, DWORD,
const(void)*, PCCERT_CHAIN_CONTEXT);

    BOOL CryptAcquireContextA(HCRYPTPROV*, LPCSTR, LPCSTR, DWORD, DWORD);
    BOOL CryptAcquireContextW(HCRYPTPROV*, LPCWSTR, LPCWSTR, DWORD, DWORD);
     BOOL CryptContextAddRef(HCRYPTPROV, DWORD*, DWORD);
    BOOL CryptReleaseContext(HCRYPTPROV, ULONG_PTR);
    BOOL CryptGenKey(HCRYPTPROV, ALG_ID, DWORD, HCRYPTKEY*);
    BOOL CryptDeriveKey(HCRYPTPROV, ALG_ID, HCRYPTHASH, DWORD, HCRYPTKEY*);
    BOOL CryptDestroyKey(HCRYPTKEY);
    static if (_WIN32_WINNT >= 0x500) {
        BOOL CryptDuplicateHash(HCRYPTHASH, DWORD*, DWORD, HCRYPTHASH*);
        BOOL CryptDuplicateKey(HCRYPTKEY, DWORD*, DWORD, HCRYPTKEY*);
    }
    BOOL CryptSetKeyParam(HCRYPTKEY, DWORD, PBYTE, DWORD);
    BOOL CryptGetKeyParam(HCRYPTKEY, DWORD, PBYTE, PDWORD, DWORD);
    BOOL CryptSetHashParam(HCRYPTHASH, DWORD, PBYTE, DWORD);
    BOOL CryptGetHashParam(HCRYPTHASH, DWORD, PBYTE, PDWORD, DWORD);
    BOOL CryptSetProvParam(HCRYPTPROV, DWORD, PBYTE, DWORD);
    BOOL CryptGetProvParam(HCRYPTPROV, DWORD, PBYTE, PDWORD, DWORD);
    BOOL CryptGenRandom(HCRYPTPROV, DWORD, PBYTE);
    BOOL CryptGetUserKey(HCRYPTPROV, DWORD, HCRYPTKEY*);
    BOOL CryptExportKey(HCRYPTKEY, HCRYPTKEY, DWORD, DWORD, PBYTE, PDWORD);
    BOOL CryptImportKey(HCRYPTPROV, PBYTE, DWORD, HCRYPTKEY, DWORD,
      HCRYPTKEY*);
    BOOL CryptEncrypt(HCRYPTKEY, HCRYPTHASH, BOOL, DWORD, PBYTE, PDWORD,
      DWORD);
    BOOL CryptDecrypt(HCRYPTKEY, HCRYPTHASH, BOOL, DWORD, PBYTE, PDWORD);
    BOOL CryptCreateHash(HCRYPTPROV, ALG_ID, HCRYPTKEY, DWORD, HCRYPTHASH*);
    BOOL CryptHashData(HCRYPTHASH, PBYTE, DWORD, DWORD);
    BOOL CryptHashSessionKey(HCRYPTHASH, HCRYPTKEY, DWORD);
    BOOL CryptGetHashValue(HCRYPTHASH, DWORD, PBYTE, PDWORD);
    BOOL CryptDestroyHash(HCRYPTHASH);
    BOOL CryptSignHashA(HCRYPTHASH, DWORD, LPCSTR, DWORD, PBYTE, PDWORD);
    BOOL CryptSignHashW(HCRYPTHASH, DWORD, LPCWSTR, DWORD, PBYTE, PDWORD);
    BOOL CryptVerifySignatureA(HCRYPTHASH, PBYTE, DWORD, HCRYPTKEY, LPCSTR,
      DWORD);
    BOOL CryptVerifySignatureW(HCRYPTHASH, PBYTE, DWORD, HCRYPTKEY, LPCWSTR,
      DWORD);
    BOOL CryptSetProviderA(LPCSTR, DWORD);
    BOOL CryptSetProviderW(LPCWSTR, DWORD);
}

version (Unicode) {
    alias CertNameToStrW CertNameToStr;
    alias CryptAcquireContextW CryptAcquireContext;
    alias CryptSignHashW CryptSignHash;
    alias CryptVerifySignatureW CryptVerifySignature;
    alias CryptSetProviderW CryptSetProvider;
    alias CertOpenSystemStoreW CertOpenSystemStore;
    /+alias CERT_FIND_SUBJECT_STR_W CERT_FIND_SUBJECT_STR;
    alias CERT_FIND_ISSUER_STR_W CERT_FIND_ISSUER_STR;+/
} else {
    alias CertNameToStrA CertNameToStr;
    alias CryptAcquireContextA CryptAcquireContext;
    alias CryptSignHashA CryptSignHash;
    alias CryptVerifySignatureA CryptVerifySignature;
    alias CryptSetProviderA CryptSetProvider;
    alias CertOpenSystemStoreA CertOpenSystemStore;
    /+alias CERT_FIND_SUBJECT_STR_A CERT_FIND_SUBJECT_STR;
    alias CERT_FIND_ISSUER_STR_A CERT_FIND_ISSUER_STR;+/
}

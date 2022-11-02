/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_winldap.d)
 */
module core.sys.windows.winldap;
version (Windows):

version (ANSI) {} else version = Unicode;

/* Comment from MinGW
  winldap.h - Header file for the Windows LDAP API

  Written by Filip Navara <xnavara@volny.cz>

  References:
    The C LDAP Application Program Interface
    http://www.watersprings.org/pub/id/draft-ietf-ldapext-ldap-c-api-05.txt

    Lightweight Directory Access Protocol Reference
    http://msdn.microsoft.com/library/en-us/netdir/ldap/ldap_reference.asp

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/

import core.sys.windows.schannel, core.sys.windows.winber;
import core.sys.windows.wincrypt, core.sys.windows.windef;

//align(4):

enum {
    LDAP_VERSION1    = 1,
    LDAP_VERSION2    = 2,
    LDAP_VERSION3    = 3,
    LDAP_VERSION     = LDAP_VERSION2,
    LDAP_VERSION_MIN = LDAP_VERSION2,
    LDAP_VERSION_MAX = LDAP_VERSION3
}

/*  MinGW defines ANSI and Unicode versions as LDAP_VENDOR_NAME and
 *  LDAP_VENDOR_NAME_W respectively; similarly with other string constants
 *  defined in this module.
 */
const TCHAR[] LDAP_VENDOR_NAME = "Microsoft Corporation.";

enum LDAP_API_VERSION          = 2004;
enum LDAP_VENDOR_VERSION       =  510;
enum LDAP_API_INFO_VERSION     =    1;
enum LDAP_FEATURE_INFO_VERSION =    1;

enum {
    LDAP_SUCCESS                    = 0x00,
    LDAP_OPT_SUCCESS                = LDAP_SUCCESS,
    LDAP_OPERATIONS_ERROR,
    LDAP_PROTOCOL_ERROR,
    LDAP_TIMELIMIT_EXCEEDED,
    LDAP_SIZELIMIT_EXCEEDED,
    LDAP_COMPARE_FALSE,
    LDAP_COMPARE_TRUE,
    LDAP_STRONG_AUTH_NOT_SUPPORTED,
    LDAP_AUTH_METHOD_NOT_SUPPORTED  = LDAP_STRONG_AUTH_NOT_SUPPORTED,
    LDAP_STRONG_AUTH_REQUIRED,
    LDAP_REFERRAL_V2,
    LDAP_PARTIAL_RESULTS            = LDAP_REFERRAL_V2,
    LDAP_REFERRAL,
    LDAP_ADMIN_LIMIT_EXCEEDED,
    LDAP_UNAVAILABLE_CRIT_EXTENSION,
    LDAP_CONFIDENTIALITY_REQUIRED,
    LDAP_SASL_BIND_IN_PROGRESS,  // = 0x0e
    LDAP_NO_SUCH_ATTRIBUTE          = 0x10,
    LDAP_UNDEFINED_TYPE,
    LDAP_INAPPROPRIATE_MATCHING,
    LDAP_CONSTRAINT_VIOLATION,
    LDAP_TYPE_OR_VALUE_EXISTS,
    LDAP_ATTRIBUTE_OR_VALUE_EXISTS  = LDAP_TYPE_OR_VALUE_EXISTS,
    LDAP_INVALID_SYNTAX,         // = 0x15
    LDAP_NO_SUCH_OBJECT             = 0x20,
    LDAP_ALIAS_PROBLEM,
    LDAP_INVALID_DN_SYNTAX,
    LDAP_IS_LEAF,
    LDAP_ALIAS_DEREF_PROBLEM,    // = 0x24
    LDAP_INAPPROPRIATE_AUTH         = 0x30,
    LDAP_INVALID_CREDENTIALS,
    LDAP_INSUFFICIENT_ACCESS,
    LDAP_INSUFFICIENT_RIGHTS        = LDAP_INSUFFICIENT_ACCESS,
    LDAP_BUSY,
    LDAP_UNAVAILABLE,
    LDAP_UNWILLING_TO_PERFORM,
    LDAP_LOOP_DETECT,            // = 0x36
    LDAP_NAMING_VIOLATION           = 0x40,
    LDAP_OBJECT_CLASS_VIOLATION,
    LDAP_NOT_ALLOWED_ON_NONLEAF,
    LDAP_NOT_ALLOWED_ON_RDN,
    LDAP_ALREADY_EXISTS,
    LDAP_NO_OBJECT_CLASS_MODS,
    LDAP_RESULTS_TOO_LARGE,
    LDAP_AFFECTS_MULTIPLE_DSAS,  // = 0x47
    LDAP_OTHER                      = 0x50,
    LDAP_SERVER_DOWN,
    LDAP_LOCAL_ERROR,
    LDAP_ENCODING_ERROR,
    LDAP_DECODING_ERROR,
    LDAP_TIMEOUT,
    LDAP_AUTH_UNKNOWN,
    LDAP_FILTER_ERROR,
    LDAP_USER_CANCELLED,
    LDAP_PARAM_ERROR,
    LDAP_NO_MEMORY,
    LDAP_CONNECT_ERROR,
    LDAP_NOT_SUPPORTED,
    LDAP_CONTROL_NOT_FOUND,
    LDAP_NO_RESULTS_RETURNED,
    LDAP_MORE_RESULTS_TO_RETURN,
    LDAP_CLIENT_LOOP,
    LDAP_REFERRAL_LIMIT_EXCEEDED // = 0x61
}

enum {
    LDAP_PORT        =  389,
    LDAP_SSL_PORT    =  636,
    LDAP_GC_PORT     = 3268,
    LDAP_SSL_GC_PORT = 3269
}

enum void*
    LDAP_OPT_OFF = null,
    LDAP_OPT_ON = cast(void*) 1;

enum {
    LDAP_OPT_API_INFO               = 0x00,
    LDAP_OPT_DESC,
    LDAP_OPT_DEREF,
    LDAP_OPT_SIZELIMIT,
    LDAP_OPT_TIMELIMIT,
    LDAP_OPT_THREAD_FN_PTRS,
    LDAP_OPT_REBIND_FN,
    LDAP_OPT_REBIND_ARG,
    LDAP_OPT_REFERRALS,
    LDAP_OPT_RESTART,
    LDAP_OPT_SSL,
    LDAP_OPT_TLS                    = LDAP_OPT_SSL,
    LDAP_OPT_IO_FN_PTRS,         // = 0x0b
    LDAP_OPT_CACHE_FN_PTRS          = 0x0d,
    LDAP_OPT_CACHE_STRATEGY,
    LDAP_OPT_CACHE_ENABLE,
    LDAP_OPT_REFERRAL_HOP_LIMIT,
    LDAP_OPT_PROTOCOL_VERSION,
    LDAP_OPT_VERSION                = LDAP_OPT_PROTOCOL_VERSION,
    LDAP_OPT_SERVER_CONTROLS,
    LDAP_OPT_CLIENT_CONTROLS,    // = 0x13
    LDAP_OPT_API_FEATURE_INFO       = 0x15,
    LDAP_OPT_HOST_NAME              = 0x30,
    LDAP_OPT_ERROR_NUMBER,
    LDAP_OPT_ERROR_STRING,
    LDAP_OPT_SERVER_ERROR,
    LDAP_OPT_SERVER_EXT_ERROR,   // = 0x34
    LDAP_OPT_PING_KEEP_ALIVE        = 0x36,
    LDAP_OPT_PING_WAIT_TIME,
    LDAP_OPT_PING_LIMIT,         // = 0x38
    LDAP_OPT_DNSDOMAIN_NAME         = 0x3b,
    LDAP_OPT_GETDSNAME_FLAGS        = 0x3d,
    LDAP_OPT_HOST_REACHABLE,
    LDAP_OPT_PROMPT_CREDENTIALS,
    LDAP_OPT_TCP_KEEPALIVE,      // = 0x40
    LDAP_OPT_REFERRAL_CALLBACK      = 0x70,
    LDAP_OPT_CLIENT_CERTIFICATE     = 0x80,
    LDAP_OPT_SERVER_CERTIFICATE, // = 0x81
    LDAP_OPT_AUTO_RECONNECT         = 0x91,
    LDAP_OPT_SSPI_FLAGS,
    LDAP_OPT_SSL_INFO,
    LDAP_OPT_TLS_INFO               = LDAP_OPT_SSL_INFO,
    LDAP_OPT_REF_DEREF_CONN_PER_MSG,
    LDAP_OPT_SIGN,
    LDAP_OPT_ENCRYPT,
    LDAP_OPT_SASL_METHOD,
    LDAP_OPT_AREC_EXCLUSIVE,
    LDAP_OPT_SECURITY_CONTEXT,
    LDAP_OPT_ROOTDSE_CACHE       // = 0x9a
}

enum {
    LDAP_DEREF_NEVER,
    LDAP_DEREF_SEARCHING,
    LDAP_DEREF_FINDING,
    LDAP_DEREF_ALWAYS
}

enum LDAP_NO_LIMIT = 0;

const TCHAR[] LDAP_CONTROL_REFERRALS = "1.2.840.113556.1.4.616";

// FIXME: check type (declared with U suffix in MinGW)
enum : uint {
    LDAP_CHASE_SUBORDINATE_REFERRALS = 0x20,
    LDAP_CHASE_EXTERNAL_REFERRALS    = 0x40
}

enum {
    LDAP_SCOPE_DEFAULT = -1,
    LDAP_SCOPE_BASE,
    LDAP_SCOPE_ONELEVEL,
    LDAP_SCOPE_SUBTREE
}

enum {
    LDAP_MOD_ADD,
    LDAP_MOD_DELETE,
    LDAP_MOD_REPLACE,
    LDAP_MOD_BVALUES = 0x80
}

enum : int {
    LDAP_RES_BIND             = 0x61,
    LDAP_RES_SEARCH_ENTRY     = 0x64,
    LDAP_RES_SEARCH_RESULT    = 0x65,
    LDAP_RES_MODIFY           = 0x67,
    LDAP_RES_ADD              = 0x69,
    LDAP_RES_DELETE           = 0x6b,
    LDAP_RES_MODRDN           = 0x6d,
    LDAP_RES_COMPARE          = 0x6f,
    LDAP_RES_SEARCH_REFERENCE = 0x73,
    LDAP_RES_EXTENDED         = 0x78,
    LDAP_RES_ANY              = -1
}

enum {
    LDAP_MSG_ONE,
    LDAP_MSG_ALL,
    LDAP_MSG_RECEIVED
}

const TCHAR[]
    LDAP_SERVER_SORT_OID         = "1.2.840.113556.1.4.473",
    LDAP_SERVER_RESP_SORT_OID    = "1.2.840.113556.1.4.474",
    LDAP_PAGED_RESULT_OID_STRING = "1.2.840.113556.1.4.319",
    LDAP_CONTROL_VLVREQUEST      = "2.16.840.1.113730.3.4.9",
    LDAP_CONTROL_VLVRESPONSE     = "2.16.840.1.113730.3.4.10",
    LDAP_START_TLS_OID           = "1.3.6.1.4.1.1466.20037",
    LDAP_TTL_EXTENDED_OP_OID     = "1.3.6.1.4.1.1466.101.119.1";

enum {
    LDAP_AUTH_NONE      = 0x00U,
    LDAP_AUTH_SIMPLE    = 0x80U,
    LDAP_AUTH_SASL      = 0x83U,
    LDAP_AUTH_OTHERKIND = 0x86U,
    LDAP_AUTH_EXTERNAL  = LDAP_AUTH_OTHERKIND | 0x0020U,
    LDAP_AUTH_SICILY    = LDAP_AUTH_OTHERKIND | 0x0200U,
    LDAP_AUTH_NEGOTIATE = LDAP_AUTH_OTHERKIND | 0x0400U,
    LDAP_AUTH_MSN       = LDAP_AUTH_OTHERKIND | 0x0800U,
    LDAP_AUTH_NTLM      = LDAP_AUTH_OTHERKIND | 0x1000U,
    LDAP_AUTH_DIGEST    = LDAP_AUTH_OTHERKIND | 0x4000U,
    LDAP_AUTH_DPA       = LDAP_AUTH_OTHERKIND | 0x2000U,
    LDAP_AUTH_SSPI      = LDAP_AUTH_NEGOTIATE
}

enum {
    LDAP_FILTER_AND        = 0xa0,
    LDAP_FILTER_OR,
    LDAP_FILTER_NOT,
    LDAP_FILTER_EQUALITY,
    LDAP_FILTER_SUBSTRINGS,
    LDAP_FILTER_GE,
    LDAP_FILTER_LE,     // = 0xa6
    LDAP_FILTER_APPROX     = 0xa8,
    LDAP_FILTER_EXTENSIBLE,
    LDAP_FILTER_PRESENT    = 0x87
}

enum {
    LDAP_SUBSTRING_INITIAL = 0x80,
    LDAP_SUBSTRING_ANY,
    LDAP_SUBSTRING_FINAL
}

// should be opaque structure
struct LDAP {
    struct _ld_sp {
        UINT_PTR sb_sd;
        UCHAR[(10*ULONG.sizeof)+1] Reserved1;
        ULONG_PTR sb_naddr;
        UCHAR[(6*ULONG.sizeof)] Reserved2;
    }
    _ld_sp   ld_sp;
    PCHAR    ld_host;
    ULONG    ld_version;
    UCHAR    ld_lberoptions;
    int      ld_deref;
    int      ld_timelimit;
    int      ld_sizelimit;
    int      ld_errno;
    PCHAR    ld_matched;
    PCHAR    ld_error;
}
alias LDAP* PLDAP;

// should be opaque structure
struct LDAPMessage {
    ULONG        lm_msgid;
    ULONG        lm_msgtype;
    BerElement*  lm_ber;
    LDAPMessage* lm_chain;
    LDAPMessage* lm_next;
    ULONG        lm_time;
}
alias LDAPMessage* PLDAPMessage;

struct LDAP_TIMEVAL {
    LONG tv_sec;
    LONG tv_usec;
}
alias LDAP_TIMEVAL* PLDAP_TIMEVAL;

struct LDAPAPIInfoA {
    int    ldapai_info_version;
    int    ldapai_api_version;
    int    ldapai_protocol_version;
    char** ldapai_extensions;
    char*  ldapai_vendor_name;
    int    ldapai_vendor_version;
}
alias LDAPAPIInfoA* PLDAPAPIInfoA;

struct LDAPAPIInfoW {
    int     ldapai_info_version;
    int     ldapai_api_version;
    int     ldapai_protocol_version;
    PWCHAR* ldapai_extensions;
    PWCHAR  ldapai_vendor_name;
    int     ldapai_vendor_version;
}
alias LDAPAPIInfoW* PLDAPAPIInfoW;

struct LDAPAPIFeatureInfoA {
    int   ldapaif_info_version;
    char* ldapaif_name;
    int   ldapaif_version;
}
alias LDAPAPIFeatureInfoA* PLDAPAPIFeatureInfoA;

struct LDAPAPIFeatureInfoW {
    int    ldapaif_info_version;
    PWCHAR ldapaif_name;
    int    ldapaif_version;
}
alias LDAPAPIFeatureInfoW* PLDAPAPIFeatureInfoW;

struct LDAPControlA {
    PCHAR    ldctl_oid;
    BerValue ldctl_value;
    BOOLEAN  ldctl_iscritical;
}
alias LDAPControlA* PLDAPControlA;

struct LDAPControlW {
    PWCHAR   ldctl_oid;
    BerValue ldctl_value;
    BOOLEAN  ldctl_iscritical;
}
alias LDAPControlW* PLDAPControlW;

/*  Do we really need these?  In MinGW, LDAPModA/W have only mod_op, mod_type
 *  and mod_vals, and macros are used to simulate anonymous unions in those
 *  structures.
 */
union mod_vals_u_tA {
    PCHAR*     modv_strvals;
    BerValue** modv_bvals;
}

union mod_vals_u_tW {
    PWCHAR*    modv_strvals;
    BerValue** modv_bvals;
}

struct LDAPModA {
    ULONG         mod_op;
    PCHAR         mod_type;

    union {
        mod_vals_u_tA mod_vals;
        // The following members are defined as macros in MinGW.
        PCHAR*        mod_values;
        BerValue**    mod_bvalues;
    }
}
alias LDAPModA* PLDAPModA;

struct LDAPModW {
    ULONG         mod_op;
    PWCHAR        mod_type;

    union {
        mod_vals_u_tW mod_vals;
        // The following members are defined as macros in MinGW.
        PWCHAR*       mod_values;
        BerValue**    mod_bvalues;
    }
}
alias LDAPModW* PLDAPModW;

/* Opaque structure
 *  http://msdn.microsoft.com/library/en-us/ldap/ldap/ldapsearch.asp
 */
struct LDAPSearch;
alias LDAPSearch* PLDAPSearch;

struct LDAPSortKeyA {
    PCHAR   sk_attrtype;
    PCHAR   sk_matchruleoid;
    BOOLEAN sk_reverseorder;
}
alias LDAPSortKeyA* PLDAPSortKeyA;

struct LDAPSortKeyW {
    PWCHAR  sk_attrtype;
    PWCHAR  sk_matchruleoid;
    BOOLEAN sk_reverseorder;
}
alias LDAPSortKeyW* PLDAPSortKeyW;

/*  MinGW defines these as immediate function typedefs, which don't translate
 *  well into D.
 */
extern (C) {
    alias ULONG function(PLDAP, PLDAP, PWCHAR, PCHAR, ULONG, PVOID, PVOID,
      PLDAP*) QUERYFORCONNECTION;
    alias BOOLEAN function(PLDAP, PLDAP, PWCHAR, PCHAR, PLDAP, ULONG, PVOID,
      PVOID, ULONG) NOTIFYOFNEWCONNECTION;
    alias ULONG function(PLDAP, PLDAP) DEREFERENCECONNECTION;
    alias BOOLEAN function(PLDAP, PSecPkgContext_IssuerListInfoEx,
      PCCERT_CONTEXT*) QUERYCLIENTCERT;
}

struct LDAP_REFERRAL_CALLBACK {
    ULONG                  SizeOfCallbacks;
    QUERYFORCONNECTION*    QueryForConnection;
    NOTIFYOFNEWCONNECTION* NotifyRoutine;
    DEREFERENCECONNECTION* DereferenceRoutine;
}
alias LDAP_REFERRAL_CALLBACK* PLDAP_REFERRAL_CALLBACK;

struct LDAPVLVInfo {
    int       ldvlv_version;
    uint      ldvlv_before_count;
    uint      ldvlv_after_count;
    uint      ldvlv_offset;
    uint      ldvlv_count;
    BerValue* ldvlv_attrvalue;
    BerValue* ldvlv_context;
    void*     ldvlv_extradata;
}

/*
 * Under Microsoft WinLDAP the function ldap_error is only stub.
 * This macro uses LDAP structure to get error string and pass it to the user.
 */
private extern (C) int printf(const scope char* format, ...);
int ldap_perror(LDAP* handle, char* message) {
    return printf("%s: %s\n", message, handle.ld_error);
}

/*  FIXME: In MinGW, these are WINLDAPAPI == DECLSPEC_IMPORT.  Linkage
 *  attribute?
 */
extern (C) {
    PLDAP ldap_initA(PCHAR, ULONG);
    PLDAP ldap_initW(PWCHAR, ULONG);
    PLDAP ldap_openA(PCHAR, ULONG);
    PLDAP ldap_openW(PWCHAR, ULONG);
    PLDAP cldap_openA(PCHAR, ULONG);
    PLDAP cldap_openW(PWCHAR, ULONG);
    ULONG ldap_connect(LDAP*, LDAP_TIMEVAL*);
    PLDAP ldap_sslinitA(PCHAR, ULONG, int);
    PLDAP ldap_sslinitW(PWCHAR, ULONG, int);
    ULONG ldap_start_tls_sA(LDAP*, PLDAPControlA*, PLDAPControlA*);
    ULONG ldap_start_tls_sW(LDAP*, PLDAPControlW*, PLDAPControlW*);
    BOOLEAN ldap_stop_tls_s(LDAP*);
    ULONG ldap_get_optionA(LDAP*, int, void*);
    ULONG ldap_get_optionW(LDAP*, int, void*);
    ULONG ldap_set_optionA(LDAP*, int, void*);
    ULONG ldap_set_optionW(LDAP*, int, void*);
    ULONG ldap_control_freeA(LDAPControlA*);
    ULONG ldap_control_freeW(LDAPControlW*);
    ULONG ldap_controls_freeA(LDAPControlA**);
    ULONG ldap_controls_freeW(LDAPControlW**);
    ULONG ldap_free_controlsA(LDAPControlA**);
    ULONG ldap_free_controlsW(LDAPControlW**);
    ULONG ldap_sasl_bindA(LDAP*, PCSTR, PCSTR, BERVAL*, PLDAPControlA*,
      PLDAPControlA*, int*);
    ULONG ldap_sasl_bindW(LDAP*, PCWSTR, PCWSTR, BERVAL*, PLDAPControlW*,
      PLDAPControlW*, int*);
    ULONG ldap_sasl_bind_sA(LDAP*, PCSTR, PCSTR, BERVAL*, PLDAPControlA*,
      PLDAPControlA*, PBERVAL*);
    ULONG ldap_sasl_bind_sW(LDAP*, PCWSTR, PCWSTR, BERVAL*, PLDAPControlW*,
      PLDAPControlW*, PBERVAL*);
    ULONG ldap_simple_bindA(LDAP*, PSTR, PSTR);
    ULONG ldap_simple_bindW(LDAP*, PWSTR, PWSTR);
    ULONG ldap_simple_bind_sA(LDAP*, PSTR, PSTR);
    ULONG ldap_simple_bind_sW(LDAP*, PWSTR, PWSTR);
    ULONG ldap_unbind(LDAP*);
    ULONG ldap_unbind_s(LDAP*);
    ULONG ldap_search_extA(LDAP*, PCSTR, ULONG, PCSTR, PZPSTR, ULONG,
      PLDAPControlA*, PLDAPControlA*, ULONG, ULONG, ULONG*);
    ULONG ldap_search_extW(LDAP*, PCWSTR, ULONG, PCWSTR, PZPWSTR, ULONG,
      PLDAPControlW*, PLDAPControlW*, ULONG, ULONG, ULONG*);
    ULONG ldap_search_ext_sA(LDAP*, PCSTR, ULONG, PCSTR, PZPSTR, ULONG,
      PLDAPControlA*, PLDAPControlA*, LDAP_TIMEVAL*, ULONG, PLDAPMessage*);
    ULONG ldap_search_ext_sW(LDAP*, PCWSTR, ULONG, PCWSTR, PZPWSTR, ULONG,
      PLDAPControlW*, PLDAPControlW*, LDAP_TIMEVAL*, ULONG, PLDAPMessage*);
    ULONG ldap_searchA(LDAP*, PCSTR, ULONG, PCSTR, PZPSTR, ULONG);
    ULONG ldap_searchW(LDAP*, PCWSTR, ULONG, PCWSTR, PZPWSTR, ULONG);
    ULONG ldap_search_sA(LDAP*, PCSTR, ULONG, PCSTR, PZPSTR, ULONG,
      PLDAPMessage*);
    ULONG ldap_search_sW(LDAP*, PCWSTR, ULONG, PCWSTR, PZPWSTR, ULONG,
      PLDAPMessage*);
    ULONG ldap_search_stA(LDAP*, PCSTR, ULONG, PCSTR, PZPSTR, ULONG,
      LDAP_TIMEVAL*, PLDAPMessage*);
    ULONG ldap_search_stW(LDAP*, PCWSTR, ULONG, PCWSTR, PZPWSTR, ULONG,
      LDAP_TIMEVAL*, PLDAPMessage*);
    ULONG ldap_compare_extA(LDAP*, PCSTR, PCSTR, PCSTR, BerValue*,
      PLDAPControlA*, PLDAPControlA*, ULONG*);
    ULONG ldap_compare_extW(LDAP*, PCWSTR, PCWSTR, PCWSTR, BerValue*,
      PLDAPControlW*, PLDAPControlW*, ULONG*);
    ULONG ldap_compare_ext_sA(LDAP*, PCSTR, PCSTR, PCSTR, BerValue*,
      PLDAPControlA*, PLDAPControlA*);
    ULONG ldap_compare_ext_sW(LDAP*, PCWSTR, PCWSTR, PCWSTR, BerValue*,
      PLDAPControlW*, PLDAPControlW*);
    ULONG ldap_compareA(LDAP*, PCSTR, PCSTR, PCSTR);
    ULONG ldap_compareW(LDAP*, PCWSTR, PCWSTR, PCWSTR);
    ULONG ldap_compare_sA(LDAP*, PCSTR, PCSTR, PCSTR);
    ULONG ldap_compare_sW(LDAP*, PCWSTR, PCWSTR, PCWSTR);
    ULONG ldap_modify_extA(LDAP*, PCSTR, LDAPModA**, PLDAPControlA*,
      PLDAPControlA*, ULONG*);
    ULONG ldap_modify_extW(LDAP*, PCWSTR, LDAPModW**, PLDAPControlW*,
      PLDAPControlW*, ULONG*);
    ULONG ldap_modify_ext_sA(LDAP*, PCSTR, LDAPModA**, PLDAPControlA*,
      PLDAPControlA*);
    ULONG ldap_modify_ext_sW(LDAP*, PCWSTR, LDAPModW**, PLDAPControlW*,
      PLDAPControlW*);
    ULONG ldap_modifyA(LDAP*, PSTR, LDAPModA**);
    ULONG ldap_modifyW(LDAP*, PWSTR, LDAPModW**);
    ULONG ldap_modify_sA(LDAP*, PSTR, LDAPModA**);
    ULONG ldap_modify_sW(LDAP*, PWSTR, LDAPModW**);
    ULONG ldap_rename_extA(LDAP*, PCSTR, PCSTR, PCSTR, INT, PLDAPControlA*,
      PLDAPControlA*, ULONG*);
    ULONG ldap_rename_extW(LDAP*, PCWSTR, PCWSTR, PCWSTR, INT, PLDAPControlW*,
      PLDAPControlW*, ULONG*);
    ULONG ldap_rename_ext_sA(LDAP*, PCSTR, PCSTR, PCSTR, INT,
      PLDAPControlA*, PLDAPControlA*);
    ULONG ldap_rename_ext_sW(LDAP*, PCWSTR, PCWSTR, PCWSTR, INT,
      PLDAPControlW*, PLDAPControlW*);
    ULONG ldap_add_extA(LDAP*, PCSTR, LDAPModA**, PLDAPControlA*,
      PLDAPControlA*, ULONG*);
    ULONG ldap_add_extW(LDAP*, PCWSTR, LDAPModW**, PLDAPControlW*,
      PLDAPControlW*, ULONG*);
    ULONG ldap_add_ext_sA(LDAP*, PCSTR, LDAPModA**, PLDAPControlA*,
      PLDAPControlA*);
    ULONG ldap_add_ext_sW(LDAP*, PCWSTR, LDAPModW**, PLDAPControlW*,
      PLDAPControlW*);
    ULONG ldap_addA(LDAP*, PSTR, LDAPModA**);
    ULONG ldap_addW(LDAP*, PWSTR, LDAPModW**);
    ULONG ldap_add_sA(LDAP*, PSTR, LDAPModA**);
    ULONG ldap_add_sW(LDAP*, PWSTR, LDAPModW**);
    ULONG ldap_delete_extA(LDAP*, PCSTR, PLDAPControlA*, PLDAPControlA*,
      ULONG*);
    ULONG ldap_delete_extW(LDAP*, PCWSTR, PLDAPControlW*, PLDAPControlW*,
      ULONG*);
    ULONG ldap_delete_ext_sA(LDAP*, PCSTR, PLDAPControlA*, PLDAPControlA*);
    ULONG ldap_delete_ext_sW(LDAP*, PCWSTR, PLDAPControlW*, PLDAPControlW*);
    ULONG ldap_deleteA(LDAP*, PCSTR);
    ULONG ldap_deleteW(LDAP*, PCWSTR);
    ULONG ldap_delete_sA(LDAP*, PCSTR);
    ULONG ldap_delete_sW(LDAP*, PCWSTR);
    ULONG ldap_extended_operationA(LDAP*, PCSTR, BerValue*, PLDAPControlA*,
      PLDAPControlA*, ULONG*);
    ULONG ldap_extended_operationW(LDAP*, PCWSTR, BerValue*, PLDAPControlW*,
      PLDAPControlW*, ULONG*);
    ULONG ldap_extended_operation_sA(LDAP*, PSTR, BerValue*, PLDAPControlA*,
      PLDAPControlA*, PCHAR*, BerValue**);
    ULONG ldap_extended_operation_sW(LDAP*, PWSTR, BerValue*, PLDAPControlW*,
      PLDAPControlW*, PWCHAR*, BerValue**);
    ULONG ldap_close_extended_op(LDAP*, ULONG);
    ULONG ldap_abandon(LDAP*, ULONG);
    ULONG ldap_result(LDAP*, ULONG, ULONG, LDAP_TIMEVAL*, LDAPMessage**);
    ULONG ldap_msgfree(LDAPMessage*);
    ULONG ldap_parse_resultA(LDAP*, LDAPMessage*, ULONG*, PSTR*, PSTR*,
      PZPSTR*, PLDAPControlA**, BOOLEAN);
    ULONG ldap_parse_resultW(LDAP*, LDAPMessage*, ULONG*, PWSTR*, PWSTR*,
      PZPWSTR*, PLDAPControlW**, BOOLEAN);
    ULONG ldap_parse_extended_resultA(LDAP, LDAPMessage*, PSTR*, BerValue**,
      BOOLEAN);
    ULONG ldap_parse_extended_resultW(LDAP, LDAPMessage*, PWSTR*, BerValue**,
      BOOLEAN);
    PCHAR ldap_err2stringA(ULONG);
    PWCHAR ldap_err2stringW(ULONG);
    ULONG LdapGetLastError();
    ULONG LdapMapErrorToWin32(ULONG);
    ULONG ldap_result2error(LDAP*, LDAPMessage*, ULONG);
    PLDAPMessage ldap_first_entry(LDAP*, LDAPMessage*);
    PLDAPMessage ldap_next_entry(LDAP*, LDAPMessage*);
    PLDAPMessage ldap_first_reference(LDAP*, LDAPMessage*);
    PLDAPMessage ldap_next_reference(LDAP*, LDAPMessage*);
    ULONG ldap_count_entries(LDAP*, LDAPMessage*);
    ULONG ldap_count_references(LDAP*, LDAPMessage*);
    PCHAR ldap_first_attributeA(LDAP*, LDAPMessage*, BerElement**);
    PWCHAR ldap_first_attributeW(LDAP*, LDAPMessage*, BerElement**);
    PCHAR ldap_next_attributeA(LDAP*, LDAPMessage*, BerElement*);
    PWCHAR ldap_next_attributeW(LDAP*, LDAPMessage*, BerElement*);
    VOID ldap_memfreeA(PCHAR);
    VOID ldap_memfreeW(PWCHAR);
    PCHAR* ldap_get_valuesA(LDAP*, LDAPMessage*, PCSTR);
    PWCHAR* ldap_get_valuesW(LDAP*, LDAPMessage*, PCWSTR);
    BerValue** ldap_get_values_lenA(LDAP*, LDAPMessage*, PCSTR);
    BerValue** ldap_get_values_lenW(LDAP*, LDAPMessage*, PCWSTR);
    ULONG ldap_count_valuesA(PCHAR*);
    ULONG ldap_count_valuesW(PWCHAR*);
    ULONG ldap_count_values_len(BerValue**);
    ULONG ldap_value_freeA(PCHAR*);
    ULONG ldap_value_freeW(PWCHAR*);
    ULONG ldap_value_free_len(BerValue**);
    PCHAR ldap_get_dnA(LDAP*, LDAPMessage*);
    PWCHAR ldap_get_dnW(LDAP*, LDAPMessage*);
    PCHAR ldap_explode_dnA(PCSTR, ULONG);
    PWCHAR ldap_explode_dnW(PCWSTR, ULONG);
    PCHAR ldap_dn2ufnA(PCSTR);
    PWCHAR ldap_dn2ufnW(PCWSTR);
    ULONG ldap_ufn2dnA(PCSTR, PSTR*);
    ULONG ldap_ufn2dnW(PCWSTR, PWSTR*);
    ULONG ldap_parse_referenceA(LDAP*, LDAPMessage*, PCHAR**);
    ULONG ldap_parse_referenceW(LDAP*, LDAPMessage*, PWCHAR**);
    ULONG ldap_check_filterA(LDAP*, PSTR);
    ULONG ldap_check_filterW(LDAP*, PWSTR);
    ULONG ldap_create_page_controlA(PLDAP, ULONG, BerValue*, UCHAR,
      PLDAPControlA*);
    ULONG ldap_create_page_controlW(PLDAP, ULONG, BerValue*, UCHAR,
      PLDAPControlW*);
    ULONG ldap_create_sort_controlA(PLDAP, PLDAPSortKeyA*, UCHAR,
      PLDAPControlA*);
    ULONG ldap_create_sort_controlW(PLDAP, PLDAPSortKeyW*, UCHAR,
    PLDAPControlW*);
    INT ldap_create_vlv_controlA(LDAP*, LDAPVLVInfo*, UCHAR, PLDAPControlA*);
    INT ldap_create_vlv_controlW(LDAP*, LDAPVLVInfo*, UCHAR, PLDAPControlW*);
    ULONG ldap_encode_sort_controlA(PLDAP, PLDAPSortKeyA*, PLDAPControlA,
      BOOLEAN);
    ULONG ldap_encode_sort_controlW(PLDAP, PLDAPSortKeyW*, PLDAPControlW,
      BOOLEAN);
    ULONG ldap_escape_filter_elementA(PCHAR, ULONG, PCHAR, ULONG);
    ULONG ldap_escape_filter_elementW(PWCHAR, ULONG, PWCHAR, ULONG);
    ULONG ldap_get_next_page(PLDAP, PLDAPSearch, ULONG, ULONG*);
    ULONG ldap_get_next_page_s(PLDAP, PLDAPSearch, LDAP_TIMEVAL*, ULONG,
      ULONG*, LDAPMessage**);
    ULONG ldap_get_paged_count(PLDAP, PLDAPSearch, ULONG*, PLDAPMessage);
    ULONG ldap_parse_page_controlA(PLDAP, PLDAPControlA*, ULONG*, BerValue**);
    ULONG ldap_parse_page_controlW(PLDAP, PLDAPControlW*, ULONG*, BerValue**);
    ULONG ldap_parse_sort_controlA(PLDAP, PLDAPControlA*, ULONG*, PCHAR*);
    ULONG ldap_parse_sort_controlW(PLDAP, PLDAPControlW*, ULONG*, PWCHAR*);
    INT ldap_parse_vlv_controlA(PLDAP, PLDAPControlA*, PULONG, PULONG,
      BerValue**, PINT);
    INT ldap_parse_vlv_controlW(PLDAP, PLDAPControlW*, PULONG, PULONG,
      BerValue**, PINT);
    PLDAPSearch ldap_search_init_pageA(PLDAP, PCSTR, ULONG, PCSTR, PZPSTR,
      ULONG, PLDAPControlA*, PLDAPControlA*, ULONG, ULONG, PLDAPSortKeyA*);
    PLDAPSearch ldap_search_init_pageW(PLDAP, PCWSTR, ULONG, PCWSTR, PZPWSTR,
      ULONG, PLDAPControlW*, PLDAPControlW*, ULONG, ULONG, PLDAPSortKeyW*);
    ULONG ldap_search_abandon_page(PLDAP, PLDAPSearch);
    LDAP ldap_conn_from_msg(LDAP*, LDAPMessage*);
    INT LdapUnicodeToUTF8(LPCWSTR, int, LPSTR, int);
    INT LdapUTF8ToUnicode(LPCSTR, int, LPWSTR, int);
    ULONG ldap_bindA(LDAP*, PSTR, PCHAR, ULONG);
    ULONG ldap_bindW(LDAP*, PWSTR, PWCHAR, ULONG);
    ULONG ldap_bind_sA(LDAP*, PSTR, PCHAR, ULONG);
    ULONG ldap_bind_sW(LDAP*, PWSTR, PWCHAR, ULONG);
    deprecated ("For LDAP 3 or later, use the ldap_rename_ext or ldap_rename_ext_s functions") {
        ULONG ldap_modrdnA(LDAP*, PCSTR, PCSTR);
        ULONG ldap_modrdnW(LDAP*, PCWSTR, PCWSTR);
        ULONG ldap_modrdn_sA(LDAP*, PCSTR, PCSTR);
        ULONG ldap_modrdn_sW(LDAP*, PCWSTR, PCWSTR);
        ULONG ldap_modrdn2A(LDAP*, PCSTR, PCSTR, INT);
        ULONG ldap_modrdn2W(LDAP*, PCWSTR, PCWSTR, INT);
        ULONG ldap_modrdn2_sA(LDAP*, PCSTR, PCSTR, INT);
        ULONG ldap_modrdn2_sW(LDAP*, PCWSTR, PCWSTR, INT);
    }
}

version (Unicode) {
    alias LDAPControlW LDAPControl;
    alias PLDAPControlW PLDAPControl;
    alias LDAPModW LDAPMod;
    alias LDAPModW PLDAPMod;
    alias LDAPSortKeyW LDAPSortKey;
    alias PLDAPSortKeyW PLDAPSortKey;
    alias LDAPAPIInfoW LDAPAPIInfo;
    alias PLDAPAPIInfoW PLDAPAPIInfo;
    alias LDAPAPIFeatureInfoW LDAPAPIFeatureInfo;
    alias PLDAPAPIFeatureInfoW PLDAPAPIFeatureInfo;
    alias cldap_openW cldap_open;
    alias ldap_openW ldap_open;
    alias ldap_simple_bindW ldap_simple_bind;
    alias ldap_simple_bind_sW ldap_simple_bind_s;
    alias ldap_sasl_bindW ldap_sasl_bind;
    alias ldap_sasl_bind_sW ldap_sasl_bind_s;
    alias ldap_initW ldap_init;
    alias ldap_sslinitW ldap_sslinit;
    alias ldap_get_optionW ldap_get_option;
    alias ldap_set_optionW ldap_set_option;
    alias ldap_start_tls_sW ldap_start_tls_s;
    alias ldap_addW ldap_add;
    alias ldap_add_extW ldap_add_ext;
    alias ldap_add_sW ldap_add_s;
    alias ldap_add_ext_sW ldap_add_ext_s;
    alias ldap_compareW ldap_compare;
    alias ldap_compare_extW ldap_compare_ext;
    alias ldap_compare_sW ldap_compare_s;
    alias ldap_compare_ext_sW ldap_compare_ext_s;
    alias ldap_deleteW ldap_delete;
    alias ldap_delete_extW ldap_delete_ext;
    alias ldap_delete_sW ldap_delete_s;
    alias ldap_delete_ext_sW ldap_delete_ext_s;
    alias ldap_extended_operation_sW ldap_extended_operation_s;
    alias ldap_extended_operationW ldap_extended_operation;
    alias ldap_modifyW ldap_modify;
    alias ldap_modify_extW ldap_modify_ext;
    alias ldap_modify_sW ldap_modify_s;
    alias ldap_modify_ext_sW ldap_modify_ext_s;
    alias ldap_check_filterW ldap_check_filter;
    alias ldap_count_valuesW ldap_count_values;
    alias ldap_create_page_controlW ldap_create_page_control;
    alias ldap_create_sort_controlW ldap_create_sort_control;
    alias ldap_create_vlv_controlW ldap_create_vlv_control;
    alias ldap_encode_sort_controlW ldap_encode_sort_control;
    alias ldap_escape_filter_elementW ldap_escape_filter_element;
    alias ldap_first_attributeW ldap_first_attribute;
    alias ldap_next_attributeW ldap_next_attribute;
    alias ldap_get_valuesW ldap_get_values;
    alias ldap_get_values_lenW ldap_get_values_len;
    alias ldap_parse_extended_resultW ldap_parse_extended_result;
    alias ldap_parse_page_controlW ldap_parse_page_control;
    alias ldap_parse_referenceW ldap_parse_reference;
    alias ldap_parse_resultW ldap_parse_result;
    alias ldap_parse_sort_controlW ldap_parse_sort_control;
    alias ldap_parse_vlv_controlW ldap_parse_vlv_control;
    alias ldap_searchW ldap_search;
    alias ldap_search_sW ldap_search_s;
    alias ldap_search_stW ldap_search_st;
    alias ldap_search_extW ldap_search_ext;
    alias ldap_search_ext_sW ldap_search_ext_s;
    alias ldap_search_init_pageW ldap_search_init_page;
    alias ldap_err2stringW ldap_err2string;
    alias ldap_control_freeW ldap_control_free;
    alias ldap_controls_freeW ldap_controls_free;
    alias ldap_free_controlsW ldap_free_controls;
    alias ldap_memfreeW ldap_memfree;
    alias ldap_value_freeW ldap_value_free;
    alias ldap_dn2ufnW ldap_dn2ufn;
    alias ldap_ufn2dnW ldap_ufn2dn;
    alias ldap_explode_dnW ldap_explode_dn;
    alias ldap_get_dnW ldap_get_dn;
    alias ldap_rename_extW ldap_rename;
    alias ldap_rename_ext_sW ldap_rename_s;
    alias ldap_rename_extW ldap_rename_ext;
    alias ldap_rename_ext_sW ldap_rename_ext_s;
    deprecated {
        alias ldap_bindW ldap_bind;
        alias ldap_bind_sW ldap_bind_s;
        alias ldap_modrdnW ldap_modrdn;
        alias ldap_modrdn_sW ldap_modrdn_s;
        alias ldap_modrdn2W ldap_modrdn2;
        alias ldap_modrdn2_sW ldap_modrdn2_s;
    }
} else {
    alias LDAPControlA LDAPControl;
    alias PLDAPControlA PLDAPControl;
    alias LDAPModA LDAPMod;
    alias LDAPModA PLDAPMod;
    alias LDAPSortKeyA LDAPSortKey;
    alias PLDAPSortKeyA PLDAPSortKey;
    alias LDAPAPIInfoA LDAPAPIInfo;
    alias PLDAPAPIInfoA PLDAPAPIInfo;
    alias LDAPAPIFeatureInfoA LDAPAPIFeatureInfo;
    alias PLDAPAPIFeatureInfoA PLDAPAPIFeatureInfo;
    alias cldap_openA cldap_open;
    alias ldap_openA ldap_open;
    alias ldap_simple_bindA ldap_simple_bind;
    alias ldap_simple_bind_sA ldap_simple_bind_s;
    alias ldap_sasl_bindA ldap_sasl_bind;
    alias ldap_sasl_bind_sA ldap_sasl_bind_s;
    alias ldap_initA ldap_init;
    alias ldap_sslinitA ldap_sslinit;
    alias ldap_get_optionA ldap_get_option;
    alias ldap_set_optionA ldap_set_option;
    alias ldap_start_tls_sA ldap_start_tls_s;
    alias ldap_addA ldap_add;
    alias ldap_add_extA ldap_add_ext;
    alias ldap_add_sA ldap_add_s;
    alias ldap_add_ext_sA ldap_add_ext_s;
    alias ldap_compareA ldap_compare;
    alias ldap_compare_extA ldap_compare_ext;
    alias ldap_compare_sA ldap_compare_s;
    alias ldap_compare_ext_sA ldap_compare_ext_s;
    alias ldap_deleteA ldap_delete;
    alias ldap_delete_extA ldap_delete_ext;
    alias ldap_delete_sA ldap_delete_s;
    alias ldap_delete_ext_sA ldap_delete_ext_s;
    alias ldap_extended_operation_sA ldap_extended_operation_s;
    alias ldap_extended_operationA ldap_extended_operation;
    alias ldap_modifyA ldap_modify;
    alias ldap_modify_extA ldap_modify_ext;
    alias ldap_modify_sA ldap_modify_s;
    alias ldap_modify_ext_sA ldap_modify_ext_s;
    alias ldap_check_filterA ldap_check_filter;
    alias ldap_count_valuesA ldap_count_values;
    alias ldap_create_page_controlA ldap_create_page_control;
    alias ldap_create_sort_controlA ldap_create_sort_control;
    alias ldap_create_vlv_controlA ldap_create_vlv_control;
    alias ldap_encode_sort_controlA ldap_encode_sort_control;
    alias ldap_escape_filter_elementA ldap_escape_filter_element;
    alias ldap_first_attributeA ldap_first_attribute;
    alias ldap_next_attributeA ldap_next_attribute;
    alias ldap_get_valuesA ldap_get_values;
    alias ldap_get_values_lenA ldap_get_values_len;
    alias ldap_parse_extended_resultA ldap_parse_extended_result;
    alias ldap_parse_page_controlA ldap_parse_page_control;
    alias ldap_parse_referenceA ldap_parse_reference;
    alias ldap_parse_resultA ldap_parse_result;
    alias ldap_parse_sort_controlA ldap_parse_sort_control;
    alias ldap_parse_vlv_controlA ldap_parse_vlv_control;
    alias ldap_searchA ldap_search;
    alias ldap_search_sA ldap_search_s;
    alias ldap_search_stA ldap_search_st;
    alias ldap_search_extA ldap_search_ext;
    alias ldap_search_ext_sA ldap_search_ext_s;
    alias ldap_search_init_pageA ldap_search_init_page;
    alias ldap_err2stringA ldap_err2string;
    alias ldap_control_freeA ldap_control_free;
    alias ldap_controls_freeA ldap_controls_free;
    alias ldap_free_controlsA ldap_free_controls;
    alias ldap_memfreeA ldap_memfree;
    alias ldap_value_freeA ldap_value_free;
    alias ldap_dn2ufnA ldap_dn2ufn;
    alias ldap_ufn2dnA ldap_ufn2dn;
    alias ldap_explode_dnA ldap_explode_dn;
    alias ldap_get_dnA ldap_get_dn;
    alias ldap_rename_extA ldap_rename;
    alias ldap_rename_ext_sA ldap_rename_s;
    alias ldap_rename_extA ldap_rename_ext;
    alias ldap_rename_ext_sA ldap_rename_ext_s;
    deprecated {
        alias ldap_bindA ldap_bind;
        alias ldap_bind_sA ldap_bind_s;
        alias ldap_modrdnA ldap_modrdn;
        alias ldap_modrdn_sA ldap_modrdn_s;
        alias ldap_modrdn2A ldap_modrdn2;
        alias ldap_modrdn2_sA ldap_modrdn2_s;
    }
}

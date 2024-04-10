/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_ntsecapi.d)
 */
module core.sys.windows.ntsecapi;
version (Windows):
pragma(lib, "advapi32");

version (ANSI) {} else version = Unicode;

private import
  core.sys.windows.basetyps, core.sys.windows.ntdef, core.sys.windows.windef, core.sys.windows.winnt, core.sys.windows.w32api;

// FIXME: check types and grouping of constants
// FIXME: check Windows version support

enum KERB_WRAP_NO_ENCRYPT        = 0x80000001;

enum LOGON_GUEST                 = 0x00000001;
enum LOGON_NOENCRYPTION          = 0x00000002;
enum LOGON_CACHED_ACCOUNT        = 0x00000004;
enum LOGON_USED_LM_PASSWORD      = 0x00000008;
enum LOGON_EXTRA_SIDS            = 0x00000020;
enum LOGON_SUBAUTH_SESSION_KEY   = 0x00000040;
enum LOGON_SERVER_TRUST_ACCOUNT  = 0x00000080;
enum LOGON_NTLMV2_ENABLED        = 0x00000100;
enum LOGON_RESOURCE_GROUPS       = 0x00000200;
enum LOGON_PROFILE_PATH_RETURNED = 0x00000400;
enum LOGON_GRACE_LOGON           = 0x01000000;

enum {
    LSA_MODE_PASSWORD_PROTECTED = 1,
    LSA_MODE_INDIVIDUAL_ACCOUNTS,
    LSA_MODE_MANDATORY_ACCESS,
    LSA_MODE_LOG_FULL
}

bool LSA_SUCCESS(int x) { return x >= 0; }

/*  TOTHINKABOUT: These constants don't have ANSI/Unicode versioned
 *  aliases.  Should we merge them anyway?
 */
const char[]  MICROSOFT_KERBEROS_NAME_A = "Kerberos";
const wchar[] MICROSOFT_KERBEROS_NAME_W = "Kerberos";
const char[]  MSV1_0_PACKAGE_NAME  = "MICROSOFT_AUTHENTICATION_PACKAGE_V1_0";
const wchar[] MSV1_0_PACKAGE_NAMEW = "MICROSOFT_AUTHENTICATION_PACKAGE_V1_0";

enum MSV1_0_ALLOW_SERVER_TRUST_ACCOUNT      =       32;
enum MSV1_0_ALLOW_WORKSTATION_TRUST_ACCOUNT =     2048;
enum MSV1_0_CLEARTEXT_PASSWORD_ALLOWED      =        2;
enum MSV1_0_CRED_LM_PRESENT                 =        1;
enum MSV1_0_CRED_NT_PRESENT                 =        2;
enum MSV1_0_CRED_VERSION                    =        0;
enum MSV1_0_DONT_TRY_GUEST_ACCOUNT          =       16;
enum MSV1_0_MAX_NTLM3_LIFE                  =     1800;
enum MSV1_0_MAX_AVL_SIZE                    =    64000;
enum MSV1_0_MNS_LOGON                       = 16777216;

enum size_t
    MSV1_0_CHALLENGE_LENGTH          = 8,
    MSV1_0_LANMAN_SESSION_KEY_LENGTH = 8,
    MSV1_0_NTLM3_RESPONSE_LENGTH     = 16,
    MSV1_0_NTLM3_OWF_LENGTH          = 16,
    MSV1_0_NTLM3_INPUT_LENGTH        = MSV1_0_NTLM3_RESPONSE.sizeof
                                       - MSV1_0_NTLM3_RESPONSE_LENGTH,
    MSV1_0_OWF_PASSWORD_LENGTH       = 16,
    MSV1_0_PACKAGE_NAMEW_LENGTH      = MSV1_0_PACKAGE_NAMEW.sizeof
                                       - WCHAR.sizeof;

enum MSV1_0_RETURN_USER_PARAMETERS      =          8;
enum MSV1_0_RETURN_PASSWORD_EXPIRY      =         64;
enum MSV1_0_RETURN_PROFILE_PATH         =        512;
enum MSV1_0_SUBAUTHENTICATION_DLL_EX    =    1048576;
enum MSV1_0_SUBAUTHENTICATION_DLL       = 0xff000000;
enum MSV1_0_SUBAUTHENTICATION_DLL_SHIFT =         24;
enum MSV1_0_SUBAUTHENTICATION_DLL_RAS   =          2;
enum MSV1_0_SUBAUTHENTICATION_DLL_IIS   =        132;
enum MSV1_0_SUBAUTHENTICATION_FLAGS     = 0xff000000;
enum MSV1_0_TRY_GUEST_ACCOUNT_ONLY      =        256;
enum MSV1_0_TRY_SPECIFIED_DOMAIN_ONLY   =       1024;
enum MSV1_0_UPDATE_LOGON_STATISTICS     =          4;
enum MSV1_0_USE_CLIENT_CHALLENGE        =        128;
enum MSV1_0_USER_SESSION_KEY_LENGTH     =         16;

const char[]
    MSV1_0_SUBAUTHENTICATION_KEY
      = `System\CurrentControlSet\Control\Lsa\MSV1_0`,
    MSV1_0_SUBAUTHENTICATION_VALUE = "Auth";

enum ACCESS_MASK
    POLICY_VIEW_LOCAL_INFORMATION   = 0x0001,
    POLICY_VIEW_AUDIT_INFORMATION   = 0x0002,
    POLICY_GET_PRIVATE_INFORMATION  = 0x0004,
    POLICY_TRUST_ADMIN              = 0x0008,
    POLICY_CREATE_ACCOUNT           = 0x0010,
    POLICY_CREATE_SECRET            = 0x0020,
    POLICY_CREATE_PRIVILEGE         = 0x0040,
    POLICY_SET_DEFAULT_QUOTA_LIMITS = 0x0080,
    POLICY_SET_AUDIT_REQUIREMENTS   = 0x0100,
    POLICY_AUDIT_LOG_ADMIN          = 0x0200,
    POLICY_SERVER_ADMIN             = 0x0400,
    POLICY_LOOKUP_NAMES             = 0x0800,

    POLICY_READ                     = STANDARD_RIGHTS_READ     | 0x0006,
    POLICY_WRITE                    = STANDARD_RIGHTS_WRITE    | 0x07F8,
    POLICY_EXECUTE                  = STANDARD_RIGHTS_EXECUTE  | 0x0801,
    POLICY_ALL_ACCESS               = STANDARD_RIGHTS_REQUIRED | 0x0FFF;

enum POLICY_AUDIT_EVENT_UNCHANGED = 0;
enum POLICY_AUDIT_EVENT_SUCCESS   = 1;
enum POLICY_AUDIT_EVENT_FAILURE   = 2;
enum POLICY_AUDIT_EVENT_NONE      = 4;
enum POLICY_AUDIT_EVENT_MASK      = 7;

enum {
    POLICY_LOCATION_LOCAL = 1,
    POLICY_LOCATION_DS
}

enum : uint {
    POLICY_MACHINE_POLICY_LOCAL     =          0,
    POLICY_MACHINE_POLICY_DEFAULTED,
    POLICY_MACHINE_POLICY_EXPLICIT,
    POLICY_MACHINE_POLICY_UNKNOWN   = 0xFFFFFFFF
}


enum POLICY_QOS_SCHANEL_REQUIRED            = 0x0001;
enum POLICY_QOS_OUTBOUND_INTEGRITY          = 0x0002;
enum POLICY_QOS_OUTBOUND_CONFIDENTIALITY    = 0x0004;
enum POLICY_QOS_INBOUND_INTEGREITY          = 0x0008;
enum POLICY_QOS_INBOUND_CONFIDENTIALITY     = 0x0010;
enum POLICY_QOS_ALLOW_LOCAL_ROOT_CERT_STORE = 0x0020;
enum POLICY_QOS_RAS_SERVER_ALLOWED          = 0x0040;
enum POLICY_QOS_DHCP_SERVER_ALLOWD          = 0x0080;

enum POLICY_KERBEROS_FORWARDABLE  = 1;
enum POLICY_KERBEROS_PROXYABLE    = 2;
enum POLICY_KERBEROS_RENEWABLE    = 4;
enum POLICY_KERBEROS_POSTDATEABLE = 8;

const char[]
    SAM_PASSWORD_CHANGE_NOTIFY_ROUTINE = "PasswordChangeNotify",
    SAM_INIT_NOTIFICATION_ROUTINE      = "InitializeChangeNotify",
    SAM_PASSWORD_FILTER_ROUTINE        = "PasswordFilter";

const TCHAR[]
    SE_INTERACTIVE_LOGON_NAME          = "SeInteractiveLogonRight",
    SE_NETWORK_LOGON_NAME              = "SeNetworkLogonRight",
    SE_BATCH_LOGON_NAME                = "SeBatchLogonRight",
    SE_SERVICE_LOGON_NAME              = "SeServiceLogonRight";

enum {
    TRUST_ATTRIBUTE_NON_TRANSITIVE =         1,
    TRUST_ATTRIBUTE_UPLEVEL_ONLY   =         2,
    TRUST_ATTRIBUTE_TREE_PARENT    =   4194304,
    TRUST_ATTRIBUTES_VALID         = -16580609
}

enum {
    TRUST_AUTH_TYPE_NONE,
    TRUST_AUTH_TYPE_NT4OWF,
    TRUST_AUTH_TYPE_CLEAR
}

enum {
    TRUST_DIRECTION_DISABLED,
    TRUST_DIRECTION_INBOUND,
    TRUST_DIRECTION_OUTBOUND,
    TRUST_DIRECTION_BIDIRECTIONAL
}

enum {
    TRUST_TYPE_DOWNLEVEL = 1,
    TRUST_TYPE_UPLEVEL,
    TRUST_TYPE_MIT,
    TRUST_TYPE_DCE
}

alias UNICODE_STRING LSA_UNICODE_STRING;
alias UNICODE_STRING* PLSA_UNICODE_STRING;
alias STRING LSA_STRING;
alias STRING* PLSA_STRING;

enum MSV1_0_LOGON_SUBMIT_TYPE {
    MsV1_0InteractiveLogon       = 2,
    MsV1_0Lm20Logon,
    MsV1_0NetworkLogon,
    MsV1_0SubAuthLogon,
    MsV1_0WorkstationUnlockLogon = 7
}
alias MSV1_0_LOGON_SUBMIT_TYPE* PMSV1_0_LOGON_SUBMIT_TYPE;

enum MSV1_0_PROFILE_BUFFER_TYPE {
    MsV1_0InteractiveProfile = 2,
    MsV1_0Lm20LogonProfile,
    MsV1_0SmartCardProfile
}
alias MSV1_0_PROFILE_BUFFER_TYPE* PMSV1_0_PROFILE_BUFFER_TYPE;


enum MSV1_0_AVID {
    MsvAvEOL,
    MsvAvNbComputerName,
    MsvAvNbDomainName,
    MsvAvDnsComputerName,
    MsvAvDnsDomainName
}

enum MSV1_0_PROTOCOL_MESSAGE_TYPE {
    MsV1_0Lm20ChallengeRequest = 0,
    MsV1_0Lm20GetChallengeResponse,
    MsV1_0EnumerateUsers,
    MsV1_0GetUserInfo,
    MsV1_0ReLogonUsers,
    MsV1_0ChangePassword,
    MsV1_0ChangeCachedPassword,
    MsV1_0GenericPassthrough,
    MsV1_0CacheLogon,
    MsV1_0SubAuth,
    MsV1_0DeriveCredential,
    MsV1_0CacheLookup
}
alias MSV1_0_PROTOCOL_MESSAGE_TYPE* PMSV1_0_PROTOCOL_MESSAGE_TYPE;

enum POLICY_LSA_SERVER_ROLE {
    PolicyServerRoleBackup = 2,
    PolicyServerRolePrimary
}
alias POLICY_LSA_SERVER_ROLE* PPOLICY_LSA_SERVER_ROLE;

enum POLICY_SERVER_ENABLE_STATE {
    PolicyServerEnabled = 2,
    PolicyServerDisabled
}
alias POLICY_SERVER_ENABLE_STATE* PPOLICY_SERVER_ENABLE_STATE;

enum POLICY_INFORMATION_CLASS {
    PolicyAuditLogInformation = 1,
    PolicyAuditEventsInformation,
    PolicyPrimaryDomainInformation,
    PolicyPdAccountInformation,
    PolicyAccountDomainInformation,
    PolicyLsaServerRoleInformation,
    PolicyReplicaSourceInformation,
    PolicyDefaultQuotaInformation,
    PolicyModificationInformation,
    PolicyAuditFullSetInformation,
    PolicyAuditFullQueryInformation,
    PolicyDnsDomainInformation,
    PolicyEfsInformation
}
alias POLICY_INFORMATION_CLASS* PPOLICY_INFORMATION_CLASS;

enum POLICY_AUDIT_EVENT_TYPE {
    AuditCategorySystem,
    AuditCategoryLogon,
    AuditCategoryObjectAccess,
    AuditCategoryPrivilegeUse,
    AuditCategoryDetailedTracking,
    AuditCategoryPolicyChange,
    AuditCategoryAccountManagement,
    AuditCategoryDirectoryServiceAccess,
    AuditCategoryAccountLogon
}
alias POLICY_AUDIT_EVENT_TYPE* PPOLICY_AUDIT_EVENT_TYPE;

enum POLICY_LOCAL_INFORMATION_CLASS {
    PolicyLocalAuditEventsInformation = 1,
    PolicyLocalPdAccountInformation,
    PolicyLocalAccountDomainInformation,
    PolicyLocalLsaServerRoleInformation,
    PolicyLocalReplicaSourceInformation,
    PolicyLocalModificationInformation,
    PolicyLocalAuditFullSetInformation,
    PolicyLocalAuditFullQueryInformation,
    PolicyLocalDnsDomainInformation,
    PolicyLocalIPSecReferenceInformation,
    PolicyLocalMachinePasswordInformation,
    PolicyLocalQualityOfServiceInformation,
    PolicyLocalPolicyLocationInformation
}
alias POLICY_LOCAL_INFORMATION_CLASS* PPOLICY_LOCAL_INFORMATION_CLASS;

enum POLICY_DOMAIN_INFORMATION_CLASS {
    PolicyDomainIPSecReferenceInformation = 1,
    PolicyDomainQualityOfServiceInformation,
    PolicyDomainEfsInformation,
    PolicyDomainPublicKeyInformation,
    PolicyDomainPasswordPolicyInformation,
    PolicyDomainLockoutInformation,
    PolicyDomainKerberosTicketInformation
}
alias POLICY_DOMAIN_INFORMATION_CLASS* PPOLICY_DOMAIN_INFORMATION_CLASS;

enum SECURITY_LOGON_TYPE {
    Interactive = 2,
    Network,
    Batch,
    Service,
    Proxy,
    Unlock
}
alias SECURITY_LOGON_TYPE* PSECURITY_LOGON_TYPE;

enum TRUSTED_INFORMATION_CLASS {
    TrustedDomainNameInformation = 1,
    TrustedControllersInformation,
    TrustedPosixOffsetInformation,
    TrustedPasswordInformation,
    TrustedDomainInformationBasic,
    TrustedDomainInformationEx,
    TrustedDomainAuthInformation,
    TrustedDomainFullInformation
}
alias TRUSTED_INFORMATION_CLASS* PTRUSTED_INFORMATION_CLASS;

struct DOMAIN_PASSWORD_INFORMATION {
    USHORT        MinPasswordLength;
    USHORT        PasswordHistoryLength;
    ULONG         PasswordProperties;
    LARGE_INTEGER MaxPasswordAge;
    LARGE_INTEGER MinPasswordAge;
}
alias DOMAIN_PASSWORD_INFORMATION* PDOMAIN_PASSWORD_INFORMATION;

struct LSA_ENUMERATION_INFORMATION {
    PSID Sid;
}
alias LSA_ENUMERATION_INFORMATION* PLSA_ENUMERATION_INFORMATION;

alias OBJECT_ATTRIBUTES LSA_OBJECT_ATTRIBUTES;
alias OBJECT_ATTRIBUTES* PLSA_OBJECT_ATTRIBUTES;

struct LSA_TRUST_INFORMATION {
    LSA_UNICODE_STRING Name;
    PSID               Sid;
}
alias LSA_TRUST_INFORMATION TRUSTED_DOMAIN_INFORMATION_BASIC;
alias LSA_TRUST_INFORMATION* PLSA_TRUST_INFORMATION;
/*  in MinGW (further down the code):
 *      typedef PLSA_TRUST_INFORMATION *PTRUSTED_DOMAIN_INFORMATION_BASIC;
 *  but it doesn't look right....
 */
alias LSA_TRUST_INFORMATION** PTRUSTED_DOMAIN_INFORMATION_BASIC;

struct LSA_REFERENCED_DOMAIN_LIST {
    ULONG                  Entries;
    PLSA_TRUST_INFORMATION Domains;
}
alias LSA_REFERENCED_DOMAIN_LIST* PLSA_REFERENCED_DOMAIN_LIST;

struct LSA_TRANSLATED_SID {
    SID_NAME_USE Use;
    ULONG        RelativeId;
    LONG         DomainIndex;
}
alias LSA_TRANSLATED_SID* PLSA_TRANSLATED_SID;

struct LSA_TRANSLATED_NAME {
    SID_NAME_USE       Use;
    LSA_UNICODE_STRING Name;
    LONG               DomainIndex;
}
alias LSA_TRANSLATED_NAME* PLSA_TRANSLATED_NAME;

struct MSV1_0_INTERACTIVE_LOGON {
    MSV1_0_LOGON_SUBMIT_TYPE MessageType;
    UNICODE_STRING           LogonDomainName;
    UNICODE_STRING           UserName;
    UNICODE_STRING           Password;
}
alias MSV1_0_INTERACTIVE_LOGON* PMSV1_0_INTERACTIVE_LOGON;

struct MSV1_0_INTERACTIVE_PROFILE {
    MSV1_0_PROFILE_BUFFER_TYPE MessageType;
    USHORT                     LogonCount;
    USHORT                     BadPasswordCount;
    LARGE_INTEGER              LogonTime;
    LARGE_INTEGER              LogoffTime;
    LARGE_INTEGER              KickOffTime;
    LARGE_INTEGER              PasswordLastSet;
    LARGE_INTEGER              PasswordCanChange;
    LARGE_INTEGER              PasswordMustChange;
    UNICODE_STRING             LogonScript;
    UNICODE_STRING             HomeDirectory;
    UNICODE_STRING             FullName;
    UNICODE_STRING             ProfilePath;
    UNICODE_STRING             HomeDirectoryDrive;
    UNICODE_STRING             LogonServer;
    ULONG                      UserFlags;
}
alias MSV1_0_INTERACTIVE_PROFILE* PMSV1_0_INTERACTIVE_PROFILE;

struct MSV1_0_LM20_LOGON {
    MSV1_0_LOGON_SUBMIT_TYPE       MessageType;
    UNICODE_STRING                 LogonDomainName;
    UNICODE_STRING                 UserName;
    UNICODE_STRING                 Workstation;
    UCHAR[MSV1_0_CHALLENGE_LENGTH] ChallengeToClient;
    STRING                         CaseSensitiveChallengeResponse;
    STRING                         CaseInsensitiveChallengeResponse;
    ULONG                          ParameterControl;
}
alias MSV1_0_LM20_LOGON* PMSV1_0_LM20_LOGON;

//static if (_WIN32_WINNT >= 0x500) {
    struct MSV1_0_SUBAUTH_LOGON {
        MSV1_0_LOGON_SUBMIT_TYPE       MessageType;
        UNICODE_STRING                 LogonDomainName;
        UNICODE_STRING                 UserName;
        UNICODE_STRING                 Workstation;
        UCHAR[MSV1_0_CHALLENGE_LENGTH] ChallengeToClient;
        STRING                         AuthenticationInfo1;
        STRING                         AuthenticationInfo2;
        ULONG                          ParameterControl;
        ULONG                          SubAuthPackageId;
    }
    alias MSV1_0_SUBAUTH_LOGON* PMSV1_0_SUBAUTH_LOGON;
//}

struct MSV1_0_LM20_LOGON_PROFILE {
    MSV1_0_PROFILE_BUFFER_TYPE              MessageType;
    LARGE_INTEGER                           KickOffTime;
    LARGE_INTEGER                           LogoffTime;
    ULONG                                   UserFlags;
    UCHAR[MSV1_0_USER_SESSION_KEY_LENGTH]   UserSessionKey;
    UNICODE_STRING                          LogonDomainName;
    UCHAR[MSV1_0_LANMAN_SESSION_KEY_LENGTH] LanmanSessionKey;
    UNICODE_STRING                          LogonServer;
    UNICODE_STRING                          UserParameters;
}
alias MSV1_0_LM20_LOGON_PROFILE* PMSV1_0_LM20_LOGON_PROFILE;

struct MSV1_0_SUPPLEMENTAL_CREDENTIAL {
    ULONG Version;
    ULONG Flags;
    UCHAR[MSV1_0_OWF_PASSWORD_LENGTH] LmPassword;
    UCHAR[MSV1_0_OWF_PASSWORD_LENGTH] NtPassword;
}
alias MSV1_0_SUPPLEMENTAL_CREDENTIAL* PMSV1_0_SUPPLEMENTAL_CREDENTIAL;

struct MSV1_0_NTLM3_RESPONSE {
    UCHAR[MSV1_0_NTLM3_RESPONSE_LENGTH] Response;
    UCHAR     RespType;
    UCHAR     HiRespType;
    USHORT    Flags;
    ULONG     MsgWord;
    ULONGLONG TimeStamp;
    UCHAR[MSV1_0_CHALLENGE_LENGTH]      ChallengeFromClient;
    ULONG     AvPairsOff;
    UCHAR     _Buffer;
    UCHAR*    Buffer() return { return &_Buffer; }
}
alias MSV1_0_NTLM3_RESPONSE* PMSV1_0_NTLM3_RESPONSE;

struct  MSV1_0_AV_PAIR {
    USHORT AvId;
    USHORT AvLen;
}
alias MSV1_0_AV_PAIR* PMSV1_0_AV_PAIR;

struct MSV1_0_CHANGEPASSWORD_REQUEST {
    MSV1_0_PROTOCOL_MESSAGE_TYPE MessageType;
    UNICODE_STRING DomainName;
    UNICODE_STRING AccountName;
    UNICODE_STRING OldPassword;
    UNICODE_STRING NewPassword;
    BOOLEAN        Impersonating;
}
alias MSV1_0_CHANGEPASSWORD_REQUEST* PMSV1_0_CHANGEPASSWORD_REQUEST;

struct MSV1_0_CHANGEPASSWORD_RESPONSE {
    MSV1_0_PROTOCOL_MESSAGE_TYPE MessageType;
    BOOLEAN                      PasswordInfoValid;
    DOMAIN_PASSWORD_INFORMATION  DomainPasswordInfo;
}
alias MSV1_0_CHANGEPASSWORD_RESPONSE* PMSV1_0_CHANGEPASSWORD_RESPONSE;

struct MSV1_0_SUBAUTH_REQUEST {
    MSV1_0_PROTOCOL_MESSAGE_TYPE MessageType;
    ULONG  SubAuthPackageId;
    ULONG  SubAuthInfoLength;
    PUCHAR SubAuthSubmitBuffer;
}
alias MSV1_0_SUBAUTH_REQUEST* PMSV1_0_SUBAUTH_REQUEST;

struct MSV1_0_SUBAUTH_RESPONSE {
    MSV1_0_PROTOCOL_MESSAGE_TYPE MessageType;
    ULONG  SubAuthInfoLength;
    PUCHAR SubAuthReturnBuffer;
}
alias MSV1_0_SUBAUTH_RESPONSE* PMSV1_0_SUBAUTH_RESPONSE;

enum MSV1_0_DERIVECRED_TYPE_SHA1 = 0;

struct MSV1_0_DERIVECRED_REQUEST {
    MSV1_0_PROTOCOL_MESSAGE_TYPE MessageType;
    LUID   LogonId;
    ULONG  DeriveCredType;
    ULONG  DeriveCredInfoLength;
    UCHAR  _DeriveCredSubmitBuffer;
    UCHAR* DeriveCredSubmitBuffer() return { return &_DeriveCredSubmitBuffer; }
}
alias MSV1_0_DERIVECRED_REQUEST* PMSV1_0_DERIVECRED_REQUEST;

struct MSV1_0_DERIVECRED_RESPONSE {
    MSV1_0_PROTOCOL_MESSAGE_TYPE MessageType;
    ULONG  DeriveCredInfoLength;
    UCHAR  _DeriveCredReturnBuffer;
    UCHAR* DeriveCredReturnBuffer() return { return &_DeriveCredReturnBuffer; }
}
alias MSV1_0_DERIVECRED_RESPONSE* PMSV1_0_DERIVECRED_RESPONSE;

alias uint LSA_ENUMERATION_HANDLE, LSA_OPERATIONAL_MODE,
  POLICY_AUDIT_EVENT_OPTIONS;
alias uint* PLSA_ENUMERATION_HANDLE, PLSA_OPERATIONAL_MODE,
  PPOLICY_AUDIT_EVENT_OPTIONS;

struct POLICY_PRIVILEGE_DEFINITION {
    LSA_UNICODE_STRING Name;
    LUID LocalValue;
}
alias POLICY_PRIVILEGE_DEFINITION* PPOLICY_PRIVILEGE_DEFINITION;

struct POLICY_AUDIT_LOG_INFO {
    ULONG         AuditLogPercentFull;
    ULONG         MaximumLogSize;
    LARGE_INTEGER AuditRetentionPeriod;
    BOOLEAN       AuditLogFullShutdownInProgress;
    LARGE_INTEGER TimeToShutdown;
    ULONG         NextAuditRecordId;
}
alias POLICY_AUDIT_LOG_INFO* PPOLICY_AUDIT_LOG_INFO;

struct POLICY_AUDIT_EVENTS_INFO {
    BOOLEAN                     AuditingMode;
    PPOLICY_AUDIT_EVENT_OPTIONS EventAuditingOptions;
    ULONG                       MaximumAuditEventCount;
}
alias POLICY_AUDIT_EVENTS_INFO* PPOLICY_AUDIT_EVENTS_INFO;

struct POLICY_ACCOUNT_DOMAIN_INFO {
    LSA_UNICODE_STRING DomainName;
    PSID               DomainSid;
}
alias POLICY_ACCOUNT_DOMAIN_INFO* PPOLICY_ACCOUNT_DOMAIN_INFO;

struct POLICY_PRIMARY_DOMAIN_INFO {
    LSA_UNICODE_STRING Name;
    PSID               Sid;
}
alias POLICY_PRIMARY_DOMAIN_INFO* PPOLICY_PRIMARY_DOMAIN_INFO;

struct POLICY_DNS_DOMAIN_INFO {
    LSA_UNICODE_STRING Name;
    LSA_UNICODE_STRING DnsDomainName;
    LSA_UNICODE_STRING DnsTreeName;
    GUID               DomainGuid;
    PSID               Sid;
}
alias POLICY_DNS_DOMAIN_INFO* PPOLICY_DNS_DOMAIN_INFO;

struct POLICY_PD_ACCOUNT_INFO {
    LSA_UNICODE_STRING Name;
}
alias POLICY_PD_ACCOUNT_INFO* PPOLICY_PD_ACCOUNT_INFO;

struct POLICY_LSA_SERVER_ROLE_INFO {
    POLICY_LSA_SERVER_ROLE LsaServerRole;
}
alias POLICY_LSA_SERVER_ROLE_INFO* PPOLICY_LSA_SERVER_ROLE_INFO;

struct POLICY_REPLICA_SOURCE_INFO {
    LSA_UNICODE_STRING ReplicaSource;
    LSA_UNICODE_STRING ReplicaAccountName;
}
alias POLICY_REPLICA_SOURCE_INFO* PPOLICY_REPLICA_SOURCE_INFO;

struct POLICY_DEFAULT_QUOTA_INFO {
    QUOTA_LIMITS QuotaLimits;
}
alias POLICY_DEFAULT_QUOTA_INFO* PPOLICY_DEFAULT_QUOTA_INFO;

struct POLICY_MODIFICATION_INFO {
    LARGE_INTEGER ModifiedId;
    LARGE_INTEGER DatabaseCreationTime;
}
alias POLICY_MODIFICATION_INFO* PPOLICY_MODIFICATION_INFO;

struct POLICY_AUDIT_FULL_SET_INFO {
    BOOLEAN ShutDownOnFull;
}
alias POLICY_AUDIT_FULL_SET_INFO* PPOLICY_AUDIT_FULL_SET_INFO;

struct POLICY_AUDIT_FULL_QUERY_INFO {
    BOOLEAN ShutDownOnFull;
    BOOLEAN LogIsFull;
}
alias POLICY_AUDIT_FULL_QUERY_INFO* PPOLICY_AUDIT_FULL_QUERY_INFO;

struct POLICY_EFS_INFO {
    ULONG InfoLength;
    PUCHAR EfsBlob;
}
alias POLICY_EFS_INFO* PPOLICY_EFS_INFO;

struct POLICY_LOCAL_IPSEC_REFERENCE_INFO {
    LSA_UNICODE_STRING ObjectPath;
}
alias POLICY_LOCAL_IPSEC_REFERENCE_INFO* PPOLICY_LOCAL_IPSEC_REFERENCE_INFO;

struct POLICY_LOCAL_MACHINE_PASSWORD_INFO {
    LARGE_INTEGER PasswordChangeInterval;
}
alias POLICY_LOCAL_MACHINE_PASSWORD_INFO* PPOLICY_LOCAL_MACHINE_PASSWORD_INFO;

struct POLICY_LOCAL_POLICY_LOCATION_INFO {
    ULONG PolicyLocation;
}
alias POLICY_LOCAL_POLICY_LOCATION_INFO* PPOLICY_LOCAL_POLICY_LOCATION_INFO;

struct POLICY_LOCAL_QUALITY_OF_SERVICE_INFO{
    ULONG QualityOfService;
}
alias POLICY_LOCAL_QUALITY_OF_SERVICE_INFO
  POLICY_DOMAIN_QUALITY_OF_SERVICE_INFO;
alias POLICY_LOCAL_QUALITY_OF_SERVICE_INFO*
  PPOLICY_LOCAL_QUALITY_OF_SERVICE_INFO,
  PPOLICY_DOMAIN_QUALITY_OF_SERVICE_INFO;

struct POLICY_DOMAIN_PUBLIC_KEY_INFO {
    ULONG  InfoLength;
    PUCHAR PublicKeyInfo;
}
alias POLICY_DOMAIN_PUBLIC_KEY_INFO* PPOLICY_DOMAIN_PUBLIC_KEY_INFO;

struct POLICY_DOMAIN_LOCKOUT_INFO {
    LARGE_INTEGER LockoutDuration;
    LARGE_INTEGER LockoutObservationWindow;
    USHORT        LockoutThreshold;
}
alias POLICY_DOMAIN_LOCKOUT_INFO* PPOLICY_DOMAIN_LOCKOUT_INFO;

struct POLICY_DOMAIN_PASSWORD_INFO {
    USHORT        MinPasswordLength;
    USHORT        PasswordHistoryLength;
    ULONG         PasswordProperties;
    LARGE_INTEGER MaxPasswordAge;
    LARGE_INTEGER MinPasswordAge;
}
alias POLICY_DOMAIN_PASSWORD_INFO* PPOLICY_DOMAIN_PASSWORD_INFO;

struct POLICY_DOMAIN_KERBEROS_TICKET_INFO {
    ULONG         AuthenticationOptions;
    LARGE_INTEGER MinTicketAge;
    LARGE_INTEGER MaxTicketAge;
    LARGE_INTEGER MaxRenewAge;
    LARGE_INTEGER ProxyLifetime;
    LARGE_INTEGER ForceLogoff;
}
alias POLICY_DOMAIN_KERBEROS_TICKET_INFO* PPOLICY_DOMAIN_KERBEROS_TICKET_INFO;

alias LSA_HANDLE = HANDLE;
alias LSA_HANDLE* PLSA_HANDLE;

struct TRUSTED_DOMAIN_NAME_INFO {
    LSA_UNICODE_STRING Name;
}
alias TRUSTED_DOMAIN_NAME_INFO* PTRUSTED_DOMAIN_NAME_INFO;

struct TRUSTED_CONTROLLERS_INFO {
    ULONG               Entries;
    PLSA_UNICODE_STRING Names;
}
alias TRUSTED_CONTROLLERS_INFO* PTRUSTED_CONTROLLERS_INFO;

struct TRUSTED_POSIX_OFFSET_INFO {
    ULONG Offset;
}
alias TRUSTED_POSIX_OFFSET_INFO* PTRUSTED_POSIX_OFFSET_INFO;

struct TRUSTED_PASSWORD_INFO {
    LSA_UNICODE_STRING Password;
    LSA_UNICODE_STRING OldPassword;
}
alias TRUSTED_PASSWORD_INFO* PTRUSTED_PASSWORD_INFO;

struct TRUSTED_DOMAIN_INFORMATION_EX {
    LSA_UNICODE_STRING Name;
    LSA_UNICODE_STRING FlatName;
    PSID               Sid;
    ULONG              TrustDirection;
    ULONG              TrustType;
    ULONG              TrustAttributes;
}
alias TRUSTED_DOMAIN_INFORMATION_EX* PTRUSTED_DOMAIN_INFORMATION_EX;

struct LSA_AUTH_INFORMATION {
    LARGE_INTEGER LastUpdateTime;
    ULONG         AuthType;
    ULONG         AuthInfoLength;
    PUCHAR        AuthInfo;
}
alias LSA_AUTH_INFORMATION* PLSA_AUTH_INFORMATION;

struct TRUSTED_DOMAIN_AUTH_INFORMATION {
    ULONG                 IncomingAuthInfos;
    PLSA_AUTH_INFORMATION IncomingAuthenticationInformation;
    PLSA_AUTH_INFORMATION IncomingPreviousAuthenticationInformation;
    ULONG                 OutgoingAuthInfos;
    PLSA_AUTH_INFORMATION OutgoingAuthenticationInformation;
    PLSA_AUTH_INFORMATION OutgoingPreviousAuthenticationInformation;
}
alias TRUSTED_DOMAIN_AUTH_INFORMATION* PTRUSTED_DOMAIN_AUTH_INFORMATION;

struct TRUSTED_DOMAIN_FULL_INFORMATION {
    TRUSTED_DOMAIN_INFORMATION_EX   Information;
    TRUSTED_POSIX_OFFSET_INFO       PosixOffset;
    TRUSTED_DOMAIN_AUTH_INFORMATION AuthInformation;
}
alias TRUSTED_DOMAIN_FULL_INFORMATION* PTRUSTED_DOMAIN_FULL_INFORMATION;

extern (Windows) {
    NTSTATUS LsaAddAccountRights(LSA_HANDLE, PSID, PLSA_UNICODE_STRING,
      ULONG);
    NTSTATUS LsaCallAuthenticationPackage(HANDLE, ULONG, PVOID, ULONG,
      PVOID*, PULONG, PNTSTATUS);
    NTSTATUS LsaClose(LSA_HANDLE);
    NTSTATUS LsaConnectUntrusted(PHANDLE);
    NTSTATUS LsaCreateTrustedDomainEx(LSA_HANDLE,
      PTRUSTED_DOMAIN_INFORMATION_EX, PTRUSTED_DOMAIN_AUTH_INFORMATION,
      ACCESS_MASK, PLSA_HANDLE);
    NTSTATUS LsaDeleteTrustedDomain(LSA_HANDLE, PSID);
    NTSTATUS LsaDeregisterLogonProcess(HANDLE);
    NTSTATUS LsaEnumerateAccountRights(LSA_HANDLE, PSID, PLSA_UNICODE_STRING*,
      PULONG);
    NTSTATUS LsaEnumerateAccountsWithUserRight(LSA_HANDLE,
      PLSA_UNICODE_STRING, PVOID*, PULONG);
    NTSTATUS LsaEnumerateTrustedDomains(LSA_HANDLE, PLSA_ENUMERATION_HANDLE,
      PVOID*, ULONG, PULONG);
    NTSTATUS LsaEnumerateTrustedDomainsEx(LSA_HANDLE, PLSA_ENUMERATION_HANDLE,
      TRUSTED_INFORMATION_CLASS, PVOID*, ULONG, PULONG);
    NTSTATUS LsaFreeMemory(PVOID);
    NTSTATUS LsaFreeReturnBuffer(PVOID);
    NTSTATUS LsaLogonUser(HANDLE, PLSA_STRING, SECURITY_LOGON_TYPE, ULONG,
      PVOID, ULONG, PTOKEN_GROUPS, PTOKEN_SOURCE, PVOID*, PULONG, PLUID,
      PHANDLE, PQUOTA_LIMITS, PNTSTATUS);
    NTSTATUS LsaLookupAuthenticationPackage(HANDLE, PLSA_STRING, PULONG);
    NTSTATUS LsaLookupNames(LSA_HANDLE, ULONG, PLSA_UNICODE_STRING,
      PLSA_REFERENCED_DOMAIN_LIST*, PLSA_TRANSLATED_SID*);
    NTSTATUS LsaLookupSids(LSA_HANDLE, ULONG, PSID*,
      PLSA_REFERENCED_DOMAIN_LIST*, PLSA_TRANSLATED_NAME*);
    ULONG LsaNtStatusToWinError(NTSTATUS);
    NTSTATUS LsaOpenPolicy(PLSA_UNICODE_STRING, PLSA_OBJECT_ATTRIBUTES,
      ACCESS_MASK, PLSA_HANDLE);
    NTSTATUS LsaQueryDomainInformationPolicy(LSA_HANDLE,
      POLICY_DOMAIN_INFORMATION_CLASS, PVOID*);
    NTSTATUS LsaQueryInformationPolicy(LSA_HANDLE, POLICY_INFORMATION_CLASS,
      PVOID*);
    NTSTATUS LsaQueryLocalInformationPolicy(LSA_HANDLE,
      POLICY_LOCAL_INFORMATION_CLASS, PVOID*);
    NTSTATUS LsaQueryTrustedDomainInfo(LSA_HANDLE, PSID,
      TRUSTED_INFORMATION_CLASS, PVOID*);
    NTSTATUS LsaQueryTrustedDomainInfoByName(LSA_HANDLE, PLSA_UNICODE_STRING,
      TRUSTED_INFORMATION_CLASS, PVOID*);
    NTSTATUS LsaRegisterLogonProcess(PLSA_STRING, PHANDLE,
      PLSA_OPERATIONAL_MODE);
    NTSTATUS LsaRemoveAccountRights(LSA_HANDLE, PSID, BOOLEAN,
      PLSA_UNICODE_STRING, ULONG);
    NTSTATUS LsaRetrievePrivateData(LSA_HANDLE, PLSA_UNICODE_STRING,
      PLSA_UNICODE_STRING*);
    NTSTATUS LsaSetDomainInformationPolicy(LSA_HANDLE,
      POLICY_DOMAIN_INFORMATION_CLASS, PVOID);
    NTSTATUS LsaSetInformationPolicy(LSA_HANDLE, POLICY_INFORMATION_CLASS,
      PVOID);
    NTSTATUS LsaSetLocalInformationPolicy(LSA_HANDLE,
      POLICY_LOCAL_INFORMATION_CLASS, PVOID);
    NTSTATUS LsaSetTrustedDomainInformation(LSA_HANDLE, PSID,
      TRUSTED_INFORMATION_CLASS, PVOID);
    NTSTATUS LsaSetTrustedDomainInfoByName(LSA_HANDLE, PLSA_UNICODE_STRING,
      TRUSTED_INFORMATION_CLASS, PVOID);
    NTSTATUS LsaStorePrivateData(LSA_HANDLE, PLSA_UNICODE_STRING,
      PLSA_UNICODE_STRING);
}

alias NTSTATUS function(PUNICODE_STRING, ULONG, PUNICODE_STRING)
  PSAM_PASSWORD_NOTIFICATION_ROUTINE;
alias BOOLEAN function() PSAM_INIT_NOTIFICATION_ROUTINE;
alias BOOLEAN function(PUNICODE_STRING, PUNICODE_STRING,
  PUNICODE_STRING, BOOLEAN) PSAM_PASSWORD_FILTER_ROUTINE;

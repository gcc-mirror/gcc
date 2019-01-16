/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_nb30.d)
 */
module core.sys.windows.nb30;
version (Windows):

private import core.sys.windows.windef;

enum size_t
    NCBNAMSZ =  16,
    MAX_LANA = 254;

// FIXME: are these really two sets of constants?
enum : UCHAR {
    REGISTERING     = 0,
    REGISTERED      = 4,
    DEREGISTERED,
    DUPLICATE,
    DUPLICATE_DEREG, // = 7
    UNIQUE_NAME     = 0,
    GROUP_NAME      = 0x80,
    NAME_FLAGS_MASK = 0x87
}

enum : UCHAR {
    LISTEN_OUTSTANDING = 1,
    CALL_PENDING,
    SESSION_ESTABLISHED,
    HANGUP_PENDING,
    HANGUP_COMPLETE,
    SESSION_ABORTED // = 6
}

enum char[4]
    ALL_TRANSPORTS = "M\0\0\0",
    MS_NBF         = "MNBF";

enum : UCHAR {
    NCBCALL        = 0x10,
    NCBLISTEN,
    NCBHANGUP,  // = 0x12
    NCBSEND        = 0x14,
    NCBRECV,
    NCBRECVANY,
    NCBCHAINSEND, // = 0x17
    NCBDGSEND      = 0x20,
    NCBDGRECV,
    NCBDGSENDBC,
    NCBDGRECVBC, // = 0x23,
    NCBADDNAME     = 0x30,
    NCBDELNAME,
    NCBRESET,
    NCBASTAT,
    NCBSSTAT,
    NCBCANCEL,
    NCBADDGRNAME,
    NCBENUM,    // = 0x37
    NCBUNLINK      = 0x70,
    NCBSENDNA,
    NCBCHAINSENDNA,
    NCBLANSTALERT, // = 0x73
    NCBACTION      = 0x77,
    NCBFINDNAME,
    NCBTRACE    // = 0x79
}

enum UCHAR ASYNCH = 0x80;

enum : UCHAR {
    NRC_GOODRET     = 0x00,
    NRC_BUFLEN      = 0x01,
    NRC_ILLCMD      = 0x03,
    NRC_CMDTMO      = 0x05,
    NRC_INCOMP,
    NRC_BADDR,
    NRC_SNUMOUT,
    NRC_NORES,
    NRC_SCLOSED,
    NRC_CMDCAN,  // = 0x0b
    NRC_DUPNAME     = 0x0d,
    NRC_NAMTFUL,
    NRC_ACTSES,  // = 0x0f,
    NRC_LOCTFUL     = 0x11,
    NRC_REMTFUL,
    NRC_ILLNN,
    NRC_NOCALL,
    NRC_NOWILD,
    NRC_INUSE,
    NRC_NAMERR,
    NRC_SABORT,
    NRC_NAMCONF, // = 0x19
    NRC_IFBUSY      = 0x21,
    NRC_TOOMANY,
    NRC_BRIDGE,
    NRC_CANOCCR, // = 0x24
    NRC_CANCEL      = 0x26,
    NRC_DUPENV      = 0x30,
    NRC_ENVNOTDEF   = 0x34,
    NRC_OSRESNOTAV,
    NRC_MAXAPPS,
    NRC_NOSAPS,
    NRC_NORESOURCES,
    NRC_INVADDRESS, // = 0x39
    NRC_INVDDID     = 0x3B,
    NRC_LOCKFAIL    = 0x3C,
    NRC_OPENERR     = 0x3f,
    NRC_SYSTEM      = 0x40,
    NRC_PENDING     = 0xff
}

struct ACTION_HEADER {
    union {
        /*  transport_id is defined as a ULONG, but both the above constants
         *  and the documented description suggest it should be a char[4]
         */
        ULONG   transport_id;
        char[4] c_transport_id;
    }
    USHORT action_code;
    USHORT reserved;
}
alias ACTION_HEADER* PACTION_HEADER;

struct ADAPTER_STATUS {
    UCHAR[6] adapter_address;
    UCHAR    rev_major;
    UCHAR    reserved0;
    UCHAR    adapter_type;
    UCHAR    rev_minor;
    WORD     duration;
    WORD     frmr_recv;
    WORD     frmr_xmit;
    WORD     iframe_recv_err;
    WORD     xmit_aborts;
    DWORD    xmit_success;
    DWORD    recv_success;
    WORD     iframe_xmit_err;
    WORD     recv_buff_unavail;
    WORD     t1_timeouts;
    WORD     ti_timeouts;
    DWORD    reserved1;
    WORD     free_ncbs;
    WORD     max_cfg_ncbs;
    WORD     max_ncbs;
    WORD     xmit_buf_unavail;
    WORD     max_dgram_size;
    WORD     pending_sess;
    WORD     max_cfg_sess;
    WORD     max_sess;
    WORD     max_sess_pkt_size;
    WORD     name_count;
}
alias ADAPTER_STATUS* PADAPTER_STATUS;

struct FIND_NAME_BUFFER {
    /*  From Win32 API docs
     *
     *  length
     *      Specifies the length, in bytes, of the FIND_NAME_BUFFER
     *      structure. Although this structure always occupies 33 bytes,
     *      not all of the structure is necessarily valid.
     *
     *  On this basis, should length be initialised?
     */
    UCHAR     length;
    UCHAR     access_control;
    UCHAR     frame_control;
    UCHAR[6]  destination_addr;
    UCHAR[6]  source_addr;
    UCHAR[18] routing_info;
}
alias FIND_NAME_BUFFER* PFIND_NAME_BUFFER;

struct FIND_NAME_HEADER {
    WORD  node_count;
    UCHAR reserved;
    UCHAR unique_group;
}
alias FIND_NAME_HEADER* PFIND_NAME_HEADER;

struct LANA_ENUM {
    UCHAR             length;
    UCHAR[MAX_LANA+1] lana;
}
alias LANA_ENUM* PLANA_ENUM;

struct NAME_BUFFER {
    UCHAR[NCBNAMSZ] name;
    UCHAR           name_num;
    UCHAR           name_flags;
}
alias NAME_BUFFER* PNAME_BUFFER;

struct NCB {
    UCHAR           ncb_command;
    UCHAR           ncb_retcode;
    UCHAR           ncb_lsn;
    UCHAR           ncb_num;
    PUCHAR          ncb_buffer;
    WORD            ncb_length;
    UCHAR[NCBNAMSZ] ncb_callname;
    UCHAR[NCBNAMSZ] ncb_name;
    UCHAR           ncb_rto;
    UCHAR           ncb_sto;
    extern (Windows) void function(NCB*) ncb_post;
    UCHAR           ncb_lana_num;
    UCHAR           ncb_cmd_cplt;
    version (Win64)
        UCHAR[18]   ncb_reserve;
    else
        UCHAR[10]   ncb_reserve;
    HANDLE          ncb_event;
}
alias NCB* PNCB;

struct SESSION_BUFFER {
    UCHAR           lsn;
    UCHAR           state;
    UCHAR[NCBNAMSZ] local_name;
    UCHAR[NCBNAMSZ] remote_name;
    UCHAR           rcvs_outstanding;
    UCHAR           sends_outstanding;
}
alias SESSION_BUFFER* PSESSION_BUFFER;

struct SESSION_HEADER {
    UCHAR sess_name;
    UCHAR num_sess;
    UCHAR rcv_dg_outstanding;
    UCHAR rcv_any_outstanding;
}
alias SESSION_HEADER* PSESSION_HEADER;

extern (Windows) UCHAR Netbios(PNCB);

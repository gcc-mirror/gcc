//Written in the D programming language

/++
    D header file for Linux's linux/if_packet.h.

    Copyright: Copyright 2023
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.linux.linux.if_packet;

version (linux):
extern(C):
@nogc:
nothrow:

import core.stdc.config : c_ulong;
import core.sys.posix.sys.socket : sa_family_t;

struct sockaddr_pkt
{
    sa_family_t spkt_family;
    ubyte[14]   spkt_device;
    ushort      spkt_protocol;
}

struct sockaddr_ll
{
    sa_family_t sll_family;
    ushort      sll_protocol;
    int         sll_ifindex;
    ushort      sll_hatype;
    ubyte       sll_pkttype;
    ubyte       sll_halen;
    ubyte[8]    sll_addr;
}

enum : ubyte
{
    PACKET_HOST      = 0,
    PACKET_BROADCAST = 1,
    PACKET_MULTICAST = 2,
    PACKET_OTHERHOST = 3,
    PACKET_OUTGOING  = 4,
    PACKET_LOOPBACK  = 5,
    PACKET_USER      = 6,
    PACKET_KERNEL    = 7,
}

enum
{
    PACKET_ADD_MEMBERSHIP  = 1,
    PACKET_DROP_MEMBERSHIP = 2,
    PACKET_RECV_OUTPUT     = 3,

    PACKET_RX_RING         = 5,
    PACKET_STATISTICS      = 6,
    PACKET_COPY_THRESH     = 7,
    PACKET_AUXDATA         = 8,
    PACKET_ORIGDEV         = 9,
    PACKET_VERSION         = 10,
    PACKET_HDRLEN          = 11,
    PACKET_RESERVE         = 12,
    PACKET_TX_RING         = 13,
    PACKET_LOSS            = 14,
    PACKET_VNET_HDR        = 15,
    PACKET_TX_TIMESTAMP    = 16,
    PACKET_TIMESTAMP       = 17,
    PACKET_FANOUT          = 18,
    PACKET_TX_HAS_OFF      = 19,
    PACKET_QDISC_BYPASS    = 20,
    PACKET_ROLLOVER_STATS  = 21,
    PACKET_FANOUT_DATA     = 22,
    PACKET_IGNORE_OUTGOING = 23,
    PACKET_VNET_HDR_SZ     = 24,

    PACKET_FANOUT_HASH     = 0,
    PACKET_FANOUT_LB       = 1,
    PACKET_FANOUT_CPU      = 2,
    PACKET_FANOUT_ROLLOVER = 3,
    PACKET_FANOUT_RND      = 4,
    PACKET_FANOUT_QM       = 5,
    PACKET_FANOUT_CBPF     = 6,
    PACKET_FANOUT_EBPF     = 7,

    PACKET_FANOUT_FLAG_ROLLOVER        = 0x1000,
    PACKET_FANOUT_FLAG_UNIQUEID        = 0x2000,
    PACKET_FANOUT_FLAG_IGNORE_OUTGOING = 0x4000,
    PACKET_FANOUT_FLAG_DEFRAG          = 0x8000,
}

struct tpacket_stats
{
    uint tp_packets;
    uint tp_drops;
}

struct tpacket_stats_v3
{
    uint tp_packets;
    uint tp_drops;
    uint tp_freeze_q_cnt;
}

struct tpacket_rollover_stats
{
    align(8):
    ulong tp_all;
    ulong tp_huge;
    ulong tp_failed;
}

union tpacket_stats_u
{
    tpacket_stats    stats1;
    tpacket_stats_v3 stats3;
}

struct tpacket_auxdata
{
    uint   tp_status;
    uint   tp_len;
    uint   tp_snaplen;
    ushort tp_mac;
    ushort tp_net;
    ushort tp_vlan_tci;
    ushort tp_vlan_tpid;
}

enum : uint
{
    TP_STATUS_KERNEL          = 0,
    TP_STATUS_USER            = 1 << 0,
    TP_STATUS_COPY            = 1 << 1,
    TP_STATUS_LOSING          = 1 << 2,
    TP_STATUS_CSUMNOTREADY    = 1 << 3,
    TP_STATUS_VLAN_VALID      = 1 << 4,
    TP_STATUS_BLK_TMO         = 1 << 5,
    TP_STATUS_VLAN_TPID_VALID = 1 << 6,
    TP_STATUS_CSUM_VALID      = 1 << 7,
    TP_STATUS_GSO_TCP         = 1 << 8,
}

enum : uint
{
    TP_STATUS_AVAILABLE    = 0,
    TP_STATUS_SEND_REQUEST = 1 << 0,
    TP_STATUS_SENDING      = 1 << 1,
    TP_STATUS_WRONG_FORMAT = 1 << 2,
}

enum : uint
{
    TP_STATUS_TS_SOFTWARE     = 1 << 29,
    TP_STATUS_TS_RAW_HARDWARE = 1U << 31,
}

enum uint TP_FT_REQ_FILL_RXHASH = 0x1;

struct tpacket_hdr
{
    c_ulong tp_status;
    uint    tp_len;
    uint    tp_snaplen;
    ushort  tp_mac;
    ushort  tp_net;
    uint    tp_sec;
    uint    tp_usec;
}

enum TPACKET_ALIGNMENT = 16;
size_t TPACKET_ALIGN(size_t x) { return (x + TPACKET_ALIGNMENT - 1) &~ (TPACKET_ALIGNMENT - 1); }
enum TPACKET_HDRLEN = TPACKET_ALIGN(tpacket_hdr.sizeof) + sockaddr_ll.sizeof;

struct tpacket2_hdr
{
    uint     tp_status;
    uint     tp_len;
    uint     tp_snaplen;
    ushort   tp_mac;
    ushort   tp_net;
    uint     tp_sec;
    uint     tp_nsec;
    ushort   tp_vlan_tci;
    ushort   tp_vlan_tpid;
    ubyte[4] tp_padding;
}

struct tpacket_hdr_variant1
{
    uint   tp_rxhash;
    uint   tp_vlan_tci;
    ushort tp_vlan_tpid;
    ushort tp_padding;
}

struct tpacket3_hdr
{
    uint   tp_next_offset;
    uint   tp_sec;
    uint   tp_nsec;
    uint   tp_snaplen;
    uint   tp_len;
    uint   tp_status;
    ushort tp_mac;
    ushort tp_net;

    union
    {
        tpacket_hdr_variant1 hv1;
    }

    ubyte[8] tp_padding;
}

struct tpacket_bd_ts
{
    uint ts_sec;

    union
    {
        uint ts_usec;
        uint ts_nsec;
    }
}

struct tpacket_hdr_v1
{
    uint block_status;
    uint num_pkts;
    uint offset_to_first_pkt;
    uint blk_len;
    align(8) ulong seq_num;
    tpacket_bd_ts ts_first_pkt;
    tpacket_bd_ts ts_last_pkt;
}

union tpacket_bd_header_u
{
    tpacket_hdr_v1 bh1;
}

struct tpacket_block_desc
{
    uint version_;
    uint offset_to_priv;
    tpacket_bd_header_u hdr;
}

enum TPACKET2_HDRLEN = TPACKET_ALIGN(tpacket2_hdr.sizeof) + sockaddr_ll.sizeof;
enum TPACKET3_HDRLEN = TPACKET_ALIGN(tpacket3_hdr.sizeof) + sockaddr_ll.sizeof;

enum tpacket_versions
{
    TPACKET_V1,
    TPACKET_V2,
    TPACKET_V3
}

struct tpacket_req
{
    uint tp_block_size;
    uint tp_block_nr;
    uint tp_frame_size;
    uint tp_frame_nr;
}

struct tpacket_req3
{
    uint tp_block_size;
    uint tp_block_nr;
    uint tp_frame_size;
    uint tp_frame_nr;
    uint tp_retire_blk_tov;
    uint tp_sizeof_priv;
    uint tp_feature_req_word;
}

union tpacket_req_u
{
    tpacket_req  req;
    tpacket_req3 req3;
}

struct packet_mreq
{
    int      mr_ifindex;
    ushort   mr_type;
    ushort   mr_alen;
    ubyte[8] mr_address;
}

struct fanout_args
{
    version(LittleEndian)
    {
        ushort id;
        ushort type_flags;
    }
    else
    {
        ushort type_flags;
        ushort id;
    }

    uint max_num_members;
}

enum
{
    PACKET_MR_MULTICAST = 0,
    PACKET_MR_PROMISC   = 1,
    PACKET_MR_ALLMULTI  = 2,
    PACKET_MR_UNICAST   = 3,
}

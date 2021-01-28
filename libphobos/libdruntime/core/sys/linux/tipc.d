/**
 * Interface for Linux TIPC sockets, /usr/include/linux/tipc.h
 *
 * Copyright: Public Domain
 * License:   Public Domain
 * Authors:   Leandro Lucarella
 */

module core.sys.linux.tipc;

version (linux):
extern (C) nothrow @nogc:
@system:

struct tipc_portid
{
        uint ref_;
        uint node;
}

struct tipc_name
{
        uint type;
        uint instance;
}

struct tipc_name_seq
{
        uint type;
        uint lower;
        uint upper;
}

struct tipc_subscr
{
        tipc_name_seq seq;
        uint timeout;
        uint filter;
        ubyte[8] usr_handle;
}

struct tipc_event
{
        uint event;
        uint found_lower;
        uint found_upper;
        tipc_portid port;
        tipc_subscr s;
}

struct sockaddr_tipc
{
        ushort family;
        ubyte addrtype;
        byte scope_;
        union Addr
        {
                tipc_portid id;
                tipc_name_seq nameseq;
                static struct Name
                {
                        tipc_name name;
                        uint domain;
                }
                Name name;
        }
        Addr addr;
}

uint tipc_addr(uint zone, uint cluster, uint node)
{
        return (zone << 24) | (cluster << 12) | node;
}

unittest
{
        assert (tipc_addr(0, 0, 0) == 0);
        assert (tipc_addr(1, 1, 1) == 16781313);
        assert (tipc_addr(2, 1, 27) == 33558555);
        assert (tipc_addr(3, 1, 63) == 50335807);
}

uint tipc_zone(uint addr)
{
        return addr >> 24;
}

unittest
{
        assert (tipc_zone(0u) == 0);
        assert (tipc_zone(16781313u) == 1);
        assert (tipc_zone(33558555u) == 2);
        assert (tipc_zone(50335807u) == 3);
}

uint tipc_cluster(uint addr)
{
        return (addr >> 12) & 0xfff;
}

unittest
{
        assert (tipc_cluster(0u) == 0);
        assert (tipc_cluster(16781313u) == 1);
        assert (tipc_cluster(33558555u) == 1);
        assert (tipc_cluster(50335807u) == 1);
}

uint tipc_node(uint addr)
{
        return addr & 0xfff;
}

unittest
{
        assert (tipc_node(0u) == 0);
        assert (tipc_node(16781313u) == 1);
        assert (tipc_node(33558555u) == 27);
        assert (tipc_node(50335807u) == 63);
}

enum: int
{
        TIPC_CFG_SRV    = 0,
        TIPC_TOP_SRV    = 1,
        TIPC_RESERVED_TYPES = 64,
}

enum: int
{
        TIPC_ZONE_SCOPE    = 1,
        TIPC_CLUSTER_SCOPE = 2,
        TIPC_NODE_SCOPE    = 3,
}

enum: int
{
        TIPC_MAX_USER_MSG_SIZE = 66000,
}

enum: int
{
        TIPC_LOW_IMPORTANCE      = 0,
        TIPC_MEDIUM_IMPORTANCE   = 1,
        TIPC_HIGH_IMPORTANCE     = 2,
        TIPC_CRITICAL_IMPORTANCE = 3,
}

enum: int
{
        TIPC_OK     = 0,
        TIPC_ERR_NO_NAME   = 1,
        TIPC_ERR_NO_PORT   = 2,
        TIPC_ERR_NO_NODE   = 3,
        TIPC_ERR_OVERLOAD  = 4,
        TIPC_CONN_SHUTDOWN = 5,
}

enum: int
{
        TIPC_SUB_PORTS    = 0x01,
        TIPC_SUB_SERVICE  = 0x02,
        TIPC_SUB_CANCEL   = 0x04,
}

version (none) enum: int
{
        TIPC_SUB_NO_BIND_EVTS   = 0x04,
        TIPC_SUB_NO_UNBIND_EVTS = 0x08,
        TIPC_SUB_SINGLE_EVT     = 0x10,
}

enum: int
{
        TIPC_WAIT_FOREVER = ~0,
}

enum: int
{

        TIPC_PUBLISHED      = 1,
        TIPC_WITHDRAWN      = 2,
        TIPC_SUBSCR_TIMEOUT = 3,
}

enum: int
{
        AF_TIPC    = 30,
        PF_TIPC    = 30,
        SOL_TIPC          = 271,
        TIPC_ADDR_NAMESEQ = 1,
        TIPC_ADDR_MCAST   = 1,
        TIPC_ADDR_NAME    = 2,
        TIPC_ADDR_ID      = 3,
}

enum: int
{
        TIPC_ERRINFO  = 1,
        TIPC_RETDATA  = 2,
        TIPC_DESTNAME = 3,
}

enum: int
{
        TIPC_IMPORTANCE     = 127,
        TIPC_SRC_DROPPABLE  = 128,
        TIPC_DEST_DROPPABLE = 129,
        TIPC_CONN_TIMEOUT   = 130,
}


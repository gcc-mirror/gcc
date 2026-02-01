/++
    D header file for Linux's net/if.h.

    Copyright: Copyright 2025
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 +/
module core.sys.linux.net.if_;

public import core.sys.posix.sys.socket : sockaddr;
public import core.sys.linux.hdlc.ioctl;

version (linux):
extern(C):
@nogc:
nothrow:

enum IFNAMSIZ = 16;
enum IFALIASZ = 256;
enum ALTIFNAMSIZ = 128;
enum IFHWADDRLEN = 6;

enum IFF_UP          = 0x1;
enum IFF_BROADCAST   = 0x2;
enum IFF_DEBUG       = 0x4;
enum IFF_LOOPBACK    = 0x8;
enum IFF_POINTOPOINT = 0x10;
enum IFF_NOTRAILERS  = 0x20;
enum IFF_RUNNING     = 0x40;
enum IFF_NOARP       = 0x80;
enum IFF_PROMISC     = 0x100;
enum IFF_ALLMULTI    = 0x200;
enum IFF_MASTER      = 0x400;
enum IFF_SLAVE       = 0x800;
enum IFF_MULTICAST   = 0x1000;
enum IFF_PORTSEL     = 0x2000;
enum IFF_AUTOMEDIA   = 0x4000;
enum IFF_DYNAMIC     = 0x8000;
enum IFF_LOWER_UP    = 0x10000;
enum IFF_DORMANT     = 0x20000;
enum IFF_ECHO        = 0x40000;

enum IF_GET_IFACE = 0x0001;		/* for querying only */
enum IF_GET_PROTO = 0x0002;

/* For definitions see hdlc.h */
enum IF_IFACE_V35 = 0x1000;		/* V.35 serial interface	*/
enum IF_IFACE_V24 = 0x1001;		/* V.24 serial interface	*/
enum IF_IFACE_X21 = 0x1002;		/* X.21 serial interface	*/
enum IF_IFACE_T1 = 0x1003;		/* T1 telco serial interface	*/
enum IF_IFACE_E1 = 0x1004;		/* E1 telco serial interface	*/
enum IF_IFACE_SYNC_SERIAL = 0x1005;	/* can't be set by software	*/
enum IF_IFACE_X21D = 0x1006;          /* X.21 Dual Clocking (FarSite) */

/* For definitions see hdlc.h */
enum IF_PROTO_HDLC = 0x2000;		/* raw HDLC protocol		*/
enum IF_PROTO_PPP = 0x2001;		/* PPP protocol			*/
enum IF_PROTO_CISCO = 0x2002;		/* Cisco HDLC protocol		*/
enum IF_PROTO_FR = 0x2003;		/* Frame Relay protocol		*/
enum IF_PROTO_FR_ADD_PVC = 0x2004;	/*    Create FR PVC		*/
enum IF_PROTO_FR_DEL_PVC = 0x2005;	/*    Delete FR PVC		*/
enum IF_PROTO_X25 = 0x2006;		/* X.25				*/
enum IF_PROTO_HDLC_ETH = 0x2007;	/* raw HDLC, Ethernet emulation	*/
enum IF_PROTO_FR_ADD_ETH_PVC = 0x2008;	/*  Create FR Ethernet-bridged PVC */
enum IF_PROTO_FR_DEL_ETH_PVC =  0x2009;	/*  Delete FR Ethernet-bridged PVC */
enum IF_PROTO_FR_PVC = 0x200A;		/* for reading PVC status	*/
enum IF_PROTO_FR_ETH_PVC = 0x200B;
enum IF_PROTO_RAW = 0x200C;          /* RAW Socket                   */

enum IF_OPER_UNKNOWN = 0;
enum IF_OPER_NOTPRESENT = 1;
enum IF_OPER_DOWN = 2;
enum IF_OPER_LOWERLAYERDOWN = 3;
enum IF_OPER_TESTING = 4;
enum IF_OPER_DORMANT = 5;
enum IF_OPER_UP = 6;

enum IF_LINK_MODE_DEFAULT = 0;
enum IF_LINK_MODE_DORMANT = 1;	/* limit upward transition to dormant */
enum IF_LINK_MODE_TESTING = 2;	/* limit upward transition to testing */

struct ifmap {
	uint mem_start;
	uint mem_end;
	ushort base_addr;
	ubyte irq;
	ubyte dma;
	ubyte port;
	/* 3 bytes spare */
}

struct if_settings {
	uint type;	/* Type of physical device or protocol */
	uint size;	/* Size of the data allocated by the caller */
	union {
		/* {atm/eth/dsl}_settings anyone ? */
		raw_hdlc_proto* raw_hdlc;
		cisco_proto* cisco;
		fr_proto* fr;
		fr_proto_pvc* fr_pvc;
		fr_proto_pvc_info* fr_pvc_info;
		x25_hdlc_proto* x25;

		/* interface settings */
		sync_serial_settings* sync;
		te1_settings* te1;
	};
}

struct ifreq {
	union
	{
		char[IFNAMSIZ]	ifrn_name = 0;		/* if name, e.g. "en0" */
	}

	union {

		short	ifru_flags;
		int	ifru_ivalue;
		int	ifru_mtu;
		ifmap ifru_map;
		char[IFNAMSIZ]	ifru_slave;	/* Just fits the size */
		char[IFNAMSIZ]	ifru_newname;
		void*	ifru_data;
		if_settings ifru_settings;
	}
}

struct ifconf {
	int	ifc_len;			/* size of buffer	*/
	union {
		char* ifcu_buf;
		ifreq* ifcu_req;
	}
}

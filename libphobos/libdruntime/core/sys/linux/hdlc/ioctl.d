/* SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note */
module core.sys.linux.hdlc.ioctl;

import core.sys.linux.net.if_ : IFNAMSIZ;

version (linux):
extern(C):
@nogc:
nothrow:

enum GENERIC_HDLC_VERSION = 4;	/* For synchronization with sethdlc utility */

enum CLOCK_DEFAULT = 0;	/* Default setting */
enum CLOCK_EXT = 1;	/* External TX and RX clock - DTE */
enum CLOCK_INT = 2;	/* Internal TX and RX clock - DCE */
enum CLOCK_TXINT = 3;	/* Internal TX and external RX clock */
enum CLOCK_TXFROMRX = 4;	/* TX clock derived from external RX clock */


enum ENCODING_DEFAULT = 0; /* Default setting */
enum ENCODING_NRZ = 1;
enum ENCODING_NRZI = 2;
enum ENCODING_FM_MARK = 3;
enum ENCODING_FM_SPACE = 4;
enum ENCODING_MANCHESTER = 5;


enum PARITY_DEFAULT = 0; /* Default setting */
enum PARITY_NONE = 1; /* No parity */
enum PARITY_CRC16_PR0 = 2; /* CRC16, initial value 0x0000 */
enum PARITY_CRC16_PR1 = 3; /* CRC16, initial value 0xFFFF */
enum PARITY_CRC16_PR0_CCITT = 4; /* CRC16, initial 0x0000, ITU-T version */
enum PARITY_CRC16_PR1_CCITT = 5; /* CRC16, initial 0xFFFF, ITU-T version */
enum PARITY_CRC32_PR0_CCITT = 6; /* CRC32, initial value 0x00000000 */
enum PARITY_CRC32_PR1_CCITT = 7; /* CRC32, initial value 0xFFFFFFFF */

enum LMI_DEFAULT = 0; /* Default setting */
enum LMI_NONE = 1; /* No LMI, all PVCs are static */
enum LMI_ANSI = 2; /* ANSI Annex D */
enum LMI_CCITT = 3; /* ITU-T Annex A */
enum LMI_CISCO = 4; /* The "original" LMI, aka Gang of Four */

struct sync_serial_settings {
	uint clock_rate; /* bits per second */
	uint clock_type; /* internal, external, TX-internal etc. */
	ushort loopback;
}          /* V.35, V.24, X.21 */

struct te1_settings{
	uint clock_rate; /* bits per second */
	uint clock_type; /* internal, external, TX-internal etc. */
	ushort loopback;
	uint slot_map;
}                  /* T1, E1 */

struct raw_hdlc_proto {
	ushort encoding;
	ushort parity;
}

struct fr_proto {
	uint t391;
	uint t392;
	uint n391;
	uint n392;
	uint n393;
	ushort lmi;
	ushort dce; /* 1 for DCE (network side) operation */
}

struct fr_proto_pvc {
	uint dlci;
}          /* for creating/deleting FR PVCs */

struct fr_proto_pvc_info {
	uint dlci;
	char[IFNAMSIZ] master = 0;	/* Name of master FRAD device */
}		/* for returning PVC information only */

struct cisco_proto {
    uint interval;
    uint timeout;
}

struct x25_hdlc_proto {
	ushort dce; /* 1 for DCE (network side) operation */
	uint modulo; /* modulo (8 = basic / 128 = extended) */
	uint window; /* frame window size */
	uint t1; /* timeout t1 */
	uint t2; /* timeout t2 */
	uint n2; /* frame retry counter */
}

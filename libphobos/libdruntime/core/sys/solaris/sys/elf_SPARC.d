/**
 * D header file for Solaris.
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/uts/common/sys/elf_SPARC.h, illumos sys/elf_SPARC.h)
 */
module core.sys.solaris.sys.elf_SPARC;

version (Solaris):
extern (C):
nothrow:

enum EF_SPARC_32PLUS_MASK = 0xffff00;
enum EF_SPARC_32PLUS      = 0x000100;
enum EF_SPARC_EXT_MASK    = 0xffff00;
enum EF_SPARC_SUN_US1     = 0x000200;
enum EF_SPARC_HAL_R1      = 0x000400;
enum EF_SPARC_SUN_US3     = 0x000800;

enum EF_SPARCV9_MM  = 0x3;
enum EF_SPARCV9_TSO = 0x0;
enum EF_SPARCV9_PSO = 0x1;
enum EF_SPARCV9_RMO = 0x2;

enum R_SPARC_NONE             = 0;
enum R_SPARC_8                = 1;
enum R_SPARC_16               = 2;
enum R_SPARC_32               = 3;
enum R_SPARC_DISP8            = 4;
enum R_SPARC_DISP16           = 5;
enum R_SPARC_DISP32           = 6;
enum R_SPARC_WDISP30          = 7;
enum R_SPARC_WDISP22          = 8;
enum R_SPARC_HI22             = 9;
enum R_SPARC_22               = 10;
enum R_SPARC_13               = 11;
enum R_SPARC_LO10             = 12;
enum R_SPARC_GOT10            = 13;
enum R_SPARC_GOT13            = 14;
enum R_SPARC_GOT22            = 15;
enum R_SPARC_PC10             = 16;
enum R_SPARC_PC22             = 17;
enum R_SPARC_WPLT30           = 18;
enum R_SPARC_COPY             = 19;
enum R_SPARC_GLOB_DAT         = 20;
enum R_SPARC_JMP_SLOT         = 21;
enum R_SPARC_RELATIVE         = 22;
enum R_SPARC_UA32             = 23;
enum R_SPARC_PLT32            = 24;
enum R_SPARC_HIPLT22          = 25;
enum R_SPARC_LOPLT10          = 26;
enum R_SPARC_PCPLT32          = 27;
enum R_SPARC_PCPLT22          = 28;
enum R_SPARC_PCPLT10          = 29;
enum R_SPARC_10               = 30;
enum R_SPARC_11               = 31;
enum R_SPARC_64               = 32;
enum R_SPARC_OLO10            = 33;
enum R_SPARC_HH22             = 34;
enum R_SPARC_HM10             = 35;
enum R_SPARC_LM22             = 36;
enum R_SPARC_PC_HH22          = 37;
enum R_SPARC_PC_HM10          = 38;
enum R_SPARC_PC_LM22          = 39;
enum R_SPARC_WDISP16          = 40;
enum R_SPARC_WDISP19          = 41;
enum R_SPARC_GLOB_JMP         = 42;
enum R_SPARC_7                = 43;
enum R_SPARC_5                = 44;
enum R_SPARC_6                = 45;
enum R_SPARC_DISP64           = 46;
enum R_SPARC_PLT64            = 47;
enum R_SPARC_HIX22            = 48;
enum R_SPARC_LOX10            = 49;
enum R_SPARC_H44              = 50;
enum R_SPARC_M44              = 51;
enum R_SPARC_L44              = 52;
enum R_SPARC_REGISTER         = 53;
enum R_SPARC_UA64             = 54;
enum R_SPARC_UA16             = 55;
enum R_SPARC_TLS_GD_HI22      = 56;
enum R_SPARC_TLS_GD_LO10      = 57;
enum R_SPARC_TLS_GD_ADD       = 58;
enum R_SPARC_TLS_GD_CALL      = 59;
enum R_SPARC_TLS_LDM_HI22     = 60;
enum R_SPARC_TLS_LDM_LO10     = 61;
enum R_SPARC_TLS_LDM_ADD      = 62;
enum R_SPARC_TLS_LDM_CALL     = 63;
enum R_SPARC_TLS_LDO_HIX22    = 64;
enum R_SPARC_TLS_LDO_LOX10    = 65;
enum R_SPARC_TLS_LDO_ADD      = 66;
enum R_SPARC_TLS_IE_HI22      = 67;
enum R_SPARC_TLS_IE_LO10      = 68;
enum R_SPARC_TLS_IE_LD        = 69;
enum R_SPARC_TLS_IE_LDX       = 70;
enum R_SPARC_TLS_IE_ADD       = 71;
enum R_SPARC_TLS_LE_HIX22     = 72;
enum R_SPARC_TLS_LE_LOX10     = 73;
enum R_SPARC_TLS_DTPMOD32     = 74;
enum R_SPARC_TLS_DTPMOD64     = 75;
enum R_SPARC_TLS_DTPOFF32     = 76;
enum R_SPARC_TLS_DTPOFF64     = 77;
enum R_SPARC_TLS_TPOFF32      = 78;
enum R_SPARC_TLS_TPOFF64      = 79;
enum R_SPARC_GOTDATA_HIX22    = 80;
enum R_SPARC_GOTDATA_LOX10    = 81;
enum R_SPARC_GOTDATA_OP_HIX22 = 82;
enum R_SPARC_GOTDATA_OP_LOX10 = 83;
enum R_SPARC_GOTDATA_OP       = 84;
enum R_SPARC_H34              = 85;
enum R_SPARC_SIZE32           = 86;
enum R_SPARC_SIZE64           = 87;
enum R_SPARC_NUM              = 88;

enum R_SPARC_L34 = R_SPARC_L44;

enum ELF_SPARC_MAXPGSZ   = 0x10000;
enum ELF_SPARCV9_MAXPGSZ = 0x100000;

enum SHT_SPARC_GOTDATA = 0x70000000;

enum SHN_BEFORE = 0xff00;
enum SHN_AFTER  =  0xff01;

enum STT_SPARC_REGISTER = 13;

enum DT_SPARC_REGISTER = 0x70000001;

enum STO_SPARC_REGISTER_G1 = 0x1;
enum STO_SPARC_REGISTER_G2 = 0x2;
enum STO_SPARC_REGISTER_G3 = 0x3;
enum STO_SPARC_REGISTER_G4 = 0x4;
enum STO_SPARC_REGISTER_G5 = 0x5;
enum STO_SPARC_REGISTER_G6 = 0x6;
enum STO_SPARC_REGISTER_G7 = 0x7;

enum M_PLT_INSSIZE  = 4;
enum M_PLT_XNumber  = 4;
enum M_GOT_XDYNAMIC = 0;
enum M_GOT_XNumber  = 1;

enum M32_WORD_ALIGN   = 4;
enum M32_PLT_ENTSIZE  = 12;
enum M32_PLT_ALIGN    = M_WORD_ALIGN;
enum M32_GOT_ENTSIZE  = 4;
enum M32_GOT_MAXSMALL = 2048;
enum M32_PLT_RESERVSZ = (M_PLT_XNumber * M32_PLT_ENTSIZE);

enum M64_WORD_ALIGN   = 8;
enum M64_PLT_ENTSIZE  = 32;
enum M64_PLT_ALIGN    = 256;
enum M64_GOT_ENTSIZE  = 8;
enum M64_GOT_MAXSMALL = 1024;
enum M64_PLT_RESERVSZ = (M_PLT_XNumber * M64_PLT_ENTSIZE);

enum M64_PLT_NEARPLTS = 0x8000;
enum M64_PLT_FENTSIZE = 24;
enum M64_PLT_PSIZE    = 8;
enum M64_PLT_FBLKCNTS = 160;
enum M64_PLT_FBLOCKSZ = (M64_PLT_FBLKCNTS * M64_PLT_ENTSIZE);

version (_ELF64)
{
    enum M_WORD_ALIGN   = M64_WORD_ALIGN;
    enum M_PLT_ENTSIZE  = M64_PLT_ENTSIZE;
    enum M_PLT_ALIGN    = M64_PLT_ALIGN;
    enum M_PLT_RESERVSZ = M64_PLT_RESERVSZ;
    enum M_GOT_ENTSIZE  = M64_GOT_ENTSIZE;
    enum M_GOT_MAXSMALL = M64_GOT_MAXSMALL;
}
else
{
    enum M_WORD_ALIGN   = M32_WORD_ALIGN;
    enum M_PLT_ENTSIZE  = M32_PLT_ENTSIZE;
    enum M_PLT_ALIGN    = M32_PLT_ALIGN;
    enum M_PLT_RESERVSZ = M32_PLT_RESERVSZ;
    enum M_GOT_ENTSIZE  = M32_GOT_ENTSIZE;
    enum M_GOT_MAXSMALL = M32_GOT_MAXSMALL;
}

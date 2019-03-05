/**
 * D header file for Solaris.
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/uts/common/sys/elf_386.h, illumos sys/elf_386.h)
 */
module core.sys.solaris.sys.elf_386;

version (Solaris):
extern (C):
nothrow:

enum R_386_NONE         = 0;
enum R_386_32           = 1;
enum R_386_PC32         = 2;
enum R_386_GOT32        = 3;
enum R_386_PLT32        = 4;
enum R_386_COPY         = 5;
enum R_386_GLOB_DAT     = 6;
enum R_386_JMP_SLOT     = 7;
enum R_386_RELATIVE     = 8;
enum R_386_GOTOFF       = 9;
enum R_386_GOTPC        = 10;
enum R_386_32PLT        = 11;
enum R_386_TLS_GD_PLT   = 12;
enum R_386_TLS_LDM_PLT  = 13;
enum R_386_TLS_TPOFF    = 14;
enum R_386_TLS_IE       = 15;
enum R_386_TLS_GOTIE    = 16;
enum R_386_TLS_LE       = 17;
enum R_386_TLS_GD       = 18;
enum R_386_TLS_LDM      = 19;
enum R_386_16           = 20;
enum R_386_PC16         = 21;
enum R_386_8            = 22;
enum R_386_PC8          = 23;
enum R_386_UNKNOWN24    = 24;
enum R_386_UNKNOWN25    = 25;
enum R_386_UNKNOWN26    = 26;
enum R_386_UNKNOWN27    = 27;
enum R_386_UNKNOWN28    = 28;
enum R_386_UNKNOWN29    = 29;
enum R_386_UNKNOWN30    = 30;
enum R_386_UNKNOWN31    = 31;
enum R_386_TLS_LDO_32   = 32;
enum R_386_UNKNOWN33    = 33;
enum R_386_UNKNOWN34    = 34;
enum R_386_TLS_DTPMOD32 = 35;
enum R_386_TLS_DTPOFF32 = 36;
enum R_386_UNKNOWN37    = 37;
enum R_386_SIZE32       = 38;
enum R_386_NUM          = 39;

enum ELF_386_MAXPGSZ = 0x10000;

enum SHF_ORDERED = 0x40000000;
enum SHF_EXCLUDE = 0x80000000;

enum SHN_BEFORE = 0xff00;
enum SHN_AFTER  = 0xff01;

enum M_PLT_INSSIZE  = 6;
enum M_PLT_XNumber  = 1;
enum M_GOT_XDYNAMIC = 0;
enum M_GOT_XLINKMAP = 1;
enum M_GOT_XRTLD    = 2;
enum M_GOT_XNumber  = 3;

enum M32_WORD_ALIGN   = 4;
enum M32_PLT_ENTSIZE  = 16;
enum M32_PLT_ALIGN    = M32_WORD_ALIGN;
enum M32_GOT_ENTSIZE  = 4;
enum M32_PLT_RESERVSZ = (M_PLT_XNumber * M32_PLT_ENTSIZE);

version (_ELF64) {}
else
{
    enum M_WORD_ALIGN   = M32_WORD_ALIGN;
    enum M_PLT_ENTSIZE  = M32_PLT_ENTSIZE;
    enum M_PLT_ALIGN    = M32_PLT_ALIGN;
    enum M_PLT_RESERVSZ = M32_PLT_RESERVSZ;
    enum M_GOT_ENTSIZE  = M32_GOT_ENTSIZE;
}

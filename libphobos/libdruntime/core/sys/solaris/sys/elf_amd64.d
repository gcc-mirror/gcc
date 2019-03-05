/**
 * D header file for Solaris.
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/uts/common/sys/elf_amd64.h, illumos sys/elf_amd64.h)
 */
module core.sys.solaris.sys.elf_amd64;

version (Solaris):
extern (C):
nothrow:

public import core.sys.solaris.sys.elf_386;

enum R_AMD64_NONE       = 0;
enum R_AMD64_64         = 1;
enum R_AMD64_PC32       = 2;
enum R_AMD64_GOT32      = 3;
enum R_AMD64_PLT32      = 4;
enum R_AMD64_COPY       = 5;
enum R_AMD64_GLOB_DAT   = 6;
enum R_AMD64_JUMP_SLOT  = 7;
enum R_AMD64_RELATIVE   = 8;
enum R_AMD64_GOTPCREL   = 9;
enum R_AMD64_32         = 10;
enum R_AMD64_32S        = 11;
enum R_AMD64_16         = 12;
enum R_AMD64_PC16       = 13;
enum R_AMD64_8          = 14;
enum R_AMD64_PC8        = 15;
enum R_AMD64_DTPMOD64   = 16;
enum R_AMD64_DTPOFF64   = 17;
enum R_AMD64_TPOFF64    = 18;
enum R_AMD64_TLSGD      = 19;
enum R_AMD64_TLSLD      = 20;
enum R_AMD64_DTPOFF32   = 21;
enum R_AMD64_GOTTPOFF   = 22;
enum R_AMD64_TPOFF32    = 23;
enum R_AMD64_PC64       = 24;
enum R_AMD64_GOTOFF64   = 25;
enum R_AMD64_GOTPC32    = 26;
enum R_AMD64_GOT64      = 27;
enum R_AMD64_GOTPCREL64 = 28;
enum R_AMD64_GOTPC64    = 29;
enum R_AMD64_GOTPLT64   = 30;
enum R_AMD64_PLTOFF64   = 31;
enum R_AMD64_SIZE32     = 32;
enum R_AMD64_SIZE64     = 33;
enum R_AMD64_NUM        = 34;


enum R_X86_64_NONE       = R_AMD64_NONE;
enum R_X86_64_64         = R_AMD64_64;
enum R_X86_64_PC32       = R_AMD64_PC32;
enum R_X86_64_GOT32      = R_AMD64_GOT32;
enum R_X86_64_PLT32      = R_AMD64_PLT32;
enum R_X86_64_COPY       = R_AMD64_COPY;
enum R_X86_64_GLOB_DAT   = R_AMD64_GLOB_DAT;
enum R_X86_64_JUMP_SLOT  = R_AMD64_JUMP_SLOT;
enum R_X86_64_RELATIVE   = R_AMD64_RELATIVE;
enum R_X86_64_GOTPCREL   = R_AMD64_GOTPCREL;
enum R_X86_64_32         = R_AMD64_32;
enum R_X86_64_32S        = R_AMD64_32S;
enum R_X86_64_16         = R_AMD64_16;
enum R_X86_64_PC16       = R_AMD64_PC16;
enum R_X86_64_8          = R_AMD64_8;
enum R_X86_64_PC8        = R_AMD64_PC8;
enum R_X86_64_DTPMOD64   = R_AMD64_DTPMOD64;
enum R_X86_64_DTPOFF64   = R_AMD64_DTPOFF64;
enum R_X86_64_TPOFF64    = R_AMD64_TPOFF64;
enum R_X86_64_TLSGD      = R_AMD64_TLSGD;
enum R_X86_64_TLSLD      = R_AMD64_TLSLD;
enum R_X86_64_DTPOFF32   = R_AMD64_DTPOFF32;
enum R_X86_64_GOTTPOFF   = R_AMD64_GOTTPOFF;
enum R_X86_64_TPOFF32    = R_AMD64_TPOFF32;
enum R_X86_64_PC64       = R_AMD64_PC64;
enum R_X86_64_GOTPC32    = R_AMD64_GOTPC32;
enum R_X86_64_GOTOFF64   = R_AMD64_GOTOFF64;
enum R_X86_64_GOT64      = R_AMD64_GOT64;
enum R_X86_64_GOTPCREL64 = R_AMD64_GOTPCREL64;
enum R_X86_64_GOTPC64    = R_AMD64_GOTPC64;
enum R_X86_64_GOTPLT64   = R_AMD64_GOTPLT64;
enum R_X86_64_PLTOFF64   = R_AMD64_PLTOFF64;
enum R_X86_64_SIZE32     = R_AMD64_SIZE32;
enum R_X86_64_SIZE64     = R_AMD64_SIZE64;
enum R_X86_64_NUM        = R_AMD64_NUM;

enum ELF_AMD64_MAXPGSZ = 0x100000;

enum SHT_AMD64_UNWIND   = 0x70000001;
enum SHT_X86_64_UNWIND  = SHT_AMD64_UNWIND;

enum SHF_AMD64_LARGE  = 0x10000000;
enum SHF_X86_64_LARGE = SHF_AMD64_LARGE;

enum SHN_AMD64_LCOMMON  = 0xff02;
enum SHN_X86_64_LCOMMON = SHN_AMD64_LCOMMON;

enum M64_WORD_ALIGN   = 8;
enum M64_PLT_ENTSIZE  = M32_PLT_ENTSIZE;
enum M64_PLT_ALIGN    = M64_WORD_ALIGN;
enum M64_GOT_ENTSIZE  = 8;
enum M64_PLT_RESERVSZ = M32_PLT_RESERVSZ;

version (_ELF64)
{
    enum M_WORD_ALIGN   = M64_WORD_ALIGN;
    enum M_PLT_ENTSIZE  = M64_PLT_ENTSIZE;
    enum M_PLT_ALIGN    = M64_PLT_ALIGN;
    enum M_PLT_RESERVSZ = M64_PLT_RESERVSZ;
    enum M_GOT_ENTSIZE  = M64_GOT_ENTSIZE;
}

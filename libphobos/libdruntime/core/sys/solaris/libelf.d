/**
 * D header file for Solaris.
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/head/libelf.h, illumos libelf.h)
 */
module core.sys.solaris.libelf;

version (Solaris):
extern (C):
nothrow:

import core.stdc.config;
import core.sys.posix.sys.types;
import core.sys.solaris.sys.elf;

enum Elf_Cmd
{
    ELF_C_NULL = 0,
    ELF_C_READ,
    ELF_C_WRITE,
    ELF_C_CLR,
    ELF_C_SET,
    ELF_C_FDDONE,
    ELF_C_FDREAD,
    ELF_C_RDWR,
    ELF_C_WRIMAGE,
    ELF_C_IMAGE,
    ELF_C_NUM
}

enum ELF_F_DIRTY  = 0x1;
enum ELF_F_LAYOUT = 0x4;

enum Elf_Kind
{
    ELF_K_NONE = 0,
    ELF_K_AR,
    ELF_K_COFF,
    ELF_K_ELF,
    ELF_K_NUM
}

enum Elf_Type
{
    ELF_T_BYTE = 0,
    ELF_T_ADDR,
    ELF_T_DYN,
    ELF_T_EHDR,
    ELF_T_HALF,
    ELF_T_OFF,
    ELF_T_PHDR,
    ELF_T_RELA,
    ELF_T_REL,
    ELF_T_SHDR,
    ELF_T_SWORD,
    ELF_T_SYM,
    ELF_T_WORD,
    ELF_T_VDEF,
    ELF_T_VNEED,
    ELF_T_SXWORD,
    ELF_T_XWORD,
    ELF_T_SYMINFO,
    ELF_T_NOTE,
    ELF_T_MOVE,
    ELF_T_MOVEP,
    ELF_T_CAP,
    ELF_T_NUM
}

struct Elf
{
}

struct Elf_Scn
{
}

struct Elf_Arhdr
{
    char*   ar_name;
    time_t  ar_date;
    uid_t   ar_uid;
    gid_t   ar_gid;
    mode_t  ar_mode;
    off_t   ar_size;
    char*   ar_rawname;
}

struct Elf_Arsym
{
    char*    as_name;
    size_t   as_off;
    c_ulong  as_hash;
}

struct Elf_Data
{
  void*     d_buf;
  Elf_Type  d_type;
  size_t    d_size;
  off_t     d_off;
  size_t    d_align;
  uint      d_version;
}

Elf* elf_begin(int, Elf_Cmd, Elf*);
int elf_cntl(Elf*, Elf_Cmd);
int elf_end(Elf*);
const(char)* elf_errmsg(int);
int elf_errno();
void elf_fill(int);
uint elf_flagdata(Elf_Data*, Elf_Cmd, uint);
uint elf_flagehdr(Elf*, Elf_Cmd,  uint);
uint elf_flagelf(Elf*, Elf_Cmd, uint);
uint elf_flagphdr(Elf*, Elf_Cmd, uint);
uint elf_flagscn(Elf_Scn*, Elf_Cmd, uint);
uint elf_flagshdr(Elf_Scn*, Elf_Cmd, uint);
size_t elf32_fsize(Elf_Type, size_t, uint);
Elf_Arhdr* elf_getarhdr(Elf*);
Elf_Arsym* elf_getarsym(Elf*, size_t*);
off_t elf_getbase(Elf*);
Elf_Data* elf_getdata(Elf_Scn*, Elf_Data*);
Elf32_Ehdr* elf32_getehdr(Elf*);
char* elf_getident(Elf*, size_t*);
Elf32_Phdr* elf32_getphdr(Elf*);
Elf_Scn* elf_getscn(Elf*, size_t);
Elf32_Shdr* elf32_getshdr(Elf_Scn*);
int elf_getphnum(Elf*, size_t*);
int elf_getphdrnum(Elf*, size_t*);
int elf_getshnum(Elf*, size_t*);
int elf_getshdrnum(Elf*, size_t*);
int elf_getshstrndx(Elf*, size_t*);
int elf_getshdrstrndx(Elf*, size_t*);
c_ulong elf_hash(const scope char*);
uint elf_sys_encoding();
long elf32_checksum(Elf*);
Elf_Kind elf_kind(Elf*);
Elf* elf_memory(char*, size_t);
size_t elf_ndxscn(Elf_Scn*);
Elf_Data* elf_newdata(Elf_Scn*);
Elf32_Ehdr* elf32_newehdr(Elf*);
Elf32_Phdr* elf32_newphdr(Elf*, size_t);
Elf_Scn* elf_newscn(Elf*);
Elf_Scn* elf_nextscn(Elf*, Elf_Scn*);
Elf_Cmd elf_next(Elf*);
size_t elf_rand(Elf*, size_t);
Elf_Data* elf_rawdata(Elf_Scn*, Elf_Data*);
char* elf_rawfile(Elf*, size_t*);
char* elf_strptr(Elf*, size_t, size_t);
off_t elf_update(Elf*, Elf_Cmd);
uint elf_version(uint);
Elf_Data* elf32_xlatetof(Elf_Data*, const scope Elf_Data*, uint);
Elf_Data* elf32_xlatetom(Elf_Data*, const scope Elf_Data*, uint);

version (D_LP64)
{
size_t elf64_fsize(Elf_Type, size_t, uint);
Elf64_Ehdr* elf64_getehdr(Elf*);
Elf64_Phdr* elf64_getphdr(Elf*);
Elf64_Shdr* elf64_getshdr(Elf_Scn*);
long elf64_checksum(Elf*);
Elf64_Ehdr* elf64_newehdr(Elf*);
Elf64_Phdr* elf64_newphdr(Elf*, size_t);
Elf_Data* elf64_xlatetof(Elf_Data*, const scope Elf_Data*, uint);
Elf_Data* elf64_xlatetom(Elf_Data*, const scope Elf_Data*, uint);
}

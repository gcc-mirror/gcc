/**
 * D header file for Solaris.
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/uts/common/sys/elftypes.h, illumos sys/elftypes.h)
 */
module core.sys.solaris.sys.elftypes;

version (Solaris):
extern (C):
nothrow:

import core.stdc.stdint;

alias uint32_t Elf32_Addr;
alias uint16_t Elf32_Half;
alias uint32_t Elf32_Off;
alias int32_t  Elf32_Sword;
alias uint32_t Elf32_Word;

alias uint64_t Elf64_Addr;
alias uint16_t Elf64_Half;
alias uint64_t Elf64_Off;
alias int32_t  Elf64_Sword;
alias int64_t  Elf64_Sxword;
alias uint32_t Elf64_Word;
alias uint64_t Elf64_Xword;
alias uint64_t Elf64_Lword;
alias uint64_t Elf32_Lword;

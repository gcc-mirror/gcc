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

public import core.sys.elf :
 Elf32_Addr, Elf32_Half, Elf32_Off,
 Elf32_Sword, Elf32_Word,
 Elf64_Addr, Elf64_Half, Elf64_Off,
 Elf64_Sword, Elf64_Sxword,
 Elf64_Word, Elf64_Xword;

alias uint64_t Elf64_Lword;
alias uint64_t Elf32_Lword;

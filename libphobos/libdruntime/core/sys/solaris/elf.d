/**
 * D header file for Solaris.
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/head/elf.h, illumos elf.h)
 */
module core.sys.solaris.elf;

version (Solaris):
extern (C):
nothrow:

public import core.sys.solaris.sys.elf;

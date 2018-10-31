/**
 * D header file for FreeBSD.
 *
 * $(LINK2 http://svnweb.freebsd.org/base/head/sys/sys/elf.h?view=markup, sys/elf.h)
 */
module core.sys.freebsd.sys.elf;

version (FreeBSD):

public import core.sys.freebsd.sys.elf32;
public import core.sys.freebsd.sys.elf64;

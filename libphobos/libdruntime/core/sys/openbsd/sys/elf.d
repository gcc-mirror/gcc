/**
 * D header file for OpenBSD.
 *
 * Authors:  Iain Buclaw
 * Based-on: core/sys/freebsd/sys
 */
module core.sys.openbsd.sys.elf;

version (OpenBSD):

public import core.sys.openbsd.sys.elf32;
public import core.sys.openbsd.sys.elf64;

/**
 * D header file for DragonFlyBSD.
 *
 * Authors: Diederik de Groot(port:DragonFlyBSD)
 * Copied:  From core/sys/freebsd/sys
 */
module core.sys.dragonflybsd.sys.elf;

version (DragonFlyBSD):

public import core.sys.dragonflybsd.sys.elf32;
public import core.sys.dragonflybsd.sys.elf64;

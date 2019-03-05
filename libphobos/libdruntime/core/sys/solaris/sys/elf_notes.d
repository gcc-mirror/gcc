/**
 * D header file for Solaris.
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/uts/common/sys/elf_notes.h, illumos sys/elf_notes.h)
 */
module core.sys.solaris.sys.elf_notes;

version (Solaris):
extern (C):
nothrow:

enum ELF_NOTE_SOLARIS = "SUNW Solaris";

enum ELF_NOTE_PAGESIZE_HINT = 1;

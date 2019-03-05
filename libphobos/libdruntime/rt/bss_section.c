/**
 * This module is used to detect copy relocated ModuleInfos (located in .bss section).
 *
 * Copyright: Copyright Martin Nowak 2014-.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 * Source: $(DRUNTIMESRC src/rt/_bss_section.c)
 */

/* These symbols are defined in the linker script and bracket the
 * .bss, .lbss, .lrodata and .ldata sections.
 */
#if defined(__linux__) || defined(__FreeBSD__) || defined(__NetBSD__)
// Need to use weak linkage to workaround a bug in ld.bfd (Bugzilla 13025).
extern int __attribute__((weak)) __bss_start, _end;

__attribute__ ((visibility ("hidden"))) void* rt_get_bss_start();
__attribute__ ((visibility ("hidden"))) void* rt_get_end();
void* rt_get_bss_start() { return (void*)&__bss_start; }
void* rt_get_end() { return (void*)&_end; }
#endif

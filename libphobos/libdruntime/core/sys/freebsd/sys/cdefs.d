/**
 * D header file for FreeBSD
 *
 * Authors: Martin Nowak
 */
module core.sys.freebsd.sys.cdefs;

version (FreeBSD):

public import core.sys.posix.config;

// https://svnweb.freebsd.org/base/head/sys/sys/cdefs.h?revision=271155&view=markup
enum __POSIX_VISIBLE = 200112;
enum __XSI_VISIBLE = 700;
enum __BSD_VISIBLE = true;
enum __ISO_C_VISIBLE = 1999;

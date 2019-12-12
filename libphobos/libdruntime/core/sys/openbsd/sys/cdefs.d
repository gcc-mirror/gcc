/**
 * D header file for OpenBSD
 *
 * Authors: Iain Buclaw
 */
module core.sys.openbsd.sys.cdefs;

version (OpenBSD):

public import core.sys.posix.config;

enum __XPG_VISIBLE = 700;
enum __POSIX_VISIBLE = 200809;
enum __ISO_C_VISIBLE = 1999;
enum __BSD_VISIBLE = true;

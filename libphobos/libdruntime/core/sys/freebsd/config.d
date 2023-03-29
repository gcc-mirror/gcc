/**
 * D header file for FreeBSD
 *
 * Authors: Iain Buclaw
 */
module core.sys.freebsd.config;

version (FreeBSD):

public import core.sys.posix.config;

// https://svnweb.freebsd.org/base/head/sys/sys/param.h?view=markup
// __FreeBSD_version numbers are documented in the Porter's Handbook.
// NOTE: When adding newer versions of FreeBSD, verify all current versioned
// bindings are still compatible with the release.

     version (FreeBSD_14) enum __FreeBSD_version = 1400000;
else version (FreeBSD_13) enum __FreeBSD_version = 1301000;
else version (FreeBSD_12) enum __FreeBSD_version = 1203000;
else version (FreeBSD_11) enum __FreeBSD_version = 1104000;
else version (FreeBSD_10) enum __FreeBSD_version = 1004000;
else version (FreeBSD_9)  enum __FreeBSD_version = 903000;
else version (FreeBSD_8)  enum __FreeBSD_version = 804000;
else static assert(false, "Unsupported version of FreeBSD");

// First version of FreeBSD to support 64-bit stat buffer.
enum INO64_FIRST = 1200031;

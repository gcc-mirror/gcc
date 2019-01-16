/**
 * D header file for Darwin
 *
 * Authors: Martin Nowak
 */
module core.sys.darwin.sys.cdefs;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):

public import core.sys.posix.config;

// http://www.opensource.apple.com/source/xnu/xnu-2422.115.4/bsd/sys/cdefs.h
enum _DARWIN_C_SOURCE = true;

enum __DARWIN_C_FULL = 900_000L;
enum __DARWIN_C_LEVEL = __DARWIN_C_FULL;

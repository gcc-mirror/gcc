module core.sys.darwin.fcntl;

public import core.sys.posix.fcntl;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):
extern (C):
nothrow:
@nogc:
@system:

enum F_FULLFSYNC = 51;

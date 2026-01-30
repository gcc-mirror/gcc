/**
 * D header file for POSIX.
 *
 * $(LINK2 https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/endian.h.html posix endian.h)
 *
 * Standards: The Open Group Base Specifications Issue 8, IEEE Std 1003.1, 2024 Edition
 */
module core.sys.posix.endian;

version (Posix):
nothrow:
@safe:
@nogc:

import core.bitop;

enum LITTLE_ENDIAN = 1234;
enum BIG_ENDIAN = 4321;
enum PDP_ENDIAN = 3412;

version (LittleEndian)
{
    enum BYTE_ORDER = LITTLE_ENDIAN;

    pragma(inline, true):
    ushort htobe16(ushort x) => core.bitop.byteswap(x);
    ushort htole16(ushort x) => x;
    ushort be16toh(ushort x) => core.bitop.byteswap(x);
    ushort le16toh(ushort x) => x;

    uint htobe32(uint x) => core.bitop.bswap(x);
    uint htole32(uint x) => x;
    uint be32toh(uint x) => core.bitop.bswap(x);
    uint le32toh(uint x) => x;

    ulong htobe64(ulong x) => core.bitop.bswap(x);
    ulong htole64(ulong x) => x;
    ulong be64toh(ulong x) => core.bitop.bswap(x);
    ulong le64toh(ulong x) => x;
}
else
{
    enum BYTE_ORDER = BIG_ENDIAN;

    pragma(inline, true):
    ushort htobe16(ushort x) => x;
    ushort htole16(ushort x) => core.bitop.byteswap(x);
    ushort be16toh(ushort x) => x;
    ushort le16toh(ushort x) => core.bitop.byteswap(x);

    uint htobe32(uint x) => x;
    uint htole32(uint x) => core.bitop.bswap(x);
    uint be32toh(uint x) => x;
    uint le32toh(uint x) => core.bitop.bswap(x);

    ulong htobe64(ulong x) => x;
    ulong htole64(ulong x) => core.bitop.bswap(x);
    ulong be64toh(ulong x) => x;
    ulong le64toh(ulong x) => core.bitop.bswap(x);
}

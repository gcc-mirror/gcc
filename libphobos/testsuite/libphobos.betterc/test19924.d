/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=19924

import core.bitop;

extern(C) void main()
{
    uint a = 0x01_23_45_67;
    a = bswap(a);
    assert(a == 0x67_45_23_01);

    ulong b = 0x01_23_45_67_89_ab_cd_ef;
    b = bswap(b);
    assert(b == 0xef_cd_ab_89_67_45_23_01);
}

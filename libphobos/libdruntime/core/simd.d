// Written in the D programming language.

/**
 * Builtin SIMD intrinsics
 *
 * Source: $(DRUNTIMESRC core/_simd.d)
 *
 * Copyright: Copyright Digital Mars 2012-2020
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright),
 * Source:    $(DRUNTIMESRC core/_simd.d)
 */

module core.simd;

pure:
nothrow:
@safe:
@nogc:

/*******************************
 * Create a vector type.
 *
 * Parameters:
 *      T = one of double[2], float[4], void[16], byte[16], ubyte[16],
 *      short[8], ushort[8], int[4], uint[4], long[2], ulong[2].
 *      For 256 bit vectors,
 *      one of double[4], float[8], void[32], byte[32], ubyte[32],
 *      short[16], ushort[16], int[8], uint[8], long[4], ulong[4]
 */

template Vector(T)
{
    /* __vector is compiler magic, hide it behind a template.
     * The compiler will reject T's that don't work.
     */
    alias __vector(T) Vector;
}

/* Handy aliases
 */
static if (is(Vector!(void[8])))    alias Vector!(void[8])    void8;        ///
static if (is(Vector!(double[1])))  alias Vector!(double[1])  double1;      ///
static if (is(Vector!(float[2])))   alias Vector!(float[2])   float2;       ///
static if (is(Vector!(byte[8])))    alias Vector!(byte[8])    byte8;        ///
static if (is(Vector!(ubyte[8])))   alias Vector!(ubyte[8])   ubyte8;       ///
static if (is(Vector!(short[4])))   alias Vector!(short[4])   short4;       ///
static if (is(Vector!(ushort[4])))  alias Vector!(ushort[4])  ushort4;      ///
static if (is(Vector!(int[2])))     alias Vector!(int[2])     int2;         ///
static if (is(Vector!(uint[2])))    alias Vector!(uint[2])    uint2;        ///
static if (is(Vector!(long[1])))    alias Vector!(long[1])    long1;        ///
static if (is(Vector!(ulong[1])))   alias Vector!(ulong[1])   ulong1;       ///

static if (is(Vector!(void[16])))   alias Vector!(void[16])   void16;       ///
static if (is(Vector!(double[2])))  alias Vector!(double[2])  double2;      ///
static if (is(Vector!(float[4])))   alias Vector!(float[4])   float4;       ///
static if (is(Vector!(byte[16])))   alias Vector!(byte[16])   byte16;       ///
static if (is(Vector!(ubyte[16])))  alias Vector!(ubyte[16])  ubyte16;      ///
static if (is(Vector!(short[8])))   alias Vector!(short[8])   short8;       ///
static if (is(Vector!(ushort[8])))  alias Vector!(ushort[8])  ushort8;      ///
static if (is(Vector!(int[4])))     alias Vector!(int[4])     int4;         ///
static if (is(Vector!(uint[4])))    alias Vector!(uint[4])    uint4;        ///
static if (is(Vector!(long[2])))    alias Vector!(long[2])    long2;        ///
static if (is(Vector!(ulong[2])))   alias Vector!(ulong[2])   ulong2;       ///

static if (is(Vector!(void[32])))   alias Vector!(void[32])   void32;       ///
static if (is(Vector!(double[4])))  alias Vector!(double[4])  double4;      ///
static if (is(Vector!(float[8])))   alias Vector!(float[8])   float8;       ///
static if (is(Vector!(byte[32])))   alias Vector!(byte[32])   byte32;       ///
static if (is(Vector!(ubyte[32])))  alias Vector!(ubyte[32])  ubyte32;      ///
static if (is(Vector!(short[16])))  alias Vector!(short[16])  short16;      ///
static if (is(Vector!(ushort[16]))) alias Vector!(ushort[16]) ushort16;     ///
static if (is(Vector!(int[8])))     alias Vector!(int[8])     int8;         ///
static if (is(Vector!(uint[8])))    alias Vector!(uint[8])    uint8;        ///
static if (is(Vector!(long[4])))    alias Vector!(long[4])    long4;        ///
static if (is(Vector!(ulong[4])))   alias Vector!(ulong[4])   ulong4;       ///

static if (is(Vector!(void[64])))   alias Vector!(void[64])   void64;       ///
static if (is(Vector!(double[8])))  alias Vector!(double[8])  double8;      ///
static if (is(Vector!(float[16])))  alias Vector!(float[16])  float16;      ///
static if (is(Vector!(byte[64])))   alias Vector!(byte[64])   byte64;       ///
static if (is(Vector!(ubyte[64])))  alias Vector!(ubyte[64])  ubyte64;      ///
static if (is(Vector!(short[32])))  alias Vector!(short[32])  short32;      ///
static if (is(Vector!(ushort[32]))) alias Vector!(ushort[32]) ushort32;     ///
static if (is(Vector!(int[16])))    alias Vector!(int[16])    int16;        ///
static if (is(Vector!(uint[16])))   alias Vector!(uint[16])   uint16;       ///
static if (is(Vector!(long[8])))    alias Vector!(long[8])    long8;        ///
static if (is(Vector!(ulong[8])))   alias Vector!(ulong[8])   ulong8;       ///

version (D_SIMD)
{
    /** XMM opcodes that conform to the following:
    *
    *  opcode xmm1,xmm2/mem
    *
    * and do not have side effects (i.e. do not write to memory).
    */
    enum XMM
    {
        ADDSS = 0xF30F58,
        ADDSD = 0xF20F58,
        ADDPS = 0x000F58,
        ADDPD = 0x660F58,
        PADDB = 0x660FFC,
        PADDW = 0x660FFD,
        PADDD = 0x660FFE,
        PADDQ = 0x660FD4,

        SUBSS = 0xF30F5C,
        SUBSD = 0xF20F5C,
        SUBPS = 0x000F5C,
        SUBPD = 0x660F5C,
        PSUBB = 0x660FF8,
        PSUBW = 0x660FF9,
        PSUBD = 0x660FFA,
        PSUBQ = 0x660FFB,

        MULSS = 0xF30F59,
        MULSD = 0xF20F59,
        MULPS = 0x000F59,
        MULPD = 0x660F59,
        PMULLW = 0x660FD5,

        DIVSS = 0xF30F5E,
        DIVSD = 0xF20F5E,
        DIVPS = 0x000F5E,
        DIVPD = 0x660F5E,

        PAND  = 0x660FDB,
        POR   = 0x660FEB,

        UCOMISS = 0x000F2E,
        UCOMISD = 0x660F2E,

        XORPS = 0x000F57,
        XORPD = 0x660F57,

        // Use STO and LOD instead of MOV to distinguish the direction
        // (Destination is first operand, Source is second operand)
        STOSS  = 0xF30F11,        /// MOVSS xmm1/m32, xmm2
        STOSD  = 0xF20F11,        /// MOVSD xmm1/m64, xmm2
        STOAPS = 0x000F29,        /// MOVAPS xmm2/m128, xmm1
        STOAPD = 0x660F29,        /// MOVAPD xmm2/m128, xmm1
        STODQA = 0x660F7F,        /// MOVDQA xmm2/m128, xmm1
        STOD   = 0x660F7E,        /// MOVD reg/mem64, xmm   66 0F 7E /r
        STOQ   = 0x660FD6,        /// MOVQ xmm2/m64, xmm1

        LODSS  = 0xF30F10,        /// MOVSS xmm1, xmm2/m32
        LODSD  = 0xF20F10,        /// MOVSD xmm1, xmm2/m64
        LODAPS = 0x000F28,        /// MOVAPS xmm1, xmm2/m128
        LODAPD = 0x660F28,        /// MOVAPD xmm1, xmm2/m128
        LODDQA = 0x660F6F,        /// MOVDQA xmm1, xmm2/m128
        LODD   = 0x660F6E,        /// MOVD xmm, reg/mem64   66 0F 6E /r
        LODQ   = 0xF30F7E,        /// MOVQ xmm1, xmm2/m64

        LODDQU   = 0xF30F6F,      /// MOVDQU xmm1, xmm2/mem128  F3 0F 6F /r
        STODQU   = 0xF30F7F,      /// MOVDQU xmm1/mem128, xmm2  F3 0F 7F /r
        MOVDQ2Q  = 0xF20FD6,      /// MOVDQ2Q mmx, xmm          F2 0F D6 /r
        MOVHLPS  = 0x0F12,        /// MOVHLPS xmm1, xmm2        0F 12 /r
        LODHPD   = 0x660F16,      /// MOVHPD xmm1, m64
        STOHPD   = 0x660F17,      /// MOVHPD mem64, xmm1        66 0F 17 /r
        LODHPS   = 0x0F16,        /// MOVHPS xmm1, m64
        STOHPS   = 0x0F17,        /// MOVHPS m64, xmm1
        MOVLHPS  = 0x0F16,        /// MOVLHPS xmm1, xmm2
        LODLPD   = 0x660F12,      /// MOVLPD xmm1, m64
        STOLPD   = 0x660F13,      /// MOVLPD m64, xmm1
        LODLPS   = 0x0F12,        /// MOVLPS xmm1, m64
        STOLPS   = 0x0F13,        /// MOVLPS m64, xmm1
        MOVMSKPD = 0x660F50,      /// MOVMSKPD reg, xmm
        MOVMSKPS = 0x0F50,        /// MOVMSKPS reg, xmm
        MOVNTDQ  = 0x660FE7,      /// MOVNTDQ m128, xmm1
        MOVNTI   = 0x0FC3,        /// MOVNTI m32, r32
        MOVNTPD  = 0x660F2B,      /// MOVNTPD m128, xmm1
        MOVNTPS  = 0x0F2B,        /// MOVNTPS m128, xmm1
        MOVNTQ   = 0x0FE7,        /// MOVNTQ m64, mm
        MOVQ2DQ  = 0xF30FD6,      /// MOVQ2DQ
        LODUPD   = 0x660F10,      /// MOVUPD xmm1, xmm2/m128
        STOUPD   = 0x660F11,      /// MOVUPD xmm2/m128, xmm1
        LODUPS   = 0x0F10,        /// MOVUPS xmm1, xmm2/m128
        STOUPS   = 0x0F11,        /// MOVUPS xmm2/m128, xmm1

        PACKSSDW = 0x660F6B,
        PACKSSWB = 0x660F63,
        PACKUSWB = 0x660F67,
        PADDSB = 0x660FEC,
        PADDSW = 0x660FED,
        PADDUSB = 0x660FDC,
        PADDUSW = 0x660FDD,
        PANDN = 0x660FDF,
        PCMPEQB = 0x660F74,
        PCMPEQD = 0x660F76,
        PCMPEQW = 0x660F75,
        PCMPGTB = 0x660F64,
        PCMPGTD = 0x660F66,
        PCMPGTW = 0x660F65,
        PMADDWD = 0x660FF5,
        PSLLW = 0x660FF1,
        PSLLD = 0x660FF2,
        PSLLQ = 0x660FF3,
        PSRAW = 0x660FE1,
        PSRAD = 0x660FE2,
        PSRLW = 0x660FD1,
        PSRLD = 0x660FD2,
        PSRLQ = 0x660FD3,
        PSUBSB = 0x660FE8,
        PSUBSW = 0x660FE9,
        PSUBUSB = 0x660FD8,
        PSUBUSW = 0x660FD9,
        PUNPCKHBW = 0x660F68,
        PUNPCKHDQ = 0x660F6A,
        PUNPCKHWD = 0x660F69,
        PUNPCKLBW = 0x660F60,
        PUNPCKLDQ = 0x660F62,
        PUNPCKLWD = 0x660F61,
        PXOR = 0x660FEF,
        ANDPD = 0x660F54,
        ANDPS = 0x0F54,
        ANDNPD = 0x660F55,
        ANDNPS = 0x0F55,
        CMPPS = 0x0FC2,
        CMPPD = 0x660FC2,
        CMPSD = 0xF20FC2,
        CMPSS = 0xF30FC2,
        COMISD = 0x660F2F,
        COMISS = 0x0F2F,
        CVTDQ2PD = 0xF30FE6,
        CVTDQ2PS = 0x0F5B,
        CVTPD2DQ = 0xF20FE6,
        CVTPD2PI = 0x660F2D,
        CVTPD2PS = 0x660F5A,
        CVTPI2PD = 0x660F2A,
        CVTPI2PS = 0x0F2A,
        CVTPS2DQ = 0x660F5B,
        CVTPS2PD = 0x0F5A,
        CVTPS2PI = 0x0F2D,
        CVTSD2SI = 0xF20F2D,
        CVTSD2SS = 0xF20F5A,
        CVTSI2SD = 0xF20F2A,
        CVTSI2SS = 0xF30F2A,
        CVTSS2SD = 0xF30F5A,
        CVTSS2SI = 0xF30F2D,
        CVTTPD2PI = 0x660F2C,
        CVTTPD2DQ = 0x660FE6,
        CVTTPS2DQ = 0xF30F5B,
        CVTTPS2PI = 0x0F2C,
        CVTTSD2SI = 0xF20F2C,
        CVTTSS2SI = 0xF30F2C,
        MASKMOVDQU = 0x660FF7,
        MASKMOVQ = 0x0FF7,
        MAXPD = 0x660F5F,
        MAXPS = 0x0F5F,
        MAXSD = 0xF20F5F,
        MAXSS = 0xF30F5F,
        MINPD = 0x660F5D,
        MINPS = 0x0F5D,
        MINSD = 0xF20F5D,
        MINSS = 0xF30F5D,
        ORPD = 0x660F56,
        ORPS = 0x0F56,
        PAVGB = 0x660FE0,
        PAVGW = 0x660FE3,
        PMAXSW = 0x660FEE,
        //PINSRW = 0x660FC4,
        PMAXUB = 0x660FDE,
        PMINSW = 0x660FEA,
        PMINUB = 0x660FDA,
        //PMOVMSKB = 0x660FD7,
        PMULHUW = 0x660FE4,
        PMULHW = 0x660FE5,
        PMULUDQ = 0x660FF4,
        PSADBW = 0x660FF6,
        PUNPCKHQDQ = 0x660F6D,
        PUNPCKLQDQ = 0x660F6C,
        RCPPS = 0x0F53,
        RCPSS = 0xF30F53,
        RSQRTPS = 0x0F52,
        RSQRTSS = 0xF30F52,
        SQRTPD = 0x660F51,
        SHUFPD = 0x660FC6,
        SHUFPS = 0x0FC6,
        SQRTPS = 0x0F51,
        SQRTSD = 0xF20F51,
        SQRTSS = 0xF30F51,
        UNPCKHPD = 0x660F15,
        UNPCKHPS = 0x0F15,
        UNPCKLPD = 0x660F14,
        UNPCKLPS = 0x0F14,

        PSHUFD = 0x660F70,
        PSHUFHW = 0xF30F70,
        PSHUFLW = 0xF20F70,
        PSHUFW = 0x0F70,
        PSLLDQ = 0x07660F73,
        PSRLDQ = 0x03660F73,

        //PREFETCH = 0x0F18,

        // SSE3 Pentium 4 (Prescott)

        ADDSUBPD = 0x660FD0,
        ADDSUBPS = 0xF20FD0,
        HADDPD   = 0x660F7C,
        HADDPS   = 0xF20F7C,
        HSUBPD   = 0x660F7D,
        HSUBPS   = 0xF20F7D,
        MOVDDUP  = 0xF20F12,
        MOVSHDUP = 0xF30F16,
        MOVSLDUP = 0xF30F12,
        LDDQU    = 0xF20FF0,
        MONITOR  = 0x0F01C8,
        MWAIT    = 0x0F01C9,

        // SSSE3
        PALIGNR = 0x660F3A0F,
        PHADDD = 0x660F3802,
        PHADDW = 0x660F3801,
        PHADDSW = 0x660F3803,
        PABSB = 0x660F381C,
        PABSD = 0x660F381E,
        PABSW = 0x660F381D,
        PSIGNB = 0x660F3808,
        PSIGND = 0x660F380A,
        PSIGNW = 0x660F3809,
        PSHUFB = 0x660F3800,
        PMADDUBSW = 0x660F3804,
        PMULHRSW = 0x660F380B,
        PHSUBD = 0x660F3806,
        PHSUBW = 0x660F3805,
        PHSUBSW = 0x660F3807,

        // SSE4.1

        BLENDPD   = 0x660F3A0D,
        BLENDPS   = 0x660F3A0C,
        BLENDVPD  = 0x660F3815,
        BLENDVPS  = 0x660F3814,
        DPPD      = 0x660F3A41,
        DPPS      = 0x660F3A40,
        EXTRACTPS = 0x660F3A17,
        INSERTPS  = 0x660F3A21,
        MPSADBW   = 0x660F3A42,
        PBLENDVB  = 0x660F3810,
        PBLENDW   = 0x660F3A0E,
        PEXTRD    = 0x660F3A16,
        PEXTRQ    = 0x660F3A16,
        PINSRB    = 0x660F3A20,
        PINSRD    = 0x660F3A22,
        PINSRQ    = 0x660F3A22,

        MOVNTDQA = 0x660F382A,
        PACKUSDW = 0x660F382B,
        PCMPEQQ = 0x660F3829,
        PEXTRB = 0x660F3A14,
        PHMINPOSUW = 0x660F3841,
        PMAXSB = 0x660F383C,
        PMAXSD = 0x660F383D,
        PMAXUD = 0x660F383F,
        PMAXUW = 0x660F383E,
        PMINSB = 0x660F3838,
        PMINSD = 0x660F3839,
        PMINUD = 0x660F383B,
        PMINUW = 0x660F383A,
        PMOVSXBW = 0x660F3820,
        PMOVSXBD = 0x660F3821,
        PMOVSXBQ = 0x660F3822,
        PMOVSXWD = 0x660F3823,
        PMOVSXWQ = 0x660F3824,
        PMOVSXDQ = 0x660F3825,
        PMOVZXBW = 0x660F3830,
        PMOVZXBD = 0x660F3831,
        PMOVZXBQ = 0x660F3832,
        PMOVZXWD = 0x660F3833,
        PMOVZXWQ = 0x660F3834,
        PMOVZXDQ = 0x660F3835,
        PMULDQ   = 0x660F3828,
        PMULLD   = 0x660F3840,
        PTEST    = 0x660F3817,

        ROUNDPD = 0x660F3A09,
        ROUNDPS = 0x660F3A08,
        ROUNDSD = 0x660F3A0B,
        ROUNDSS = 0x660F3A0A,

        // SSE4.2
        PCMPESTRI  = 0x660F3A61,
        PCMPESTRM  = 0x660F3A60,
        PCMPISTRI  = 0x660F3A63,
        PCMPISTRM  = 0x660F3A62,
        PCMPGTQ    = 0x660F3837,
        //CRC32

        // SSE4a (AMD only)
        // EXTRQ,INSERTQ,MOVNTSD,MOVNTSS

        // POPCNT and LZCNT (have their own CPUID bits)
        POPCNT     = 0xF30FB8,
        // LZCNT
    }

    /**
    * Generate two operand instruction with XMM 128 bit operands.
    *
    * This is a compiler magic function - it doesn't behave like
    * regular D functions.
    *
    * Parameters:
    *      opcode = any of the XMM opcodes; it must be a compile time constant
    *      op1    = first operand
    *      op2    = second operand
    * Returns:
    *      result of opcode
    */
    pure @safe void16 __simd(XMM opcode, void16 op1, void16 op2);

    ///
    unittest
    {
        float4 a;
        a = cast(float4)__simd(XMM.PXOR, a, a);
    }

    /**
    * Unary SIMD instructions.
    */
    pure @safe void16 __simd(XMM opcode, void16 op1);
    pure @safe void16 __simd(XMM opcode, double d);   ///
    pure @safe void16 __simd(XMM opcode, float f);    ///

    ///
    unittest
    {
        float4 a;
        a = cast(float4)__simd(XMM.LODSS, a);
    }

    /****
    * For instructions:
    * CMPPD, CMPSS, CMPSD, CMPPS,
    * PSHUFD, PSHUFHW, PSHUFLW,
    * BLENDPD, BLENDPS, DPPD, DPPS,
    * MPSADBW, PBLENDW,
    * ROUNDPD, ROUNDPS, ROUNDSD, ROUNDSS
    * Parameters:
    *      opcode = any of the above XMM opcodes; it must be a compile time constant
    *      op1    = first operand
    *      op2    = second operand
    *      imm8   = third operand; must be a compile time constant
    * Returns:
    *      result of opcode
    */
    pure @safe void16 __simd(XMM opcode, void16 op1, void16 op2, ubyte imm8);

    ///
    unittest
    {
        float4 a;
        a = cast(float4)__simd(XMM.CMPPD, a, a, 0x7A);
    }

    /***
    * For instructions with the imm8 version:
    * PSLLD, PSLLQ, PSLLW, PSRAD, PSRAW, PSRLD, PSRLQ, PSRLW,
    * PSRLDQ, PSLLDQ
    * Parameters:
    *      opcode = any of the XMM opcodes; it must be a compile time constant
    *      op1    = first operand
    *      imm8   = second operand; must be a compile time constant
    * Returns:
    *      result of opcode
    */
    pure @safe void16 __simd_ib(XMM opcode, void16 op1, ubyte imm8);

    ///
    unittest
    {
        float4 a;
        a = cast(float4) __simd_ib(XMM.PSRLQ, a, 0x7A);
    }

    /*****
    * For "store" operations of the form:
    *    op1 op= op2
    * Returns:
    *    op2
    * These cannot be marked as pure, as semantic() doesn't check them.
    */
    @safe void16 __simd_sto(XMM opcode, void16 op1, void16 op2);
    @safe void16 __simd_sto(XMM opcode, double op1, void16 op2); ///
    @safe void16 __simd_sto(XMM opcode, float op1, void16 op2);  ///

    ///
    unittest
    {
        void16 a;
        float f = 1;
        double d = 1;

        cast(void)__simd_sto(XMM.STOUPS, a, a);
        cast(void)__simd_sto(XMM.STOUPS, f, a);
        cast(void)__simd_sto(XMM.STOUPS, d, a);
    }

    /* The following use overloading to ensure correct typing.
    * Compile with inlining on for best performance.
    */

    pure @safe short8 pcmpeq()(short8 v1, short8 v2)
    {
        return cast(short8)__simd(XMM.PCMPEQW, v1, v2);
    }

    pure @safe ushort8 pcmpeq()(ushort8 v1, ushort8 v2)
    {
        return cast(ushort8)__simd(XMM.PCMPEQW, v1, v2);
    }

    /*********************
    * Emit prefetch instruction.
    * Params:
    *    address = address to be prefetched
    *    writeFetch = true for write fetch, false for read fetch
    *    locality = 0..3 (0 meaning least local, 3 meaning most local)
    * Note:
    *    The Intel mappings are:
    *    $(TABLE
    *    $(THEAD writeFetch, locality, Instruction)
    *    $(TROW false, 0, prefetchnta)
    *    $(TROW false, 1, prefetch2)
    *    $(TROW false, 2, prefetch1)
    *    $(TROW false, 3, prefetch0)
    *    $(TROW true, 0, prefetchw)
    *    $(TROW true, 1, prefetchw)
    *    $(TROW true, 2, prefetchw)
    *    $(TROW true, 3, prefetchw)
    *    )
    */
    void prefetch(bool writeFetch, ubyte locality)(const(void)* address)
    {
        static if (writeFetch)
            __prefetch(address, 4);
        else static if (locality < 4)
            __prefetch(address, 3 - locality);
        else
            static assert(0, "0..3 expected for locality");
    }

    private void __prefetch(const(void*) address, ubyte encoding);

    /*************************************
    * Load unaligned vector from address.
    * This is a compiler intrinsic.
    * Params:
    *    p = pointer to vector
    * Returns:
    *    vector
    */

    V loadUnaligned(V)(const V* p)
        if (is(V == void16) ||
            is(V == byte16) ||
            is(V == ubyte16) ||
            is(V == short8) ||
            is(V == ushort8) ||
            is(V == int4) ||
            is(V == uint4) ||
            is(V == long2) ||
            is(V == ulong2) ||
            is(V == double2) ||
            is(V == float4))
    {
        pragma(inline, true);
        static if (is(V == double2))
            return cast(V)__simd(XMM.LODUPD, *cast(const void16*)p);
        else static if (is(V == float4))
            return cast(V)__simd(XMM.LODUPS, *cast(const void16*)p);
        else
            return cast(V)__simd(XMM.LODDQU, *cast(const void16*)p);
    }

    @system
    unittest
    {
        // Memory to load into the vector:
        // Should have enough data to test all 16-byte alignments, and still
        // have room for a 16-byte vector
        ubyte[32] data;
        foreach (i; 0..data.length)
        {
            data[i] = cast(ubyte)i;
        }

        // to test all alignments from 1 ~ 16
        foreach (i; 0..16)
        {
            ubyte* d = &data[i];

            void test(T)()
            {
                // load the data
                T v = loadUnaligned(cast(T*)d);

                // check that the data was loaded correctly
                ubyte* ptrToV = cast(ubyte*)&v;
                foreach (j; 0..T.sizeof)
                {
                    assert(ptrToV[j] == d[j]);
                }
            }

            test!void16();
            test!byte16();
            test!ubyte16();
            test!short8();
            test!ushort8();
            test!int4();
            test!uint4();
            test!long2();
            test!ulong2();
            test!double2();
            test!float4();
        }
    }

    /*************************************
    * Store vector to unaligned address.
    * This is a compiler intrinsic.
    * Params:
    *    p = pointer to vector
    *    value = value to store
    * Returns:
    *    value
    */

    V storeUnaligned(V)(V* p, V value)
        if (is(V == void16) ||
            is(V == byte16) ||
            is(V == ubyte16) ||
            is(V == short8) ||
            is(V == ushort8) ||
            is(V == int4) ||
            is(V == uint4) ||
            is(V == long2) ||
            is(V == ulong2) ||
            is(V == double2) ||
            is(V == float4))
    {
        pragma(inline, true);
        static if (is(V == double2))
            return cast(V)__simd_sto(XMM.STOUPD, *cast(void16*)p, value);
        else static if (is(V == float4))
            return cast(V)__simd_sto(XMM.STOUPS, *cast(void16*)p, value);
        else
            return cast(V)__simd_sto(XMM.STODQU, *cast(void16*)p, value);
    }

    @system
    unittest
    {
        // Memory to store the vector to:
        // Should have enough data to test all 16-byte alignments, and still
        // have room for a 16-byte vector
        ubyte[32] data;

        // to test all alignments from 1 ~ 16
        foreach (i; 0..16)
        {
            ubyte* d = &data[i];

            void test(T)()
            {
                T v;

                // populate v` with data
                ubyte* ptrToV = cast(ubyte*)&v;
                foreach (j; 0..T.sizeof)
                {
                    ptrToV[j] = cast(ubyte)j;
                }

                // store `v` to location pointed to by `d`
                storeUnaligned(cast(T*)d, v);

                // check that the the data was stored correctly
                foreach (j; 0..T.sizeof)
                {
                    assert(ptrToV[j] == d[j]);
                }
            }

            test!void16();
            test!byte16();
            test!ubyte16();
            test!short8();
            test!ushort8();
            test!int4();
            test!uint4();
            test!long2();
            test!ulong2();
            test!double2();
            test!float4();
        }
    }
}

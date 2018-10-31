/** Optimised asm arbitrary precision arithmetic ('bignum')
 * routines for X86 processors.
 *
 * All functions operate on arrays of uints, stored LSB first.
 * If there is a destination array, it will be the first parameter.
 * Currently, all of these functions are subject to change, and are
 * intended for internal use only.
 * The symbol [#] indicates an array of machine words which is to be
 * interpreted as a multi-byte number.
 */

/*          Copyright Don Clugston 2008 - 2010.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
/**
 * In simple terms, there are 3 modern x86 microarchitectures:
 * (a) the P6 family (Pentium Pro, PII, PIII, PM, Core), produced by Intel;
 * (b) the K6, Athlon, and AMD64 families, produced by AMD; and
 * (c) the Pentium 4, produced by Marketing.
 *
 * This code has been optimised for the Intel P6 family.
 * Generally the code remains near-optimal for Intel Core2/Corei7, after
 * translating EAX-> RAX, etc, since all these CPUs use essentially the same
 * pipeline, and are typically limited by memory access.
 * The code uses techniques described in Agner Fog's superb Pentium manuals
 * available at www.agner.org.
 * Not optimised for AMD, which can do two memory loads per cycle (Intel
 * CPUs can only do one). Despite this, performance is superior on AMD.
 * Performance is dreadful on P4.
 *
 *  Timing results (cycles per int)
 *              --Intel Pentium--  --AMD--
 *              PM     P4   Core2   K7
 *  +,-         2.25  15.6   2.25   1.5
 *  <<,>>       2.0    6.6   2.0    5.0
 *    (<< MMX)  1.7    5.3   1.5    1.2
 *  *           5.0   15.0   4.0    4.3
 *  mulAdd      5.7   19.0   4.9    4.0
 *  div        30.0   32.0  32.0   22.4
 *  mulAcc(32)  6.5   20.0   5.4    4.9
 *
 * mulAcc(32) is multiplyAccumulate() for a 32*32 multiply. Thus it includes
 * function call overhead.
 * The timing for Div is quite unpredictable, but it's probably too slow
 * to be useful. On 64-bit processors, these times should
 * halve if run in 64-bit mode, except for the MMX functions.
 */

module std.internal.math.biguintx86;

@system:
pure:
nothrow:

/*
  Naked asm is used throughout, because:
  (a) it frees up the EBP register
  (b) compiler bugs prevent the use of .ptr when a frame pointer is used.
*/

version (D_InlineAsm_X86)
{

private:

/* Duplicate string s, with n times, substituting index for '@'.
 *
 * Each instance of '@' in s is replaced by 0,1,...n-1. This is a helper
 * function for some of the asm routines.
 */
string indexedLoopUnroll(int n, string s) pure @safe
{
    string u;
    for (int i = 0; i<n; ++i)
    {
        string nstr= (i>9 ? ""~ cast(char)('0'+i/10) : "") ~ cast(char)('0' + i%10);

        int last = 0;
        for (int j = 0; j<s.length; ++j)
        {
            if (s[j]=='@')
            {
                u ~= s[last .. j] ~ nstr;
                last = j+1;
            }
        }
        if (last<s.length) u = u ~ s[last..$];

    }
    return u;
}
@safe unittest
{
    assert(indexedLoopUnroll(3, "@*23;")=="0*23;1*23;2*23;");
}

public:

alias BigDigit = uint; // A Bignum is an array of BigDigits. Usually the machine word size.

// Limits for when to switch between multiplication algorithms.
enum : int { KARATSUBALIMIT = 18 }; // Minimum value for which Karatsuba is worthwhile.
enum : int { KARATSUBASQUARELIMIT=26 }; // Minimum value for which square Karatsuba is worthwhile

/** Multi-byte addition or subtraction
 *    dest[#] = src1[#] + src2[#] + carry (0 or 1).
 * or dest[#] = src1[#] - src2[#] - carry (0 or 1).
 * Returns carry or borrow (0 or 1).
 * Set op == '+' for addition, '-' for subtraction.
 */
uint multibyteAddSub(char op)(uint[] dest, const uint [] src1, const uint []
        src2, uint carry) pure
{
    // Timing:
    // Pentium M: 2.25/int
    // P6 family, Core2 have a partial flags stall when reading the carry flag in
    // an ADC, SBB operation after an operation such as INC or DEC which
    // modifies some, but not all, flags. We avoid this by storing carry into
    // a resister (AL), and restoring it after the branch.

    enum { LASTPARAM = 4*4 } // 3* pushes + return address.
    asm pure nothrow {
        naked;
        push EDI;
        push EBX;
        push ESI;
        mov ECX, [ESP + LASTPARAM + 4*4]; // dest.length;
        mov EDX, [ESP + LASTPARAM + 3*4]; // src1.ptr
        mov ESI, [ESP + LASTPARAM + 1*4]; // src2.ptr
        mov EDI, [ESP + LASTPARAM + 5*4]; // dest.ptr
             // Carry is in EAX
        // Count UP to zero (from -len) to minimize loop overhead.
        lea EDX, [EDX + 4*ECX]; // EDX = end of src1.
        lea ESI, [ESI + 4*ECX]; // EBP = end of src2.
        lea EDI, [EDI + 4*ECX]; // EDI = end of dest.

        neg ECX;
        add ECX, 8;
        jb L2;  // if length < 8 , bypass the unrolled loop.
L_unrolled:
        shr AL, 1; // get carry from EAX
    }
    mixin(" asm pure nothrow {"
        ~ indexedLoopUnroll( 8,
        "mov EAX, [@*4-8*4+EDX+ECX*4];"
        ~ ( op == '+' ? "adc" : "sbb" ) ~ " EAX, [@*4-8*4+ESI+ECX*4];"
        ~ "mov [@*4-8*4+EDI+ECX*4], EAX;")
        ~ "}");
    asm pure nothrow {
        setc AL; // save carry
        add ECX, 8;
        ja L_unrolled;
L2:     // Do the residual 1 .. 7 ints.

        sub ECX, 8;
        jz done;
L_residual:
        shr AL, 1; // get carry from EAX
    }
    mixin(" asm pure nothrow {"
        ~ indexedLoopUnroll( 1,
        "mov EAX, [@*4+EDX+ECX*4];"
        ~ ( op == '+' ? "adc" : "sbb" ) ~ " EAX, [@*4+ESI+ECX*4];"
        ~ "mov [@*4+EDI+ECX*4], EAX;") ~ "}");
    asm pure nothrow {
        setc AL; // save carry
        add ECX, 1;
        jnz L_residual;
done:
        and EAX, 1; // make it O or 1.
        pop ESI;
        pop EBX;
        pop EDI;
        ret 6*4;
    }
}

@system unittest
{
    uint [] a = new uint[40];
    uint [] b = new uint[40];
    uint [] c = new uint[40];
    for (int i=0; i<a.length; ++i)
    {
        if (i&1) a[i]=0x8000_0000 + i;
        else a[i]=i;
        b[i]= 0x8000_0003;
    }
    c[19]=0x3333_3333;
    uint carry = multibyteAddSub!('+')(c[0 .. 18], a[0 .. 18], b[0 .. 18], 0);
    assert(carry == 1);
    assert(c[0]==0x8000_0003);
    assert(c[1]==4);
    assert(c[19]==0x3333_3333); // check for overrun
    for (int i=0; i<a.length; ++i)
    {
        a[i]=b[i]=c[i]=0;
    }
    a[8]=0x048D159E;
    b[8]=0x048D159E;
    a[10]=0x1D950C84;
    b[10]=0x1D950C84;
    a[5] =0x44444444;
    carry = multibyteAddSub!('-')(a[0 .. 12], a[0 .. 12], b[0 .. 12], 0);
    assert(a[11]==0);
    for (int i=0; i<10; ++i) if (i != 5) assert(a[i]==0);

    for (int q=3; q<36;++q)
    {
        for (int i=0; i<a.length; ++i)
        {
            a[i]=b[i]=c[i]=0;
        }
        a[q-2]=0x040000;
        b[q-2]=0x040000;
       carry = multibyteAddSub!('-')(a[0 .. q], a[0 .. q], b[0 .. q], 0);
       assert(a[q-2]==0);
    }
}

/** dest[#] += carry, or dest[#] -= carry.
 *  op must be '+' or '-'
 *  Returns final carry or borrow (0 or 1)
 */
uint multibyteIncrementAssign(char op)(uint[] dest, uint carry) pure
{
    enum { LASTPARAM = 1*4 } // 0* pushes + return address.
    asm pure nothrow {
        naked;
        mov ECX, [ESP + LASTPARAM + 0*4]; // dest.length;
        mov EDX, [ESP + LASTPARAM + 1*4]; // dest.ptr
        // EAX  = carry
L1: ;
    }
    static if (op=='+')
        asm pure nothrow { add [EDX], EAX; }
    else
        asm pure nothrow { sub [EDX], EAX; }
    asm pure nothrow {
        mov EAX, 1;
        jnc L2;
        add EDX, 4;
        dec ECX;
        jnz L1;
        mov EAX, 2;
L2:     dec EAX;
        ret 2*4;
    }
}

/** dest[#] = src[#] << numbits
 *  numbits must be in the range 1 .. 31
 *  Returns the overflow
 */
uint multibyteShlNoMMX(uint [] dest, const uint [] src, uint numbits) pure
{
    // Timing: Optimal for P6 family.
    // 2.0 cycles/int on PPro .. PM (limited by execution port p0)
    // 5.0 cycles/int on Athlon, which has 7 cycles for SHLD!!
    enum { LASTPARAM = 4*4 } // 3* pushes + return address.
    asm pure nothrow {
        naked;
        push ESI;
        push EDI;
        push EBX;
        mov EDI, [ESP + LASTPARAM + 4*3]; //dest.ptr;
        mov EBX, [ESP + LASTPARAM + 4*2]; //dest.length;
        mov ESI, [ESP + LASTPARAM + 4*1]; //src.ptr;
        mov ECX, EAX; // numbits;

        mov EAX, [-4+ESI + 4*EBX];
        mov EDX, 0;
        shld EDX, EAX, CL;
        push EDX; // Save return value
        cmp EBX, 1;
        jz L_last;
        mov EDX, [-4+ESI + 4*EBX];
        test EBX, 1;
        jz L_odd;
        sub EBX, 1;
L_even:
        mov EDX, [-4+ ESI + 4*EBX];
        shld EAX, EDX, CL;
        mov [EDI+4*EBX], EAX;
L_odd:
        mov EAX, [-8+ESI + 4*EBX];
        shld EDX, EAX, CL;
        mov [-4+EDI + 4*EBX], EDX;
        sub EBX, 2;
        jg L_even;
L_last:
        shl EAX, CL;
        mov [EDI], EAX;
        pop EAX; // pop return value
        pop EBX;
        pop EDI;
        pop ESI;
        ret 4*4;
    }
}

/** dest[#] = src[#] >> numbits
 *  numbits must be in the range 1 .. 31
 * This version uses MMX.
 */
uint multibyteShl(uint [] dest, const uint [] src, uint numbits) pure
{
    // Timing:
    // K7 1.2/int. PM 1.7/int P4 5.3/int
    enum { LASTPARAM = 4*4 } // 3* pushes + return address.
    asm pure nothrow {
        naked;
        push ESI;
        push EDI;
        push EBX;
        mov EDI, [ESP + LASTPARAM + 4*3]; //dest.ptr;
        mov EBX, [ESP + LASTPARAM + 4*2]; //dest.length;
        mov ESI, [ESP + LASTPARAM + 4*1]; //src.ptr;

        movd MM3, EAX; // numbits = bits to shift left
        xor EAX, 63;
        align   16;
        inc EAX;
        movd MM4, EAX ; // 64-numbits = bits to shift right

        // Get the return value into EAX
        and EAX, 31; // EAX = 32-numbits
        movd MM2, EAX; // 32-numbits
        movd MM1, [ESI+4*EBX-4];
        psrlq MM1, MM2;
        movd EAX, MM1;  // EAX = return value
        test EBX, 1;
        jz L_even;
L_odd:
        cmp EBX, 1;
        jz L_length1;

         // deal with odd lengths
        movq MM1, [ESI+4*EBX-8];
        psrlq MM1, MM2;
        movd    [EDI +4*EBX-4], MM1;
        sub EBX, 1;
L_even: // It's either singly or doubly even
        movq    MM2, [ESI + 4*EBX - 8];
        psllq   MM2, MM3;
        sub EBX, 2;
        jle L_last;
        movq MM1, MM2;
        add EBX, 2;
        test EBX, 2;
        jz L_onceeven;
        sub EBX, 2;

        // MAIN LOOP -- 128 bytes per iteration
 L_twiceeven:      // here MM2 is the carry
        movq    MM0, [ESI + 4*EBX-8];
        psrlq   MM0, MM4;
        movq    MM1, [ESI + 4*EBX-8];
        psllq   MM1, MM3;
        por     MM2, MM0;
        movq    [EDI +4*EBX], MM2;
L_onceeven:        // here MM1 is the carry
        movq    MM0, [ESI + 4*EBX-16];
        psrlq   MM0, MM4;
        movq    MM2, [ESI + 4*EBX-16];
        por     MM1, MM0;
        movq    [EDI +4*EBX-8], MM1;
        psllq   MM2, MM3;
        sub EBX, 4;
        jg L_twiceeven;
L_last:
        movq    [EDI +4*EBX], MM2;
L_alldone:
        emms;  // NOTE: costs 6 cycles on Intel CPUs
        pop EBX;
        pop EDI;
        pop ESI;
        ret 4*4;

L_length1:
        // length 1 is a special case
        movd MM1, [ESI];
        psllq MM1, MM3;
        movd [EDI], MM1;
        jmp L_alldone;
    }
}

void multibyteShr(uint [] dest, const uint [] src, uint numbits) pure
{
    enum { LASTPARAM = 4*4 } // 3* pushes + return address.
    asm pure nothrow {
        naked;
        push ESI;
        push EDI;
        push EBX;
        mov EDI, [ESP + LASTPARAM + 4*3]; //dest.ptr;
        mov EBX, [ESP + LASTPARAM + 4*2]; //dest.length;
align 16;
        mov ESI, [ESP + LASTPARAM + 4*1]; //src.ptr;
        lea EDI, [EDI + 4*EBX]; // EDI = end of dest
        lea ESI, [ESI + 4*EBX]; // ESI = end of src
        neg EBX;                // count UP to zero.

        movd MM3, EAX; // numbits = bits to shift right
        xor EAX, 63;
        inc EAX;
        movd MM4, EAX ; // 64-numbits = bits to shift left

        test EBX, 1;
        jz L_even;
L_odd:
         // deal with odd lengths
        and EAX, 31; // EAX = 32-numbits
        movd MM2, EAX; // 32-numbits
        cmp EBX, -1;
        jz L_length1;

        movq MM0, [ESI+4*EBX];
        psrlq MM0, MM3;
        movd    [EDI +4*EBX], MM0;
        add EBX, 1;
L_even:
        movq    MM2, [ESI + 4*EBX];
        psrlq   MM2, MM3;

        movq MM1, MM2;
        add EBX, 4;
        cmp EBX, -2+4;
        jz L_last;
        // It's either singly or doubly even
        sub EBX, 2;
        test EBX, 2;
        jnz L_onceeven;
        add EBX, 2;

        // MAIN LOOP -- 128 bytes per iteration
 L_twiceeven:      // here MM2 is the carry
        movq    MM0, [ESI + 4*EBX-8];
        psllq   MM0, MM4;
        movq    MM1, [ESI + 4*EBX-8];
        psrlq   MM1, MM3;
        por     MM2, MM0;
        movq    [EDI +4*EBX-16], MM2;
L_onceeven:        // here MM1 is the carry
        movq    MM0, [ESI + 4*EBX];
        psllq   MM0, MM4;
        movq    MM2, [ESI + 4*EBX];
        por     MM1, MM0;
        movq    [EDI +4*EBX-8], MM1;
        psrlq   MM2, MM3;
        add EBX, 4;
        jl L_twiceeven;
L_last:
        movq    [EDI +4*EBX-16], MM2;
L_alldone:
        emms;  // NOTE: costs 6 cycles on Intel CPUs
        pop EBX;
        pop EDI;
        pop ESI;
        ret 4*4;

L_length1:
        // length 1 is a special case
        movd MM1, [ESI+4*EBX];
        psrlq MM1, MM3;
        movd    [EDI +4*EBX], MM1;
        jmp L_alldone;

    }
}

/** dest[#] = src[#] >> numbits
 *  numbits must be in the range 1 .. 31
 */
void multibyteShrNoMMX(uint [] dest, const uint [] src, uint numbits) pure
{
    // Timing: Optimal for P6 family.
    // 2.0 cycles/int on PPro .. PM (limited by execution port p0)
    // Terrible performance on AMD64, which has 7 cycles for SHRD!!
    enum { LASTPARAM = 4*4 } // 3* pushes + return address.
    asm pure nothrow {
        naked;
        push ESI;
        push EDI;
        push EBX;
        mov EDI, [ESP + LASTPARAM + 4*3]; //dest.ptr;
        mov EBX, [ESP + LASTPARAM + 4*2]; //dest.length;
        mov ESI, [ESP + LASTPARAM + 4*1]; //src.ptr;
        mov ECX, EAX; // numbits;

        lea EDI, [EDI + 4*EBX]; // EDI = end of dest
        lea ESI, [ESI + 4*EBX]; // ESI = end of src
        neg EBX;                // count UP to zero.
        mov EAX, [ESI + 4*EBX];
        cmp EBX, -1;
        jz L_last;
        mov EDX, [ESI + 4*EBX];
        test EBX, 1;
        jz L_odd;
        add EBX, 1;
L_even:
        mov EDX, [ ESI + 4*EBX];
        shrd EAX, EDX, CL;
        mov [-4 + EDI+4*EBX], EAX;
L_odd:
        mov EAX, [4 + ESI + 4*EBX];
        shrd EDX, EAX, CL;
        mov [EDI + 4*EBX], EDX;
        add EBX, 2;
        jl L_even;
L_last:
        shr EAX, CL;
        mov [-4 + EDI], EAX;

        pop EBX;
        pop EDI;
        pop ESI;
        ret 4*4;
    }
}

@system unittest
{

    uint [] aa = [0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD, 0xEEEE_EEEE];
    multibyteShr(aa[0..$-1], aa, 4);
    assert(aa[0] == 0x6122_2222 && aa[1]==0xA455_5555
        && aa[2]==0xD899_9999 && aa[3]==0x0BCC_CCCC);

    aa = [0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD, 0xEEEE_EEEE];
    multibyteShr(aa[2..$-1], aa[2..$-1], 4);
    assert(aa[0] == 0x1222_2223 && aa[1]==0x4555_5556
        && aa[2]==0xD899_9999 && aa[3]==0x0BCC_CCCC);

    aa = [0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD, 0xEEEE_EEEE];
    multibyteShr(aa[0..$-2], aa, 4);
    assert(aa[1]==0xA455_5555 && aa[2]==0x0899_9999);
    assert(aa[0]==0x6122_2222);
    assert(aa[3]==0xBCCC_CCCD);


    aa = [0xF0FF_FFFF, 0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD, 0xEEEE_EEEE];
    uint r = multibyteShl(aa[2 .. 4], aa[2 .. 4], 4);
    assert(aa[0] == 0xF0FF_FFFF && aa[1]==0x1222_2223
        && aa[2]==0x5555_5560 && aa[3]==0x9999_99A4 && aa[4]==0xBCCC_CCCD);
    assert(r == 8);

    aa = [0xF0FF_FFFF, 0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD, 0xEEEE_EEEE];
    r = multibyteShl(aa[1 .. 4], aa[1 .. 4], 4);
    assert(aa[0] == 0xF0FF_FFFF
        && aa[2]==0x5555_5561);
        assert(aa[3]==0x9999_99A4 && aa[4]==0xBCCC_CCCD);
    assert(r == 8);
        assert(aa[1]==0x2222_2230);

    aa = [0xF0FF_FFFF, 0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD, 0xEEEE_EEEE];
    r = multibyteShl(aa[0 .. 4], aa[1 .. 5], 31);
}

/** dest[#] = src[#] * multiplier + carry.
 * Returns carry.
 */
uint multibyteMul(uint[] dest, const uint[] src, uint multiplier, uint carry)
    pure
{
    // Timing: definitely not optimal.
    // Pentium M: 5.0 cycles/operation, has 3 resource stalls/iteration
    // Fastest implementation found was 4.6 cycles/op, but not worth the complexity.

    enum { LASTPARAM = 4*4 } // 4* pushes + return address.
    // We'll use p2 (load unit) instead of the overworked p0 or p1 (ALU units)
    // when initializing variables to zero.
    version (D_PIC)
    {
        enum { zero = 0 }
    }
    else
    {
        __gshared int zero = 0;
    }
    asm pure nothrow {
        naked;
        push ESI;
        push EDI;
        push EBX;

        mov EDI, [ESP + LASTPARAM + 4*4]; // dest.ptr
        mov EBX, [ESP + LASTPARAM + 4*3]; // dest.length
        mov ESI, [ESP + LASTPARAM + 4*2];  // src.ptr
        align 16;
        lea EDI, [EDI + 4*EBX]; // EDI = end of dest
        lea ESI, [ESI + 4*EBX]; // ESI = end of src
        mov ECX, EAX; // [carry]; -- last param is in EAX.
        neg EBX;                // count UP to zero.
        test EBX, 1;
        jnz L_odd;
        add EBX, 1;
 L1:
        mov EAX, [-4 + ESI + 4*EBX];
        mul int ptr [ESP+LASTPARAM]; //[multiplier];
        add EAX, ECX;
        mov ECX, zero;
        mov [-4+EDI + 4*EBX], EAX;
        adc ECX, EDX;
L_odd:
        mov EAX, [ESI + 4*EBX];  // p2
        mul int ptr [ESP+LASTPARAM]; //[multiplier]; // p0*3,
        add EAX, ECX;
        mov ECX, zero;
        adc ECX, EDX;
        mov [EDI + 4*EBX], EAX;
        add EBX, 2;
        jl L1;

        mov EAX, ECX; // get final carry

        pop EBX;
        pop EDI;
        pop ESI;
        ret 5*4;
    }
}

@system unittest
{
    uint [] aa = [0xF0FF_FFFF, 0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD, 0xEEEE_EEEE];
    multibyteMul(aa[1 .. 4], aa[1 .. 4], 16, 0);
    assert(aa[0] == 0xF0FF_FFFF && aa[1] == 0x2222_2230 &&
        aa[2]==0x5555_5561 && aa[3]==0x9999_99A4 && aa[4]==0x0BCCC_CCCD);
}

// The inner multiply-and-add loop, together with the Even entry point.
// Multiples by M_ADDRESS which should be "ESP+LASTPARAM" or "ESP". OP must be "add" or "sub"
// This is the most time-critical code in the BigInt library.
// It is used by both MulAdd, multiplyAccumulate, and triangleAccumulate
string asmMulAdd_innerloop(string OP, string M_ADDRESS) pure {
    // The bottlenecks in this code are extremely complicated. The MUL, ADD, and ADC
    // need 4 cycles on each of the ALUs units p0 and p1. So we use memory load
    // (unit p2) for initializing registers to zero.
    // There are also dependencies between the instructions, and we run up against the
    // ROB-read limit (can only read 2 registers per cycle).
    // We also need the number of uops in the loop to be a multiple of 3.
    // The only available execution unit for this is p3 (memory write). Unfortunately we can't do that
        // if Position-Independent Code is required.

        // Register usage
    // ESI = end of src
    // EDI = end of dest
    // EBX = index. Counts up to zero (in steps of 2).
    // EDX:EAX = scratch, used in multiply.
    // ECX = carry1.
    // EBP = carry2.
        // ESP = points to the multiplier.

        // The first member of 'dest' which will be modified is [EDI+4*EBX].
        // EAX must already contain the first member of 'src', [ESI+4*EBX].

    version (D_PIC) { bool using_PIC = true; } else { bool using_PIC = false; }
    return "
        // Entry point for even length
        add EBX, 1;
        mov EBP, ECX; // carry

        mul int ptr [" ~ M_ADDRESS ~ "]; // M
        mov ECX, 0;

        add EBP, EAX;
        mov EAX, [ESI+4*EBX];
        adc ECX, EDX;

        mul int ptr [" ~ M_ADDRESS ~ "]; // M
        " ~ OP ~ " [-4+EDI+4*EBX], EBP;
        mov EBP, zero;

        adc ECX, EAX;
        mov EAX, [4+ESI+4*EBX];

        adc EBP, EDX;
        add EBX, 2;
        jnl L_done;
L1:
        mul int ptr [" ~ M_ADDRESS ~ "];
        " ~ OP ~ " [-8+EDI+4*EBX], ECX;
        adc EBP, EAX;
        mov ECX, zero;
        mov EAX, [ESI+4*EBX];
        adc ECX, EDX;
" ~
        (using_PIC ? "" : "   mov storagenop, EDX; ") // make #uops in loop a multiple of 3, can't do this in PIC mode.
~ "
        mul int ptr [" ~ M_ADDRESS ~ "];
        " ~ OP ~ " [-4+EDI+4*EBX], EBP;
        mov EBP, zero;

        adc ECX, EAX;
        mov EAX, [4+ESI+4*EBX];

        adc EBP, EDX;
        add EBX, 2;
        jl L1;
L_done: " ~ OP ~ " [-8+EDI+4*EBX], ECX;
        adc EBP, 0;
";
                // final carry is now in EBP
}

string asmMulAdd_enter_odd(string OP, string M_ADDRESS) pure
{
    return "
        mul int ptr [" ~M_ADDRESS ~"];
        mov EBP, zero;
        add ECX, EAX;
        mov EAX, [4+ESI+4*EBX];

        adc EBP, EDX;
        add EBX, 2;
        jl L1;
        jmp L_done;
";
}



/**
 * dest[#] += src[#] * multiplier OP carry(0 .. FFFF_FFFF).
 * where op == '+' or '-'
 * Returns carry out of MSB (0 .. FFFF_FFFF).
 */
uint multibyteMulAdd(char op)(uint [] dest, const uint [] src, uint
        multiplier, uint carry) pure {
    // Timing: This is the most time-critical bignum function.
    // Pentium M: 5.4 cycles/operation, still has 2 resource stalls + 1load block/iteration

    // The main loop is pipelined and unrolled by 2,
    //   so entry to the loop is also complicated.

    // Register usage
    // EDX:EAX = multiply
    // EBX = counter
    // ECX = carry1
    // EBP = carry2
    // EDI = dest
    // ESI = src

    enum string OP = (op=='+')? "add" : "sub";
    version (D_PIC)
    {
        enum { zero = 0 }
    }
    else
    {
        // use p2 (load unit) instead of the overworked p0 or p1 (ALU units)
        // when initializing registers to zero.
        __gshared int zero = 0;
        // use p3/p4 units
        __gshared int storagenop; // write-only
    }

    enum { LASTPARAM = 5*4 } // 4* pushes + return address.
    asm pure nothrow {
        naked;

        push ESI;
        push EDI;
        push EBX;
        push EBP;
        mov EDI, [ESP + LASTPARAM + 4*4]; // dest.ptr
        mov EBX, [ESP + LASTPARAM + 4*3]; // dest.length
        align 16;
        nop;
        mov ESI, [ESP + LASTPARAM + 4*2];  // src.ptr
        lea EDI, [EDI + 4*EBX]; // EDI = end of dest
        lea ESI, [ESI + 4*EBX]; // ESI = end of src
        mov EBP, 0;
        mov ECX, EAX; // ECX = input carry.
        neg EBX;                // count UP to zero.
        mov EAX, [ESI+4*EBX];
        test EBX, 1;
        jnz L_enter_odd;
    }
    // Main loop, with entry point for even length
    mixin("asm pure nothrow {" ~ asmMulAdd_innerloop(OP, "ESP+LASTPARAM") ~ "}");
    asm pure nothrow {
        mov EAX, EBP; // get final carry
        pop EBP;
        pop EBX;
        pop EDI;
        pop ESI;
        ret 5*4;
    }
L_enter_odd:
    mixin("asm pure nothrow {" ~ asmMulAdd_enter_odd(OP, "ESP+LASTPARAM") ~ "}");
}

@system unittest
{

    uint [] aa = [0xF0FF_FFFF, 0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD, 0xEEEE_EEEE];
    uint [] bb = [0x1234_1234, 0xF0F0_F0F0, 0x00C0_C0C0, 0xF0F0_F0F0, 0xC0C0_C0C0];
    multibyteMulAdd!('+')(bb[1..$-1], aa[1..$-2], 16, 5);
    assert(bb[0] == 0x1234_1234 && bb[4] == 0xC0C0_C0C0);
    assert(bb[1] == 0x2222_2230 + 0xF0F0_F0F0+5 && bb[2] == 0x5555_5561+0x00C0_C0C0+1
         && bb[3] == 0x9999_99A4+0xF0F0_F0F0 );
}

/**
   Sets result[#] = result[0 .. left.length] + left[#] * right[#]

   It is defined in this way to allow cache-efficient multiplication.
   This function is equivalent to:
    ----
    for (int i = 0; i< right.length; ++i)
    {
        dest[left.length + i] = multibyteMulAdd(dest[i .. left.length+i],
                left, right[i], 0);
    }
    ----
 */
void multibyteMultiplyAccumulate(uint [] dest, const uint[] left,
        const uint [] right) pure {
    // Register usage
    // EDX:EAX = used in multiply
    // EBX = index
    // ECX = carry1
    // EBP = carry2
    // EDI = end of dest for this pass through the loop. Index for outer loop.
    // ESI = end of left. never changes
    // [ESP] = M = right[i] = multiplier for this pass through the loop.
    // right.length is changed into dest.ptr+dest.length
    version (D_PIC)
    {
        enum { zero = 0 }
    }
    else
    {
        // use p2 (load unit) instead of the overworked p0 or p1 (ALU units)
        // when initializing registers to zero.
        __gshared int zero = 0;
        // use p3/p4 units
        __gshared int storagenop; // write-only
    }

    enum { LASTPARAM = 6*4 } // 4* pushes + local + return address.
    asm pure nothrow {
        naked;

        push ESI;
        push EDI;
        align 16;
        push EBX;
        push EBP;
        push EAX;    // local variable M
        mov EDI, [ESP + LASTPARAM + 4*5]; // dest.ptr
        mov EBX, [ESP + LASTPARAM + 4*2]; // left.length
        mov ESI, [ESP + LASTPARAM + 4*3];  // left.ptr
        lea EDI, [EDI + 4*EBX]; // EDI = end of dest for first pass

        mov EAX, [ESP + LASTPARAM + 4*0]; // right.length
        lea EAX, [EDI + 4*EAX];
        mov [ESP + LASTPARAM + 4*0], EAX; // last value for EDI

        lea ESI, [ESI + 4*EBX]; // ESI = end of left
        mov EAX, [ESP + LASTPARAM + 4*1]; // right.ptr
        mov EAX, [EAX];
        mov [ESP], EAX; // M
outer_loop:
        mov EBP, 0;
        mov ECX, 0; // ECX = input carry.
        neg EBX;                // count UP to zero.
        mov EAX, [ESI+4*EBX];
        test EBX, 1;
        jnz L_enter_odd;
    }
    // -- Inner loop, with even entry point
    mixin("asm pure nothrow { " ~ asmMulAdd_innerloop("add", "ESP") ~ "}");
    asm pure nothrow {
        mov [-4+EDI+4*EBX], EBP;
        add EDI, 4;
        cmp EDI, [ESP + LASTPARAM + 4*0]; // is EDI = &dest[$]?
        jz outer_done;
        mov EAX, [ESP + LASTPARAM + 4*1]; // right.ptr
        mov EAX, [EAX+4];                 // get new M
        mov [ESP], EAX;                   // save new M
        add int ptr [ESP + LASTPARAM + 4*1], 4; // right.ptr
        mov EBX, [ESP + LASTPARAM + 4*2]; // left.length
        jmp outer_loop;
outer_done:
        pop EAX;
        pop EBP;
        pop EBX;
        pop EDI;
        pop ESI;
        ret 6*4;
    }
L_enter_odd:
    mixin("asm pure nothrow {" ~ asmMulAdd_enter_odd("add", "ESP") ~ "}");
}

/**  dest[#] /= divisor.
 * overflow is the initial remainder, and must be in the range 0 .. divisor-1.
 * divisor must not be a power of 2 (use right shift for that case;
 * A division by zero will occur if divisor is a power of 2).
 * Returns the final remainder
 *
 * Based on public domain code by Eric Bainville.
 * (http://www.bealto.com/) Used with permission.
 */
uint multibyteDivAssign(uint [] dest, uint divisor, uint overflow) pure
{
    // Timing: limited by a horrible dependency chain.
    // Pentium M: 18 cycles/op, 8 resource stalls/op.
    // EAX, EDX = scratch, used by MUL
    // EDI = dest
    // CL = shift
    // ESI = quotient
    // EBX = remainderhi
    // EBP = remainderlo
    // [ESP-4] = mask
    // [ESP] = kinv (2^64 /divisor)
    enum { LASTPARAM = 5*4 } // 4* pushes + return address.
    enum { LOCALS = 2*4} // MASK, KINV
    asm pure nothrow {
        naked;

        push ESI;
        push EDI;
        push EBX;
        push EBP;

        mov EDI, [ESP + LASTPARAM + 4*2]; // dest.ptr
        mov EBX, [ESP + LASTPARAM + 4*1]; // dest.length

        // Loop from msb to lsb
        lea     EDI, [EDI + 4*EBX];
        mov EBP, EAX; // rem is the input remainder, in 0 .. divisor-1
        // Build the pseudo-inverse of divisor k: 2^64/k
        // First determine the shift in ecx to get the max number of bits in kinv
        xor     ECX, ECX;
        mov     EAX, [ESP + LASTPARAM]; //divisor;
        mov     EDX, 1;
kinv1:
        inc     ECX;
        ror     EDX, 1;
        shl     EAX, 1;
        jnc     kinv1;
        dec     ECX;
        // Here, ecx is a left shift moving the msb of k to bit 32

        mov     EAX, 1;
        shl     EAX, CL;
        dec     EAX;
        ror     EAX, CL ; //ecx bits at msb
        push    EAX; // MASK

        // Then divide 2^(32+cx) by divisor (edx already ok)
        xor     EAX, EAX;
        div     int ptr [ESP + LASTPARAM +  LOCALS-4*1]; //divisor;
        push    EAX; // kinv
        align   16;
L2:
        // Get 32 bits of quotient approx, multiplying
        // most significant word of (rem*2^32+input)
        mov     EAX, [ESP+4]; //MASK;
        and     EAX, [EDI - 4];
        or      EAX, EBP;
        rol     EAX, CL;
        mov     EBX, EBP;
        mov     EBP, [EDI - 4];
        mul     int ptr [ESP]; //KINV;

        shl     EAX, 1;
        rcl     EDX, 1;

        // Multiply by k and subtract to get remainder
        // Subtraction must be done on two words
        mov     EAX, EDX;
        mov     ESI, EDX; // quot = high word
        mul     int ptr [ESP + LASTPARAM+LOCALS]; //divisor;
        sub     EBP, EAX;
        sbb     EBX, EDX;
        jz      Lb;  // high word is 0, goto adjust on single word

        // Adjust quotient and remainder on two words
Ld:     inc     ESI;
        sub     EBP, [ESP + LASTPARAM+LOCALS]; //divisor;
        sbb     EBX, 0;
        jnz     Ld;

        // Adjust quotient and remainder on single word
Lb:     cmp     EBP, [ESP + LASTPARAM+LOCALS]; //divisor;
        jc      Lc; // rem in 0 .. divisor-1, OK
        sub     EBP, [ESP + LASTPARAM+LOCALS]; //divisor;
        inc     ESI;
        jmp     Lb;

        // Store result
Lc:
        mov     [EDI - 4], ESI;
        lea     EDI, [EDI - 4];
        dec     int ptr [ESP + LASTPARAM + 4*1+LOCALS]; // len
        jnz    L2;

        pop EAX; // discard kinv
        pop EAX; // discard mask

        mov     EAX, EBP; // return final remainder
        pop     EBP;
        pop     EBX;
        pop     EDI;
        pop     ESI;
        ret     3*4;
    }
}

@system unittest
{
    uint [] aa = new uint[101];
    for (int i=0; i<aa.length; ++i) aa[i] = 0x8765_4321 * (i+3);
    uint overflow = multibyteMul(aa, aa, 0x8EFD_FCFB, 0x33FF_7461);
    uint r = multibyteDivAssign(aa, 0x8EFD_FCFB, overflow);
    for (int i=0; i<aa.length-1; ++i) assert(aa[i] == 0x8765_4321 * (i+3));
    assert(r == 0x33FF_7461);
}

// Set dest[2*i .. 2*i+1]+=src[i]*src[i]
void multibyteAddDiagonalSquares(uint [] dest, const uint [] src) pure
{
    /* Unlike mulAdd, the carry is only 1 bit,
           since FFFF*FFFF+FFFF_FFFF = 1_0000_0000.
           Note also that on the last iteration, no carry can occur.
           As for multibyteAdd, we save & restore carry flag through the loop.

           The timing is entirely dictated by the dependency chain. We could
           improve it by moving the mov EAX after the adc [EDI], EAX. Probably not worthwhile.
    */
    enum { LASTPARAM = 4*5 } // 4* pushes + return address.
    asm pure nothrow {
        naked;
        push ESI;
        push EDI;
        push EBX;
            push ECX;
        mov EDI, [ESP + LASTPARAM + 4*3]; //dest.ptr;
        mov EBX, [ESP + LASTPARAM + 4*0]; //src.length;
        mov ESI, [ESP + LASTPARAM + 4*1]; //src.ptr;
        lea EDI, [EDI + 8*EBX];      // EDI = end of dest
        lea ESI, [ESI + 4*EBX];      // ESI = end of src
        neg EBX;                     // count UP to zero.
        xor ECX, ECX;             // initial carry = 0.
L1:
        mov EAX, [ESI + 4*EBX];
        mul EAX, EAX;
        shr CL, 1;                 // get carry
        adc [EDI + 8*EBX], EAX;
        adc [EDI + 8*EBX + 4], EDX;
        setc CL;                   // save carry
        inc EBX;
        jnz L1;

        pop ECX;
        pop EBX;
        pop EDI;
        pop ESI;
        ret 4*4;
    }
}

@system unittest
{
    uint [] aa = new uint[13];
        uint [] bb = new uint[6];
    for (int i=0; i<aa.length; ++i) aa[i] = 0x8000_0000;
    for (int i=0; i<bb.length; ++i) bb[i] = i;
        aa[$-1]= 7;
    multibyteAddDiagonalSquares(aa[0..$-1], bb);
        assert(aa[$-1]==7);
        for (int i=0; i<bb.length; ++i) { assert(aa[2*i]==0x8000_0000+i*i); assert(aa[2*i+1]==0x8000_0000); }
}

void multibyteTriangleAccumulateD(uint[] dest, uint[] x) pure
{
    for (int i = 0; i < x.length-3; ++i)
    {
        dest[i+x.length] = multibyteMulAdd!('+')(
             dest[i+i+1 .. i+x.length], x[i+1..$], x[i], 0);
    }
    ulong c = cast(ulong)(x[$-3]) * x[$-2] + dest[$-5];
    dest[$-5] = cast(uint) c;
    c >>= 32;
    c += cast(ulong)(x[$-3]) * x[$-1] + dest[$-4];
    dest[$-4] = cast(uint) c;
    c >>= 32;
length2:
    c += cast(ulong)(x[$-2]) * x[$-1];
        dest[$-3] = cast(uint) c;
        c >>= 32;
        dest[$-2] = cast(uint) c;
}

//dest += src[0]*src[1...$] + src[1]*src[2..$] + ... + src[$-3]*src[$-2..$]+ src[$-2]*src[$-1]
// assert(dest.length = src.length*2);
// assert(src.length >= 3);
void multibyteTriangleAccumulateAsm(uint[] dest, const uint[] src) pure
{
    // Register usage
    // EDX:EAX = used in multiply
    // EBX = index
    // ECX = carry1
    // EBP = carry2
    // EDI = end of dest for this pass through the loop. Index for outer loop.
    // ESI = end of src. never changes
    // [ESP] = M = src[i] = multiplier for this pass through the loop.
    // dest.length is changed into dest.ptr+dest.length
    version (D_PIC)
    {
        enum { zero = 0 }
    }
    else
    {
        // use p2 (load unit) instead of the overworked p0 or p1 (ALU units)
        // when initializing registers to zero.
        __gshared int zero = 0;
        // use p3/p4 units
        __gshared int storagenop; // write-only
    }

    enum { LASTPARAM = 6*4 } // 4* pushes + local + return address.
    asm pure nothrow {
        naked;

        push ESI;
        push EDI;
        align 16;
        push EBX;
        push EBP;
        push EAX;    // local variable M= src[i]
        mov EDI, [ESP + LASTPARAM + 4*3]; // dest.ptr
        mov EBX, [ESP + LASTPARAM + 4*0]; // src.length
        mov ESI, [ESP + LASTPARAM + 4*1];  // src.ptr

        lea ESI, [ESI + 4*EBX]; // ESI = end of left
        add int ptr [ESP + LASTPARAM + 4*1], 4; // src.ptr, used for getting M

        // local variable [ESP + LASTPARAM + 4*2] = last value for EDI
        lea EDI, [EDI + 4*EBX]; // EDI = end of dest for first pass

        lea EAX, [EDI + 4*EBX-3*4]; // up to src.length - 3
        mov [ESP + LASTPARAM + 4*2], EAX; // last value for EDI  = &dest[src.length*2 -3]

        cmp EBX, 3;
        jz length_is_3;

        // We start at src[1], not src[0].
        dec EBX;
        mov [ESP + LASTPARAM + 4*0], EBX;

outer_loop:
        mov EBX, [ESP + LASTPARAM + 4*0]; // src.length
        mov EBP, 0;
        mov ECX, 0; // ECX = input carry.
        dec [ESP + LASTPARAM + 4*0]; // Next time, the length will be shorter by 1.
        neg EBX;                // count UP to zero.

        mov EAX, [ESI + 4*EBX - 4*1]; // get new M
        mov [ESP], EAX;                   // save new M

        mov EAX, [ESI+4*EBX];
        test EBX, 1;
        jnz L_enter_odd;
    }
    // -- Inner loop, with even entry point
    mixin("asm pure nothrow { " ~ asmMulAdd_innerloop("add", "ESP") ~ "}");
    asm pure nothrow {
        mov [-4+EDI+4*EBX], EBP;
        add EDI, 4;
        cmp EDI, [ESP + LASTPARAM + 4*2]; // is EDI = &dest[$-3]?
        jnz outer_loop;
length_is_3:
        mov EAX, [ESI - 4*3];
        mul EAX, [ESI - 4*2];
        mov ECX, 0;
        add [EDI-2*4], EAX;  // ECX:dest[$-5] += x[$-3] * x[$-2]
        adc ECX, EDX;

        mov EAX, [ESI - 4*3];
        mul EAX, [ESI - 4*1]; // x[$-3] * x[$-1]
        add EAX, ECX;
        mov ECX, 0;
        adc EDX, 0;
        // now EDX: EAX = c + x[$-3] * x[$-1]
        add [EDI-1*4], EAX; // ECX:dest[$-4] += (EDX:EAX)
        adc ECX, EDX;  //  ECX holds dest[$-3], it acts as carry for the last row
// do length == 2
        mov EAX, [ESI - 4*2];
        mul EAX, [ESI - 4*1];
        add ECX, EAX;
        adc EDX, 0;
        mov [EDI - 0*4], ECX; // dest[$-2:$-3] = c + x[$-2] * x[$-1];
        mov [EDI + 1*4], EDX;

        pop EAX;
        pop EBP;
        pop EBX;
        pop EDI;
        pop ESI;
        ret 4*4;
    }
L_enter_odd:
    mixin("asm pure nothrow {" ~ asmMulAdd_enter_odd("add", "ESP") ~ "}");
}

@system unittest
{
   uint [] aa = new uint[200];
   uint [] a  = aa[0 .. 100];
   uint [] b  = new uint [100];
   aa[] = 761;
   a[] = 0;
   b[] = 0;
   a[3] = 6;
   b[0]=1;
   b[1] = 17;
   b[50 .. 100]=78;
   multibyteTriangleAccumulateAsm(a, b[0 .. 50]);
   uint [] c = new uint[100];
   c[] = 0;
   c[1] = 17;
   c[3] = 6;
   assert(a[]==c[]);
   assert(a[0]==0);
   aa[] = 0xFFFF_FFFF;
   a[] = 0;
   b[] = 0;
   b[0]= 0xbf6a1f01;
   b[1]=  0x6e38ed64;
   b[2]=  0xdaa797ed;
   b[3] = 0;

   multibyteTriangleAccumulateAsm(a[0 .. 8], b[0 .. 4]);
   assert(a[1]==0x3a600964);
   assert(a[2]==0x339974f6);
   assert(a[3]==0x46736fce);
   assert(a[4]==0x5e24a2b4);

   b[3] = 0xe93ff9f4;
   b[4] = 0x184f03;
   a[]=0;
   multibyteTriangleAccumulateAsm(a[0 .. 14], b[0 .. 7]);
   assert(a[3]==0x79fff5c2);
   assert(a[4]==0xcf384241);
   assert(a[5]== 0x4a17fc8);
   assert(a[6]==0x4d549025);
}


void multibyteSquare(BigDigit[] result, const BigDigit [] x) pure
{
    if (x.length < 4)
    {
        // Special cases, not worth doing triangular.
        result[x.length] = multibyteMul(result[0 .. x.length], x, x[0], 0);
        multibyteMultiplyAccumulate(result[1..$], x, x[1..$]);
        return;
    }
    //  Do half a square multiply.
    //  dest += src[0]*src[1...$] + src[1]*src[2..$] + ... + src[$-3]*src[$-2..$]+ src[$-2]*src[$-1]
    result[x.length] = multibyteMul(result[1 .. x.length], x[1..$], x[0], 0);
    multibyteTriangleAccumulateAsm(result[2..$], x[1..$]);
    // Multiply by 2
    result[$-1] = multibyteShlNoMMX(result[1..$-1], result[1..$-1], 1);
    // And add the diagonal elements
    result[0] = 0;
    multibyteAddDiagonalSquares(result, x);
}

version (BignumPerformanceTest)
{
import core.stdc.stdio;
int clock() { asm { push EBX; xor EAX, EAX; cpuid; pop EBX; rdtsc; } }

__gshared uint [2200] X1;
__gshared uint [2200] Y1;
__gshared uint [4000] Z1;

void testPerformance() pure
{
    // The performance results at the top of this file were obtained using
    // a Windows device driver to access the CPU performance counters.
    // The code below is less accurate but more widely usable.
    // The value for division is quite inconsistent.
    for (int i=0; i<X1.length; ++i) { X1[i]=i; Y1[i]=i; Z1[i]=i; }
    int t, t0;
    multibyteShl(Z1[0 .. 2000], X1[0 .. 2000], 7);
    t0 = clock();
    multibyteShl(Z1[0 .. 1000], X1[0 .. 1000], 7);
    t = clock();
    multibyteShl(Z1[0 .. 2000], X1[0 .. 2000], 7);
    auto shltime = (clock() - t) - (t - t0);
    t0 = clock();
    multibyteShr(Z1[2 .. 1002], X1[4 .. 1004], 13);
    t = clock();
    multibyteShr(Z1[2 .. 2002], X1[4 .. 2004], 13);
    auto shrtime = (clock() - t) - (t - t0);
    t0 = clock();
    multibyteAddSub!('+')(Z1[0 .. 1000], X1[0 .. 1000], Y1[0 .. 1000], 0);
    t = clock();
    multibyteAddSub!('+')(Z1[0 .. 2000], X1[0 .. 2000], Y1[0 .. 2000], 0);
    auto addtime = (clock() - t) - (t-t0);
    t0 = clock();
    multibyteMul(Z1[0 .. 1000], X1[0 .. 1000], 7, 0);
    t = clock();
    multibyteMul(Z1[0 .. 2000], X1[0 .. 2000], 7, 0);
    auto multime = (clock() - t) - (t - t0);
    multibyteMulAdd!('+')(Z1[0 .. 2000], X1[0 .. 2000], 217, 0);
    t0 = clock();
    multibyteMulAdd!('+')(Z1[0 .. 1000], X1[0 .. 1000], 217, 0);
    t = clock();
    multibyteMulAdd!('+')(Z1[0 .. 2000], X1[0 .. 2000], 217, 0);
    auto muladdtime = (clock() - t) - (t - t0);
    multibyteMultiplyAccumulate(Z1[0 .. 64], X1[0 .. 32], Y1[0 .. 32]);
    t = clock();
    multibyteMultiplyAccumulate(Z1[0 .. 64], X1[0 .. 32], Y1[0 .. 32]);
    auto accumtime = clock() - t;
    t0 = clock();
    multibyteDivAssign(Z1[0 .. 2000], 217, 0);
    t = clock();
    multibyteDivAssign(Z1[0 .. 1000], 37, 0);
    auto divtime = (t - t0) - (clock() - t);
        t= clock();
    multibyteSquare(Z1[0 .. 64], X1[0 .. 32]);
    auto squaretime = clock() - t;

    printf("-- BigInt asm performance (cycles/int) --\n");
    printf("Add:        %.2f\n", addtime/1000.0);
    printf("Shl:        %.2f\n", shltime/1000.0);
    printf("Shr:        %.2f\n", shrtime/1000.0);
    printf("Mul:        %.2f\n", multime/1000.0);
    printf("MulAdd:     %.2f\n", muladdtime/1000.0);
    printf("Div:        %.2f\n", divtime/1000.0);
    printf("MulAccum32: %.2f*n*n (total %d)\n", accumtime/(32.0*32.0), accumtime);
    printf("Square32: %.2f*n*n (total %d)\n\n", squaretime/(32.0*32.0), squaretime);
}

static this()
{
    testPerformance();
}
}

} // version (D_InlineAsm_X86)

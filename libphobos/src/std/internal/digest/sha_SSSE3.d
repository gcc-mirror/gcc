// Written in the D programming language.

/**
 * Computes SHA1 digests of arbitrary data, using an optimized algorithm with SSSE3 instructions.
 *
 * Authors:
 * The general idea is described by Dean Gaudet.
 * Another important observation is published by Max Locktyukhin.
 * (Both implementations are public domain.)
 * Translation to X86 and D by Kai Nacke <kai@redstar.de>
 *
 * References:
 *      $(LINK2 http://arctic.org/~dean/crypto/sha1.html)
 *      $(LINK2 http://software.intel.com/en-us/articles/improving-the-performance-of-the-secure-hash-algorithm-1/, Fast implementation of SHA1)
 */
module std.internal.digest.sha_SSSE3;

version (D_InlineAsm_X86)
{
    version (D_PIC) {} // Bugzilla 9378
    else
    {
        private version = USE_SSSE3;
        private version = _32Bit;
    }
}
else version (D_InlineAsm_X86_64)
{
    private version = USE_SSSE3;
    private version = _64Bit;
}

/*
 * The idea is quite simple. The SHA-1 specification defines the following message schedule:
 *     W[i] = (W[i-3] ^ W[i-8]  ^ W[i-14] ^ W[i-16]) rol 1
 *
 * To employ SSE, simply write down the formula four times:
 *     W[i  ] = (W[i-3] ^ W[i-8] ^ W[i-14] ^ W[i-16]) rol 1
 *     W[i+1] = (W[i-2] ^ W[i-7] ^ W[i-13] ^ W[i-15]) rol 1
 *     W[i+2] = (W[i-1] ^ W[i-6] ^ W[i-12] ^ W[i-14]) rol 1
 *     W[i+3] = (W[i  ] ^ W[i-5] ^ W[i-11] ^ W[i-13]) rol 1
 * The last formula requires value W[i] computed with the first formula.
 * Because the xor operation and the rotate operation are commutative, we can replace the
 * last formula with
 *     W[i+3] = (     0 ^ W[i-5] ^ W[i-11] ^ W[i-13]) rol 1
 * and then calculate
 *     W[i+3] ^= W[i] rol 1
 * which unfortunately requires many additional operations. This approach was described by
 * Dean Gaudet.
 *
 * Max Locktyukhin observed that
 *     W[i] = W[i-A] ^ W[i-B]
 * is equivalent to
 *     W[i] = W[i-2*A] ^ W[i-2*B]
 * (if the indices are still in valid ranges). Using this observation, the formula is
 * translated to
 *     W[i] = (W[i-6] ^ W[i-16] ^ W[i-28] ^ W[i-32]) rol 2
 * Again, to employ SSE the formula is used four times.
 *
 * Later on, the expression W[i] + K(i) is used. (K(i) is the constant used in round i.)
 * Once the 4 W[i] are calculated, we can also add the four K(i) values with one SSE instruction.
 *
 * The 32bit and 64bit implementations are almost identical. The main difference is that there
 * are only 8 XMM registers in 32bit mode. Therefore, space on the stack is needed to save
 * computed values.
 */

version (USE_SSSE3)
{
    /*
     * The general idea is to use the XMM registers as a sliding window over
     * message schedule. XMM0 to XMM7 are used to store the last 64 byte of
     * the message schedule. In 64 bit mode this is fine because of the number of
     * registers. The main difference of the 32 bit code is that a part of the
     * calculated message schedule is saved on the stack because 2 temporary
     * registers are needed.
     */

    /* Number of message words we are precalculating. */
    private immutable int PRECALC_AHEAD = 16;

    /* T1 and T2 are used for intermediate results of computations. */
    private immutable string T1 = "EAX";
    private immutable string T2 = "EBX";

    /* The registers used for the SHA-1 variables. */
    private immutable string A = "ECX";
    private immutable string B = "ESI";
    private immutable string C = "EDI";
    private immutable string D = "EBP";
    private immutable string E = "EDX";

    /* */
    version (_32Bit)
    {
        private immutable string SP = "ESP";
        private immutable string BUFFER_PTR = "EAX";
        private immutable string STATE_PTR = "EBX";

        // Control byte for shuffle instruction (only used in round 0-15)
        private immutable string X_SHUFFLECTL = "XMM6";

        // Round constant (only used in round 0-15)
        private immutable string X_CONSTANT = "XMM7";
    }
    version (_64Bit)
    {
        private immutable string SP = "RSP";
        private immutable string BUFFER_PTR = "R9";
        private immutable string STATE_PTR = "R8";
        private immutable string CONSTANTS_PTR = "R10";

        // Registers for temporary results (XMM10 and XMM11 are also used temporary)
        private immutable string W_TMP = "XMM8";
        private immutable string W_TMP2 = "XMM9";

        // Control byte for shuffle instruction (only used in round 0-15)
        private immutable string X_SHUFFLECTL = "XMM12";

        // Round constant
        private immutable string X_CONSTANT = "XMM13";
    }

    /* The control words for the byte shuffle instruction and the round constants. */
    align(16) public immutable uint[20] constants =
    [
        // The control words for the byte shuffle instruction.
        0x0001_0203, 0x0405_0607, 0x0809_0a0b, 0x0c0d_0e0f,
        // Constants for round 0-19
        0x5a827999, 0x5a827999, 0x5a827999, 0x5a827999,
        // Constants for round 20-39
        0x6ed9eba1, 0x6ed9eba1, 0x6ed9eba1, 0x6ed9eba1,
        // Constants for round 40-59
        0x8f1bbcdc, 0x8f1bbcdc, 0x8f1bbcdc, 0x8f1bbcdc,
        // Constants for round 60-79
        0xca62c1d6, 0xca62c1d6, 0xca62c1d6, 0xca62c1d6
    ];

    /** Simple version to produce numbers < 100 as string. */
    private nothrow pure string to_string(uint i)
    {
        if (i < 10)
            return "0123456789"[i .. i + 1];

        assert(i < 100);
        char[2] s;
        s[0] = cast(char)(i / 10 + '0');
        s[1] = cast(char)(i % 10 + '0');
        return s.idup;
    }

    /** Returns the reference to the byte shuffle control word. */
    private nothrow pure string bswap_shufb_ctl()
    {
        version (_64Bit)
            return "["~CONSTANTS_PTR~"]";
        else
            return "[constants]";
    }

    /** Returns the reference to constant used in round i. */
    private nothrow pure string constant(uint i)
    {
        version (_64Bit)
            return "16 + 16*"~to_string(i/20)~"["~CONSTANTS_PTR~"]";
        else
            return "[constants + 16 + 16*"~to_string(i/20)~"]";
    }

    /** Returns the XMM register number used in round i */
    private nothrow pure uint regno(uint i)
    {
        return (i/4)&7;
    }

    /** Returns reference to storage of vector W[i .. i+4]. */
    private nothrow pure string WiV(uint i)
    {
        return "["~SP~" + WI_PTR + "~to_string((i/4)&7)~"*16]";
    }

    /** Returns reference to storage of vector (W + K)[i .. i+4]. */
    private nothrow pure string WiKiV(uint i)
    {
        return "["~SP~" + WI_PLUS_KI_PTR + "~to_string((i/4)&3)~"*16]";
    }

    /** Returns reference to storage of value W[i] + K[i]. */
    private nothrow pure string WiKi(uint i)
    {
        return "["~SP~" + WI_PLUS_KI_PTR + 4*"~to_string(i&15)~"]";
    }

    /**
     * Chooses the instruction sequence based on the 32bit or 64bit model.
     */
    private nothrow pure string[] swt3264(string[] insn32, string[] insn64)
    {
        version (_32Bit)
        {
            return insn32;
        }
        version (_64Bit)
        {
            return insn64;
        }
    }

    /**
     * Flattens the instruction sequence and wraps it in an asm block.
     */
    private nothrow pure string wrap(string[] insn)
    {
        string s = "asm pure nothrow @nogc {";
        foreach (t; insn) s ~= (t ~ "; \n");
        s ~= "}";
        return s;
        // Is not CTFE:
        // return "asm pure nothrow @nogc { " ~ join(insn, "; \n") ~ "}";
    }

    /**
     * Weaves the 2 instruction sequences together.
     */
    private nothrow pure string[] weave(string[] seq1, string[] seq2, uint dist = 1)
    {
        string[] res = [];
        auto i1 = 0, i2 = 0;
        while (i1 < seq1.length || i2 < seq2.length)
        {
            if (i2 < seq2.length)
            {
                res ~= seq2[i2 .. i2+1];
                i2 += 1;
            }
            if (i1 < seq1.length)
            {
                import std.algorithm.comparison : min;

                res ~= seq1[i1 .. min(i1+dist, $)];
                i1 += dist;
            }
        }
        return res;
    }

    /**
     * Generates instructions to load state from memory into registers.
     */
    private nothrow pure string[] loadstate(string base, string a, string b, string c, string d, string e)
    {
        return ["mov "~a~",["~base~" + 0*4]",
                "mov "~b~",["~base~" + 1*4]",
                "mov "~c~",["~base~" + 2*4]",
                "mov "~d~",["~base~" + 3*4]",
                "mov "~e~",["~base~" + 4*4]" ];
    }

    /**
     * Generates instructions to update state from registers, saving result in memory.
     */
    private nothrow pure string[] savestate(string base, string a, string b, string c, string d, string e)
    {
        return ["add ["~base~" + 0*4],"~a,
                "add ["~base~" + 1*4],"~b,
                "add ["~base~" + 2*4],"~c,
                "add ["~base~" + 3*4],"~d,
                "add ["~base~" + 4*4],"~e ];
    }

    /** Calculates Ch(x, y, z) = z ^ (x & (y ^ z)) */
    private nothrow pure string[] Ch(string x, string y, string z)
    {
        return ["mov "~T1~","~y,
                "xor "~T1~","~z,
                "and "~T1~","~x,
                "xor "~T1~","~z ];
    }

    /** Calculates Parity(x, y, z) = x ^ y ^ z */
    private nothrow pure string[] Parity(string x, string y, string z)
    {
        return ["mov "~T1~","~z,
                "xor "~T1~","~y,
                "xor "~T1~","~x ];
    }

    /** Calculates Maj(x, y, z) = (x & y) | (z & (x ^ y)) */
    private nothrow pure string[] Maj(string x, string y, string z)
    {
        return ["mov "~T1~","~y,
                "mov "~T2~","~x,
                "or  "~T1~","~x,
                "and "~T2~","~y,
                "and "~T1~","~z,
                "or  "~T1~","~T2 ];
    }

    /** Returns function for round i. Function returns result in T1 and may destroy T2. */
    private nothrow pure string[] F(int i, string b, string c, string d)
    {
        string[] insn;
        if (i >=  0 && i <= 19) insn = Ch(b, c, d);
        else if (i >= 20 && i <= 39) insn = Parity(b, c, d);
        else if (i >= 40 && i <= 59) insn = Maj(b, c, d);
        else if (i >= 60 && i <= 79) insn = Parity(b, c, d);
        else assert(false, "Coding error");
        return insn;
    }

    /** Returns instruction used to setup a round. */
    private nothrow pure string[] xsetup(int i)
    {
        if (i == 0)
        {
            return swt3264(["movdqa "~X_SHUFFLECTL~","~bswap_shufb_ctl(),
                             "movdqa "~X_CONSTANT~","~constant(i)],
                            ["movdqa "~X_SHUFFLECTL~","~bswap_shufb_ctl(),
                             "movdqa "~X_CONSTANT~","~constant(i)]);
        }
        version (_64Bit)
        {
            if (i%20 == 0)
            {
                return ["movdqa "~X_CONSTANT~","~constant(i)];
            }
        }
        return [];
    }

    /**
     * Loads the message words and performs the little to big endian conversion.
     * Requires that the shuffle control word and the round constant is loaded
     * into required XMM register. The BUFFER_PTR register must point to the
     * buffer.
     */
    private nothrow pure string[] precalc_00_15(int i)
    {
        int regno = regno(i);

        string W = "XMM" ~ to_string(regno);
        version (_32Bit)
        {
            string W_TMP = "XMM" ~ to_string(regno+2);
        }
        version (_64Bit)
        {
            string W_TMP = "XMM" ~ to_string(regno+8);
        }

        if ((i & 3) == 0)
        {
            return ["movdqu "~W~",["~BUFFER_PTR~" + "~to_string(regno)~"*16]"];
        }
        else if ((i & 3) == 1)
        {
            return ["pshufb "~W~","~X_SHUFFLECTL] ~
                    swt3264(["movdqa "~WiV(i)~","~W], []);
        }
        else if ((i & 3) == 2)
        {
            return ["movdqa "~W_TMP~","~W,
                    "paddd "~W_TMP~","~X_CONSTANT,
                   ];
        }
        else
        {
            return ["movdqa "~WiKiV(i)~","~W_TMP,
                   ];
        }
    }

    /**
     * Done on 4 consequtive W[i] values in a single XMM register
     *  W[i  ] = (W[i-3] ^ W[i-8] ^ W[i-14] ^ W[i-16]) rol 1
     *  W[i+1] = (W[i-2] ^ W[i-7] ^ W[i-13] ^ W[i-15]) rol 1
     *  W[i+2] = (W[i-1] ^ W[i-6] ^ W[i-12] ^ W[i-14]) rol 1
     *  W[i+3] = (   0   ^ W[i-5] ^ W[i-11] ^ W[i-13]) rol 1
     *
     * This additional calculation unfortunately requires many additional operations
     *  W[i+3] ^= W[i] rol 1
     *
     * Once we have 4 W[i] values in XMM we can also add four K values with one instruction
     *   W[i:i+3] += {K,K,K,K}
     */
    private nothrow pure string[] precalc_16_31(int i)
    {
        int regno = regno(i);

        string W = "XMM" ~ to_string(regno);
        string W_minus_4 = "XMM" ~ to_string((regno-1)&7);
        string W_minus_8 = "XMM" ~ to_string((regno-2)&7);
        string W_minus_12 = "XMM" ~ to_string((regno-3)&7);
        string W_minus_16 = "XMM" ~ to_string((regno-4)&7);
        version (_32Bit)
        {
            string W_TMP = "XMM" ~ to_string((regno+1)&7);
            string W_TMP2 = "XMM" ~ to_string((regno+2)&7);
        }

        if ((i & 3) == 0)
        {
            return ["movdqa "~W~","~W_minus_12,
                    "palignr "~W~","~W_minus_16~",8",   // W[i] = W[i-14]
                    "pxor "~W~","~W_minus_16,           // W[i] ^= W[i-16]
                    "pxor "~W~","~W_minus_8,            // W[i] ^= W[i-8]
                    "movdqa "~W_TMP~","~W_minus_4,
            ];
        }
        else if ((i & 3) == 1)
        {
            return ["psrldq "~W_TMP~",4",               // W[i-3]
                    "pxor "~W~","~W_TMP,                // W[i] ^= W[i-3]
                    "movdqa "~W_TMP~","~W,
                    "psrld "~W~",31",
                    "pslld "~W_TMP~",1",
            ];
        }
        else if ((i & 3) == 2)
        {
            return ["por "~W~","~W_TMP,
                    "movdqa "~W_TMP~","~W,
                    "pslldq "~W_TMP~",12",
                    "movdqa "~W_TMP2~","~W_TMP,
                    "pslld "~W_TMP~",1",
            ];
        }
        else
        {
            return ["psrld "~W_TMP2~",31",
                    "por "~W_TMP~","~W_TMP2,
                    "pxor "~W~","~W_TMP,
                    "movdqa "~W_TMP~","~W ] ~
                   swt3264(["movdqa "~WiV(i)~","~W,
                            "paddd "~W_TMP~","~constant(i) ],
                           ["paddd "~W_TMP~","~X_CONSTANT ]) ~
                   ["movdqa "~WiKiV(i)~","~W_TMP];
        }
    }

    /** Performs the main calculation as decribed above. */
    private nothrow pure string[] precalc_32_79(int i)
    {
        int regno = regno(i);

        string W = "XMM" ~ to_string(regno);
        string W_minus_4 = "XMM" ~ to_string((regno-1)&7);
        string W_minus_8 = "XMM" ~ to_string((regno-2)&7);
        string W_minus_16 = "XMM" ~ to_string((regno-4)&7);
        version (_32Bit)
        {
            string W_minus_28 = "[ESP + WI_PTR + "~ to_string((regno-7)&7)~"*16]";
            string W_minus_32 = "[ESP + WI_PTR + "~ to_string((regno-8)&7)~"*16]";
            string W_TMP = "XMM" ~ to_string((regno+1)&7);
            string W_TMP2 = "XMM" ~ to_string((regno+2)&7);
        }
        version (_64Bit)
        {
            string W_minus_28 = "XMM" ~ to_string((regno-7)&7);
            string W_minus_32 = "XMM" ~ to_string((regno-8)&7);
        }

        if ((i & 3) == 0)
        {
            return swt3264(["movdqa "~W~","~W_minus_32], []) ~
                   ["movdqa "~W_TMP~","~W_minus_4,
                    "pxor "~W~","~W_minus_28,         // W is W_minus_32 before xor
                    "palignr "~W_TMP~","~W_minus_8~",8",
            ];
        }
        else if ((i & 3) == 1)
        {
            return ["pxor "~W~","~W_minus_16,
                    "pxor "~W~","~W_TMP,
                    "movdqa "~W_TMP~","~W,
            ];
        }
        else if ((i & 3) == 2)
        {
            return ["psrld "~W~",30",
                    "pslld "~W_TMP~",2",
                    "por "~W_TMP~","~W,
            ];
        }
        else
        {
            if (i < 76)
                return ["movdqa "~W~","~W_TMP] ~
                       swt3264(["movdqa "~WiV(i)~","~W,
                                "paddd "~W_TMP~","~constant(i)],
                               ["paddd "~W_TMP~","~X_CONSTANT]) ~
                       ["movdqa "~WiKiV(i)~","~W_TMP];
            else
                return swt3264(["paddd "~W_TMP~","~constant(i)],
                               ["paddd "~W_TMP~","~X_CONSTANT]) ~
                       ["movdqa "~WiKiV(i)~","~W_TMP];
        }
    }

    /** Choose right precalc method. */
    private nothrow pure string[] precalc(int i)
    {
        if (i >= 0 && i < 16) return precalc_00_15(i);
        if (i >= 16 && i < 32) return precalc_16_31(i);
        if (i >= 32 && i < 80) return precalc_32_79(i);
        return [];
    }

    /**
     * Return code for round i and i+1.
     * Performs the following rotation:
     * in=>out: A=>D, B=>E, C=>A, D=>B, E=>C
     */
    private nothrow pure string[] round(int i, string a, string b, string c, string d, string e)
    {
        return xsetup(PRECALC_AHEAD + i) ~
               weave(F(i, b, c, d) ~ // Returns result in T1; may destroy T2
               ["add "~e~","~WiKi(i),
                "ror "~b~",2",
                "mov "~T2~","~a,
                "add "~d~","~WiKi(i+1),
                "rol "~T2~",5",
                "add "~e~","~T1 ],
                precalc(PRECALC_AHEAD + i), 2) ~
               weave(
               ["add "~T2~","~e,  // T2 = (A <<< 5) + F(B, C, D) + Wi + Ki + E
                "mov "~e~","~T2,
                "rol "~T2~",5",
                "add "~d~","~T2 ] ~
               F(i+1, a, b, c) ~ // Returns result in T1; may destroy T2
               ["add "~d~","~T1,
                "ror "~a~",2"],
                precalc(PRECALC_AHEAD + i+1), 2);
    }

    // Offset into stack (see below)
    version (_32Bit)
    {
        private enum { STATE_OFS = 4, WI_PLUS_KI_PTR = 8, WI_PTR = 72 };
    }
    version (_64Bit)
    {
        private enum { WI_PLUS_KI_PTR = 0 };
    }

    /** The prologue sequence. */
    private nothrow pure string[] prologue()
    {
        version (_32Bit)
        {
            /*
             * Parameters:
             *   EAX contains pointer to input buffer
             *
             * Stack layout as follows:
             * +----------------+
             * | ptr to state   |
             * +----------------+
             * | return address |
             * +----------------+
             * | EBP            |
             * +----------------+
             * | ESI            |
             * +----------------+
             * | EDI            |
             * +----------------+
             * | EBX            |
             * +----------------+
             * | Space for      |
             * | Wi             | <- ESP+72
             * +----------------+
             * | Space for      |
             * | Wi+Ki          | <- ESP+8
             * +----------------+ <- 16byte aligned
             * | ptr to state   | <- ESP+4
             * +----------------+
             * | old ESP        | <- ESP
             * +----------------+
             */
            static assert(BUFFER_PTR == "EAX");
            static assert(STATE_PTR == "EBX");
            return [// Save registers according to calling convention
                    "push EBP",
                    "push ESI",
                    "push EDI",
                    "push EBX",
                    // Load parameters
                    "mov EBX, [ESP + 5*4]", //pointer to state
                    // Align stack
                    "mov EBP, ESP",
                    "sub ESP, 4*16 + 8*16",
                    "and ESP, 0xffff_fff0",
                    "push EBX",
                    "push EBP",
            ];
        }
        version (_64Bit)
        {
            /*
             * Parameters:
             *   RDX contains pointer to state
             *   RSI contains pointer to input buffer
             *   RDI contains pointer to constants
             *
             * Stack layout as follows:
             * +----------------+
             * | return address |
             * +----------------+
             * | RBP            |
             * +----------------+
             * | RBX            |
             * +----------------+
             * | Unused         |
             * +----------------+
             * | Space for      |
             * | Wi+Ki          | <- RSP
             * +----------------+ <- 16byte aligned
             */
            return [// Save registers according to calling convention
                    "push RBP",
                    "push RBX",
                    // Save parameters
                    "mov "~STATE_PTR~", RDX", //pointer to state
                    "mov "~BUFFER_PTR~", RSI", //pointer to buffer
                    "mov "~CONSTANTS_PTR~", RDI", //pointer to constants to avoid absolute addressing
                    // Align stack
                    "sub RSP, 4*16+8",
            ];
        }
    }

    /**
      * The epilogue sequence. Just pop the saved registers from stack and return to caller.
      */
    private nothrow pure string[] epilogue()
    {
        version (_32Bit)
        {
            return ["pop ESP",
                    "pop EBX",
                    "pop EDI",
                    "pop ESI",
                    "pop EBP",
                    "ret 4",
                   ];
        }
        version (_64Bit)
        {
            return ["add RSP,4*16+8",
                    "pop RBX",
                    "pop RBP",
                    "ret 0",
                   ];
        }
    }

    // constants as extra argument for PIC, see Bugzilla 9378
    import std.meta : AliasSeq;
    version (_64Bit)
        alias ExtraArgs = AliasSeq!(typeof(&constants));
    else
        alias ExtraArgs = AliasSeq!();

    /**
     *
     */
    public void transformSSSE3(uint[5]* state, const(ubyte[64])* buffer, ExtraArgs) pure nothrow @nogc
    {
        mixin(wrap(["naked;"] ~ prologue()));
        // Precalc first 4*16=64 bytes
        mixin(wrap(xsetup(0)));
        mixin(wrap(weave(precalc(0)~precalc(1)~precalc(2)~precalc(3),
                         precalc(4)~precalc(5)~precalc(6)~precalc(7))));
        mixin(wrap(weave(loadstate(STATE_PTR, A, B, C, D, E),
                   weave(precalc(8)~precalc(9)~precalc(10)~precalc(11),
                         precalc(12)~precalc(13)~precalc(14)~precalc(15)))));
        // Round 1
        mixin(wrap(round( 0, A, B, C, D, E)));
        mixin(wrap(round( 2, D, E, A, B, C)));
        mixin(wrap(round( 4, B, C, D, E, A)));
        mixin(wrap(round( 6, E, A, B, C, D)));
        mixin(wrap(round( 8, C, D, E, A, B)));
        mixin(wrap(round(10, A, B, C, D, E)));
        mixin(wrap(round(12, D, E, A, B, C)));
        mixin(wrap(round(14, B, C, D, E, A)));
        mixin(wrap(round(16, E, A, B, C, D)));
        mixin(wrap(round(18, C, D, E, A, B)));
        // Round 2
        mixin(wrap(round(20, A, B, C, D, E)));
        mixin(wrap(round(22, D, E, A, B, C)));
        mixin(wrap(round(24, B, C, D, E, A)));
        mixin(wrap(round(26, E, A, B, C, D)));
        mixin(wrap(round(28, C, D, E, A, B)));
        mixin(wrap(round(30, A, B, C, D, E)));
        mixin(wrap(round(32, D, E, A, B, C)));
        mixin(wrap(round(34, B, C, D, E, A)));
        mixin(wrap(round(36, E, A, B, C, D)));
        mixin(wrap(round(38, C, D, E, A, B)));
        // Round 3
        mixin(wrap(round(40, A, B, C, D, E)));
        mixin(wrap(round(42, D, E, A, B, C)));
        mixin(wrap(round(44, B, C, D, E, A)));
        mixin(wrap(round(46, E, A, B, C, D)));
        mixin(wrap(round(48, C, D, E, A, B)));
        mixin(wrap(round(50, A, B, C, D, E)));
        mixin(wrap(round(52, D, E, A, B, C)));
        mixin(wrap(round(54, B, C, D, E, A)));
        mixin(wrap(round(56, E, A, B, C, D)));
        mixin(wrap(round(58, C, D, E, A, B)));
        // Round 4
        mixin(wrap(round(60, A, B, C, D, E)));
        mixin(wrap(round(62, D, E, A, B, C)));
        mixin(wrap(round(64, B, C, D, E, A)));
        mixin(wrap(round(66, E, A, B, C, D)));
        mixin(wrap(round(68, C, D, E, A, B)));
        mixin(wrap(round(70, A, B, C, D, E)));
        mixin(wrap(round(72, D, E, A, B, C)));
        mixin(wrap(round(74, B, C, D, E, A)));
        mixin(wrap(round(76, E, A, B, C, D)));
        mixin(wrap(round(78, C, D, E, A, B)));
        version (_32Bit)
        {
            // Load pointer to state
            mixin(wrap(["mov "~STATE_PTR~",[ESP + STATE_OFS]"]));
        }
        mixin(wrap(savestate(STATE_PTR, A, B, C, D, E)));
        mixin(wrap(epilogue()));
    }
}

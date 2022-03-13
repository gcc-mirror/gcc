/**
 * Contains a bitfield used by the GC.
 *
 * Copyright: D Language Foundation 2005 - 2021.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, David Friedman, Sean Kelly
 */
module core.internal.gc.bits;

import core.internal.gc.os : os_mem_map, os_mem_unmap, HaveFork;

import core.bitop;
import core.stdc.string;
import core.stdc.stdlib;
import core.exception : onOutOfMemoryError;

// use version gcbitsSingleBitOperation to disable optimizations that use
//  word operands on bulk operation copyRange, setRange, clrRange, etc.
// version = gcbitsSingleBitOperation;

struct GCBits
{
@nogc:
    alias size_t wordtype;

    enum BITS_PER_WORD = (wordtype.sizeof * 8);
    enum BITS_SHIFT = (wordtype.sizeof == 8 ? 6 : 5);
    enum BITS_MASK = (BITS_PER_WORD - 1);
    enum BITS_0 = cast(wordtype)0;
    enum BITS_1 = cast(wordtype)1;
    enum BITS_2 = cast(wordtype)2;

    wordtype* data;
    size_t nbits;

    void Dtor(bool share = false) nothrow
    {
        if (data)
        {
            static if (!HaveFork)
                free(data);
            else if (share)
                os_mem_unmap(data, nwords * data[0].sizeof);
            else
                free(data);
            data = null;
        }
    }

    void alloc(size_t nbits, bool share = false) nothrow
    {
        this.nbits = nbits;
        static if (!HaveFork)
            data = cast(typeof(data[0])*)calloc(nwords, data[0].sizeof);
        else if (share)
            data = cast(typeof(data[0])*)os_mem_map(nwords * data[0].sizeof, true); // Allocate as MAP_SHARED
        else
            data = cast(typeof(data[0])*)calloc(nwords, data[0].sizeof);
        if (!data)
            onOutOfMemoryError();
    }

    wordtype test(size_t i) const scope @trusted pure nothrow @nogc
    in
    {
        assert(i < nbits);
    }
    do
    {
        return core.bitop.bt(data, i);
    }

    int set(size_t i) scope @trusted pure nothrow @nogc
    in
    {
        assert(i < nbits);
    }
    do
    {
        return core.bitop.bts(data, i);
    }

    int clear(size_t i) scope @trusted pure nothrow @nogc
    in
    {
        assert(i <= nbits);
    }
    do
    {
        return core.bitop.btr(data, i);
    }

    // return non-zero if bit already set
    size_t setLocked(size_t i) scope @trusted pure nothrow @nogc
    {
        version (GNU)
        {
            import gcc.builtins;
            const pos = i >> BITS_SHIFT;
            const mask = BITS_1 << (i & BITS_MASK);
            mixin("auto val = __atomic_fetch_or_" ~ size_t.sizeof.stringof[0]
                ~ "(cast(shared)(data + pos), mask, 3);");
            return (val & mask) != 0;
        }
        else version (LDC)
        {
            import ldc.intrinsics;
            const pos = i >> BITS_SHIFT;
            const mask = BITS_1 << (i & BITS_MASK);
            auto val = llvm_atomic_rmw_or(cast(shared)(data + pos), mask);
            return (val & mask) != 0;
        }
        else version (D_InlineAsm_X86)
        {
            asm pure @nogc nothrow {
                mov EAX, this;
                mov ECX, data[EAX];
                mov EDX, i;
                lock;
                bts dword ptr[ECX], EDX;
                sbb EAX,EAX;
            }
        }
        else version (D_InlineAsm_X86_64)
        {
            asm pure @nogc nothrow {
                mov RAX, this;
                mov RAX, data[RAX];
                mov RDX, i;
                lock;
                bts qword ptr[RAX], RDX;
                sbb RAX,RAX;
            }
        }
        else
        {
            auto pos = i >> BITS_SHIFT;
            auto pdata = cast(shared)(data + pos);
            auto mask = BITS_1 << (i & BITS_MASK);
            auto state = *pdata;
            if (state & mask)
                return state;

            import core.atomic;
            auto newstate = state | mask;
            while (!cas(pdata, state, newstate))
            {
                state = *pdata;
                if (state & mask)
                    return state;
                newstate = state | mask;
            }
            return 0;
        }
    }

    template testAndSet(bool locked)
    {
        static if (locked)
            alias testAndSet = setLocked;
        else
            alias testAndSet = set;
    }


    mixin template RangeVars()
    {
        size_t firstWord = (target >> BITS_SHIFT);
        size_t firstOff  = target &  BITS_MASK;
        size_t last      = target + len - 1;
        size_t lastWord  = (last >> BITS_SHIFT);
        size_t lastOff   = last &  BITS_MASK;
    }

    // extract loops to allow inlining the rest
    void clearWords(size_t firstWord, size_t lastWord) nothrow
    {
        for (size_t w = firstWord; w < lastWord; w++)
            data[w] = 0;
    }

    void setWords(size_t firstWord, size_t lastWord) nothrow
    {
        for (size_t w = firstWord; w < lastWord; w++)
            data[w] = ~0;
    }

    void copyWords(size_t firstWord, size_t lastWord, const(wordtype)* source) nothrow
    {
        for (size_t w = firstWord; w < lastWord; w++)
            data[w] = source[w - firstWord];
    }

    void copyWordsShifted(size_t firstWord, size_t cntWords, size_t firstOff, const(wordtype)* source) nothrow
    {
        wordtype mask = ~BITS_0 << firstOff;
        data[firstWord] = (data[firstWord] & ~mask) | (source[0] << firstOff);
        for (size_t w = 1; w < cntWords; w++)
            data[firstWord + w] = (source[w - 1] >> (BITS_PER_WORD - firstOff)) | (source[w] << firstOff);
    }

    // target = the biti to start the copy to
    // destlen = the number of bits to copy from source
    void copyRange(size_t target, size_t len, const(wordtype)* source) nothrow
    {
        version (gcbitsSingleBitOperation)
        {
            for (size_t i = 0; i < len; i++)
                if (source[(i >> BITS_SHIFT)] & (BITS_1 << (i & BITS_MASK)))
                    set(target+i);
                else
                    clear(target+i);
        }
        else
        {
            if (len > 0)
                copyRangeZ(target, len, source);
        }
    }

    void copyRangeZ(size_t target, size_t len, const(wordtype)* source) nothrow
    {
        mixin RangeVars!();

        if (firstWord == lastWord)
        {
            wordtype mask = ((BITS_2 << (lastOff - firstOff)) - 1) << firstOff;
            data[firstWord] = (data[firstWord] & ~mask) | ((source[0] << firstOff) & mask);
        }
        else if (firstOff == 0)
        {
            copyWords(firstWord, lastWord, source);

            wordtype mask = (BITS_2 << lastOff) - 1;
            data[lastWord] = (data[lastWord] & ~mask) | (source[lastWord - firstWord] & mask);
        }
        else
        {
            size_t cntWords = lastWord - firstWord;
            copyWordsShifted(firstWord, cntWords, firstOff, source);

            wordtype src = (source[cntWords - 1] >> (BITS_PER_WORD - firstOff));
            if (lastOff >= firstOff) // prevent buffer overread
                src |= (source[cntWords] << firstOff);
            wordtype mask = (BITS_2 << lastOff) - 1;
            data[lastWord] = (data[lastWord] & ~mask) | (src & mask);
        }
    }

    void copyRangeRepeating(size_t target, size_t destlen, const(wordtype)* source, size_t sourcelen) nothrow
    {
        version (gcbitsSingleBitOperation)
        {
            for (size_t i=0; i < destlen; i++)
            {
                bool b;
                size_t j = i % sourcelen;
                b = (source[j >> BITS_SHIFT] & (BITS_1 << (j & BITS_MASK))) != 0;
                if (b) set(target+i);
                else clear(target+i);
            }
        }
        else
        {
            while (destlen > sourcelen)
            {
                copyRange(target, sourcelen, source);
                target += sourcelen;
                destlen -= sourcelen;
            }
            copyRange(target, destlen, source);
        }
    }

    unittest
    {
        // simulate broken array append test case in vibe.d
        GCBits bits;
        bits.alloc(10000);
        auto data = bits.data;

        GCBits src;
        src.alloc(67);
        src.data[0] = 0x4;

        bits.copyRangeRepeating(2, 10000, src.data, 67);

        foreach (i; 0 .. 10000)
            if ((i - 2) % 67 == 2)
                assert(bits.test(i));
            else
                assert(!bits.test(i));
    }

    void setRange(size_t target, size_t len) nothrow
    {
        version (gcbitsSingleBitOperation)
        {
            for (size_t i = 0; i < len; i++)
                set(target+i);
        }
        else
        {
            if (len > 0)
                setRangeZ(target, len);
        }
    }

    void setRangeZ(size_t target, size_t len) nothrow
    {
        mixin RangeVars!();

        if (firstWord == lastWord)
        {
            wordtype mask = ((BITS_2 << (lastOff - firstOff)) - 1) << firstOff;
            data[firstWord] |= mask;
        }
        else
        {
            data[firstWord] |= ~BITS_0 << firstOff;
            setWords(firstWord + 1, lastWord);
            wordtype mask = (BITS_2 << lastOff) - 1;
            data[lastWord] |= mask;
        }
    }

    void clrRange(size_t target, size_t len) nothrow
    {
        version (gcbitsSingleBitOperation)
        {
            for (size_t i = 0; i < len; i++)
                clear(target+i);
        }
        else
        {
            if (len > 0)
                clrRangeZ(target, len);
        }
    }

    void clrRangeZ(size_t target, size_t len) nothrow
    {
        mixin RangeVars!();
        if (firstWord == lastWord)
        {
            wordtype mask = ((BITS_2 << (lastOff - firstOff)) - 1) << firstOff;
            data[firstWord] &= ~mask;
        }
        else
        {
            data[firstWord] &= ~(~BITS_0 << firstOff);
            clearWords(firstWord + 1, lastWord);
            wordtype mask = (BITS_2 << lastOff) - 1;
            data[lastWord] &= ~mask;
        }
    }

    unittest
    {
        GCBits bits;
        bits.alloc(1000);
        auto data = bits.data;

        bits.setRange(0,1);
        assert(data[0] == 1);

        bits.clrRange(0,1);
        assert(data[0] == 0);

        bits.setRange(BITS_PER_WORD-1,1);
        assert(data[0] == BITS_1 << (BITS_PER_WORD-1));

        bits.clrRange(BITS_PER_WORD-1,1);
        assert(data[0] == 0);

        bits.setRange(12,7);
        assert(data[0] == 0b0111_1111_0000_0000_0000);

        bits.clrRange(14,4);
        assert(data[0] == 0b0100_0011_0000_0000_0000);

        bits.clrRange(0,BITS_PER_WORD);
        assert(data[0] == 0);

        bits.setRange(0,BITS_PER_WORD);
        assert(data[0] == ~0);
        assert(data[1] == 0);

        bits.setRange(BITS_PER_WORD,BITS_PER_WORD);
        assert(data[0] == ~0);
        assert(data[1] == ~0);
        assert(data[2] == 0);
        bits.clrRange(BITS_PER_WORD/2,BITS_PER_WORD);
        assert(data[0] == (BITS_1 << (BITS_PER_WORD/2)) - 1);
        assert(data[1] == ~data[0]);
        assert(data[2] == 0);

        bits.setRange(8*BITS_PER_WORD+1,4*BITS_PER_WORD-2);
        assert(data[8] == ~0 << 1);
        assert(data[9] == ~0);
        assert(data[10] == ~0);
        assert(data[11] == cast(wordtype)~0 >> 1);

        bits.clrRange(9*BITS_PER_WORD+1,2*BITS_PER_WORD);
        assert(data[8] == ~0 << 1);
        assert(data[9] == 1);
        assert(data[10] == 0);
        assert(data[11] == ((cast(wordtype)~0 >> 1) & ~1));

        wordtype[4] src = [ 0xa, 0x5, 0xaa, 0x55 ];

        void testCopyRange(size_t start, size_t len, int repeat = 1)
        {
            bits.setRange(0, bits.nbits);
            if (repeat > 1)
                bits.copyRangeRepeating(start, repeat * len, src.ptr, len);
            else
                bits.copyRange(start, len, src.ptr);
            foreach (i; 0 .. start)
                assert(bits.test(i));
            foreach (r; 0 .. repeat)
                foreach (i; 0 .. len)
                    assert(!bits.test(start + r*len + i) == !core.bitop.bt(src.ptr, i));
            foreach (i; start + repeat*len .. 10*BITS_PER_WORD)
                assert(bits.test(i));
        }

        testCopyRange(20, 10); // short copy range within same word
        testCopyRange(50, 20); // short copy range spanning two words
        testCopyRange(64, 3 * BITS_PER_WORD + 3); // aligned copy range
        testCopyRange(77, 2 * BITS_PER_WORD + 15); // unaligned copy range
        testCopyRange(64, 127); // copy range within critical end alignment

        testCopyRange(10, 4, 5); // repeating small range within same word
        testCopyRange(20, 5, 10); // repeating small range spanning two words
        testCopyRange(40, 21, 7); // repeating medium range
        testCopyRange(73, 2 * BITS_PER_WORD + 15, 5); // repeating multi-word range

        testCopyRange(2, 3, 166); // failed with assert
    }

    void zero() nothrow
    {
        memset(data, 0, nwords * wordtype.sizeof);
    }

    void setAll() nothrow
    {
        memset(data, 0xFF, nwords * wordtype.sizeof);
    }

    void copy(GCBits *f) nothrow
    in
    {
        assert(nwords == f.nwords);
    }
    do
    {
        memcpy(data, f.data, nwords * wordtype.sizeof);
    }

    @property size_t nwords() const pure nothrow
    {
        return (nbits + (BITS_PER_WORD - 1)) >> BITS_SHIFT;
    }
}

unittest
{
    GCBits b;

    b.alloc(786);
    assert(!b.test(123));
    assert(!b.clear(123));
    assert(!b.set(123));
    assert(b.test(123));
    assert(b.clear(123));
    assert(!b.test(123));

    b.set(785);
    b.set(0);
    assert(b.test(785));
    assert(b.test(0));
    b.zero();
    assert(!b.test(785));
    assert(!b.test(0));

    GCBits b2;
    b2.alloc(786);
    b2.set(38);
    b.copy(&b2);
    assert(b.test(38));
    b2.Dtor();
    b.Dtor();
}

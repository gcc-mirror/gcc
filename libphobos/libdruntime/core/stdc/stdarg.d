/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_stdarg.h.html, _stdarg.h)
 *
 * Copyright: Copyright Digital Mars 2000 - 2020.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Hauke Duden
 * Standards: ISO/IEC 9899:1999 (E)
 * Source: $(DRUNTIMESRC core/stdc/_stdarg.d)
 */

module core.stdc.stdarg;

@nogc:
nothrow:

version (X86_64)
{
    version (Windows) { /* different ABI */ }
    else version = SysV_x64;
}

version (GNU)
{
    import gcc.builtins;
}
else version (SysV_x64)
{
    static import core.internal.vararg.sysv_x64;

    version (DigitalMars)
    {
        align(16) struct __va_argsave_t
        {
            size_t[6] regs;   // RDI,RSI,RDX,RCX,R8,R9
            real[8] fpregs;   // XMM0..XMM7
            __va_list va;
        }
    }
}

version (ARM)     version = ARM_Any;
version (AArch64) version = ARM_Any;
version (MIPS32)  version = MIPS_Any;
version (MIPS64)  version = MIPS_Any;
version (PPC)     version = PPC_Any;
version (PPC64)   version = PPC_Any;
version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;

version (GNU)
{
    // Uses gcc.builtins
}
else version (ARM_Any)
{
    // Darwin uses a simpler varargs implementation
    version (OSX) {}
    else version (iOS) {}
    else version (TVOS) {}
    else version (WatchOS) {}
    else:

    version (ARM)
    {
        version = AAPCS32;
    }
    else version (AArch64)
    {
        version = AAPCS64;
        static import core.internal.vararg.aarch64;
    }
}


T alignUp(size_t alignment = size_t.sizeof, T)(T base) pure
{
    enum mask = alignment - 1;
    static assert(alignment > 0 && (alignment & mask) == 0, "alignment must be a power of 2");
    auto b = cast(size_t) base;
    b = (b + mask) & ~mask;
    return cast(T) b;
}

unittest
{
    assert(1.alignUp == size_t.sizeof);
    assert(31.alignUp!16 == 32);
    assert(32.alignUp!16 == 32);
    assert(33.alignUp!16 == 48);
    assert((-9).alignUp!8 == -8);
}


version (BigEndian)
{
    // Adjusts a size_t-aligned pointer for types smaller than size_t.
    T* adjustForBigEndian(T)(T* p, size_t size) pure
    {
        return size >= size_t.sizeof ? p :
            cast(T*) ((cast(void*) p) + (size_t.sizeof - size));
    }
}


/**
 * The argument pointer type.
 */
version (GNU)
{
    alias va_list = __gnuc_va_list;
    alias __gnuc_va_list = __builtin_va_list;
}
else version (SysV_x64)
{
    alias va_list = core.internal.vararg.sysv_x64.va_list;
    public import core.internal.vararg.sysv_x64 : __va_list, __va_list_tag;
}
else version (AAPCS32)
{
    alias va_list = __va_list;

    // need std::__va_list for C++ mangling compatibility (AAPCS32 section 8.1.4)
    extern (C++, std) struct __va_list
    {
        void* __ap;
    }
}
else version (AAPCS64)
{
    alias va_list = core.internal.vararg.aarch64.va_list;
}
else version (RISCV_Any)
{
    // The va_list type is void*, according to RISCV Calling Convention
    // https://github.com/riscv-non-isa/riscv-elf-psabi-doc/blob/master/riscv-cc.adoc
    alias va_list = void*;
}
else
{
    alias va_list = char*; // incl. unknown platforms
}


/**
 * Initialize ap.
 * parmn should be the last named parameter.
 */
version (GNU)
{
    void va_start(T)(out va_list ap, ref T parmn);
}
else version (LDC)
{
    pragma(LDC_va_start)
    void va_start(T)(out va_list ap, ref T parmn) @nogc;
}
else version (DigitalMars)
{
    version (X86)
    {
        void va_start(T)(out va_list ap, ref T parmn)
        {
            ap = cast(va_list) ((cast(void*) &parmn) + T.sizeof.alignUp);
        }
    }
    else
    {
        void va_start(T)(out va_list ap, ref T parmn); // intrinsic; parmn should be __va_argsave for non-Windows x86_64 targets
    }
}


/**
 * Retrieve and return the next value that is of type T.
 */
version (GNU)
    T va_arg(T)(ref va_list ap); // intrinsic
else
T va_arg(T)(ref va_list ap)
{
    version (X86)
    {
        auto p = cast(T*) ap;
        ap += T.sizeof.alignUp;
        return *p;
    }
    else version (Win64)
    {
        // LDC passes slices as 2 separate 64-bit values, not as 128-bit struct
        version (LDC) enum isLDC = true;
        else          enum isLDC = false;
        static if (isLDC && is(T == E[], E))
        {
            auto p = cast(T*) ap;
            ap += T.sizeof;
            return *p;
        }
        else
        {
            // passed indirectly by value if > 64 bits or of a size that is not a power of 2
            static if (T.sizeof > size_t.sizeof || (T.sizeof & (T.sizeof - 1)) != 0)
                auto p = *cast(T**) ap;
            else
                auto p = cast(T*) ap;
            ap += size_t.sizeof;
            return *p;
        }
    }
    else version (SysV_x64)
    {
        return core.internal.vararg.sysv_x64.va_arg!T(ap);
    }
    else version (AAPCS32)
    {
        // AAPCS32 section 6.5 B.5: type with alignment >= 8 is 8-byte aligned
        // instead of normal 4-byte alignment (APCS doesn't do this).
        if (T.alignof >= 8)
            ap.__ap = ap.__ap.alignUp!8;
        auto p = cast(T*) ap.__ap;
        version (BigEndian)
            static if (T.sizeof < size_t.sizeof)
                p = adjustForBigEndian(p, T.sizeof);
        ap.__ap += T.sizeof.alignUp;
        return *p;
    }
    else version (AAPCS64)
    {
        return core.internal.vararg.aarch64.va_arg!T(ap);
    }
    else version (ARM_Any)
    {
        auto p = cast(T*) ap;
        version (BigEndian)
            static if (T.sizeof < size_t.sizeof)
                p = adjustForBigEndian(p, T.sizeof);
        ap += T.sizeof.alignUp;
        return *p;
    }
    else version (PPC_Any)
    {
        /*
         * The rules are described in the 64bit PowerPC ELF ABI Supplement 1.9,
         * available here:
         * http://refspecs.linuxfoundation.org/ELF/ppc64/PPC-elf64abi-1.9.html#PARAM-PASS
         */

        // Chapter 3.1.4 and 3.2.3: alignment may require the va_list pointer to first
        // be aligned before accessing a value
        if (T.alignof >= 8)
            ap = ap.alignUp!8;
        auto p = cast(T*) ap;
        version (BigEndian)
            static if (T.sizeof < size_t.sizeof)
                p = adjustForBigEndian(p, T.sizeof);
        ap += T.sizeof.alignUp;
        return *p;
    }
    else version (LoongArch64)
    {
        auto p = cast(T*) ap;
        ap += T.sizeof.alignUp;
        return *p;
    }
    else version (MIPS_Any)
    {
        auto p = cast(T*) ap;
        version (BigEndian)
            static if (T.sizeof < size_t.sizeof)
                p = adjustForBigEndian(p, T.sizeof);
        ap += T.sizeof.alignUp;
        return *p;
    }
    else version (RISCV_Any)
    {
        static if (T.sizeof > (size_t.sizeof << 1))
            auto p = *cast(T**) ap;
        else
        {
            static if (T.alignof == (size_t.sizeof << 1))
                ap = ap.alignUp!(size_t.sizeof << 1);
            auto p = cast(T*) ap;
        }
        ap += T.sizeof.alignUp;
        return *p;
    }
    else
        static assert(0, "Unsupported platform");
}


/**
 * Retrieve and store in parmn the next value that is of type T.
 */
version (GNU)
    void va_arg(T)(ref va_list ap, ref T parmn); // intrinsic
else
void va_arg(T)(ref va_list ap, ref T parmn)
{
    parmn = va_arg!T(ap);
}


/**
 * End use of ap.
 */
version (GNU)
{
    alias va_end = __builtin_va_end;
}
else version (LDC)
{
    pragma(LDC_va_end)
    void va_end(va_list ap);
}
else version (DigitalMars)
{
    void va_end(va_list ap) {}
}


/**
 * Make a copy of ap.
 */
version (GNU)
{
    alias va_copy = __builtin_va_copy;
}
else version (LDC)
{
    pragma(LDC_va_copy)
    void va_copy(out va_list dest, va_list src);
}
else version (DigitalMars)
{
    version (SysV_x64)
    {
        void va_copy(out va_list dest, va_list src, void* storage = alloca(__va_list_tag.sizeof))
        {
            // Instead of copying the pointers, and aliasing the source va_list,
            // the default argument alloca will allocate storage in the caller's
            // stack frame.  This is still not correct (it should be allocated in
            // the place where the va_list variable is declared) but most of the
            // time the caller's stack frame _is_ the place where the va_list is
            // allocated, so in most cases this will now work.
            dest = cast(va_list) storage;
            *dest = *src;
        }

        import core.stdc.stdlib : alloca;
    }
    else
    {
        void va_copy(out va_list dest, va_list src)
        {
            dest = src;
        }
    }
}

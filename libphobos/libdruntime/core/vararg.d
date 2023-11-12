/**
 * The vararg module is intended to facilitate vararg manipulation in D.
 * It should be interface compatible with the C module "stdarg," and the
 * two modules may share a common implementation if possible (as is done
 * here).
 * Copyright: Copyright Digital Mars 2000 - 2009.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Walter Bright, Hauke Duden
 * Source:    $(DRUNTIMESRC core/_vararg.d)
 */

/*          Copyright Digital Mars 2000 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.vararg;

public import core.stdc.stdarg;


version (GNU) { /* TypeInfo-based va_arg overload unsupported */ }
else:

version (ARM)     version = ARM_Any;
version (AArch64) version = ARM_Any;
version (MIPS32)  version = MIPS_Any;
version (MIPS64)  version = MIPS_Any;
version (PPC)     version = PPC_Any;
version (PPC64)   version = PPC_Any;
version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;

version (ARM_Any)
{
    // Darwin uses a simpler varargs implementation
    version (OSX) {}
    else version (iOS) {}
    else version (TVOS) {}
    else version (WatchOS) {}
    else:

    version (ARM)     version = AAPCS32;
    version (AArch64) version = AAPCS64;
}


///
alias va_arg = core.stdc.stdarg.va_arg;


/**
 * Retrieve and store through parmn the next value that is of TypeInfo ti.
 * Used when the static type is not known.
 */
void va_arg()(ref va_list ap, TypeInfo ti, void* parmn)
{
    version (X86)
    {
        // Wait until everyone updates to get TypeInfo.talign
        //auto talign = ti.talign;
        //auto p = cast(void*)(cast(size_t)ap + talign - 1) & ~(talign - 1);
        auto p = ap;
        auto tsize = ti.tsize;
        ap = cast(va_list) (p + tsize.alignUp);
        parmn[0..tsize] = p[0..tsize];
    }
    else version (Win64)
    {
        version (LDC) enum isLDC = true;
        else          enum isLDC = false;

        // Wait until everyone updates to get TypeInfo.talign
        //auto talign = ti.talign;
        //auto p = cast(void*)(cast(size_t)ap + talign - 1) & ~(talign - 1);
        auto p = ap;
        auto tsize = ti.tsize;
        void* q;
        if (isLDC && tsize == 16 && cast(TypeInfo_Array) ti)
        {
            q = p;
            ap = cast(va_list) (p + tsize);
        }
        else
        {
            q = (tsize > size_t.sizeof || (tsize & (tsize - 1)) != 0) ? *cast(void**) p : p;
            ap = cast(va_list) (p + size_t.sizeof);
        }
        parmn[0..tsize] = q[0..tsize];
    }
    else version (X86_64)
    {
        static import core.internal.vararg.sysv_x64;
        core.internal.vararg.sysv_x64.va_arg(ap, ti, parmn);
    }
    else version (AAPCS32)
    {
        const tsize = ti.tsize;
        if (ti.talign >= 8)
            ap.__ap = ap.__ap.alignUp!8;
        auto p = ap.__ap;
        version (BigEndian)
            p = adjustForBigEndian(p, tsize);
        ap.__ap += tsize.alignUp;
        parmn[0..tsize] = p[0..tsize];
    }
    else version (AAPCS64)
    {
        static import core.internal.vararg.aarch64;
        core.internal.vararg.aarch64.va_arg(ap, ti, parmn);
    }
    else version (ARM_Any)
    {
        const tsize = ti.tsize;
        auto p = cast(void*) ap;
        version (BigEndian)
            p = adjustForBigEndian(p, tsize);
        ap += tsize.alignUp;
        parmn[0..tsize] = p[0..tsize];
    }
    else version (PPC_Any)
    {
        if (ti.talign >= 8)
            ap = ap.alignUp!8;
        const tsize = ti.tsize;
        auto p = cast(void*) ap;
        version (BigEndian)
            p = adjustForBigEndian(p, tsize);
        ap += tsize.alignUp;
        parmn[0..tsize] = p[0..tsize];
    }
    else version (LoongArch64)
    {
        const tsize = ti.tsize;
        auto p = cast(void*) ap;
        ap += tsize.alignUp;
        parmn[0..tsize] = p[0..tsize];
    }
    else version (MIPS_Any)
    {
        const tsize = ti.tsize;
        auto p = cast(void*) ap;
        version (BigEndian)
            p = adjustForBigEndian(p, tsize);
        ap += tsize.alignUp;
        parmn[0..tsize] = p[0..tsize];
    }
    else version (RISCV_Any)
    {
        const tsize = ti.tsize;
        void* p;
        if (tsize > (size_t.sizeof << 1))
            p = *cast(void**) ap;
        else
        {
            if (tsize == (size_t.sizeof << 1))
                ap = ap.alignUp!(size_t.sizeof << 1);
            p = cast(void*) ap;
        }
        ap += tsize.alignUp;
        parmn[0..tsize] = p[0..tsize];
    }
    else
        static assert(0, "Unsupported platform");
}

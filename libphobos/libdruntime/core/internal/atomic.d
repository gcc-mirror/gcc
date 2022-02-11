/**
* The core.internal.atomic module comtains the low-level atomic features available in hardware.
* This module may be a routing layer for compiler intrinsics.
*
* Copyright: Copyright Manu Evans 2019.
* License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
* Authors:   Sean Kelly, Alex Rønne Petersen, Manu Evans
* Source:    $(DRUNTIMESRC core/internal/_atomic.d)
*/

module core.internal.atomic;

import core.atomic : MemoryOrder, has128BitCAS;

version (DigitalMars)
{
    private
    {
        enum : int
        {
            AX, BX, CX, DX, DI, SI, R8, R9
        }

        immutable string[4][8] registerNames = [
            [ "AL", "AX", "EAX", "RAX" ],
            [ "BL", "BX", "EBX", "RBX" ],
            [ "CL", "CX", "ECX", "RCX" ],
            [ "DL", "DX", "EDX", "RDX" ],
            [ "DIL", "DI", "EDI", "RDI" ],
            [ "SIL", "SI", "ESI", "RSI" ],
            [ "R8B", "R8W", "R8D", "R8" ],
            [ "R9B", "R9W", "R9D", "R9" ],
        ];

        template RegIndex(T)
        {
            static if (T.sizeof == 1)
                enum RegIndex = 0;
            else static if (T.sizeof == 2)
                enum RegIndex = 1;
            else static if (T.sizeof == 4)
                enum RegIndex = 2;
            else static if (T.sizeof == 8)
                enum RegIndex = 3;
            else
                static assert(false, "Invalid type");
        }

        enum SizedReg(int reg, T = size_t) = registerNames[reg][RegIndex!T];
    }

    inout(T) atomicLoad(MemoryOrder order = MemoryOrder.seq, T)(inout(T)* src) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        static assert(order != MemoryOrder.rel, "invalid MemoryOrder for atomicLoad()");

        static if (T.sizeof == size_t.sizeof * 2)
        {
            version (D_InlineAsm_X86)
            {
                asm pure nothrow @nogc @trusted
                {
                    push EDI;
                    push EBX;
                    mov EBX, 0;
                    mov ECX, 0;
                    mov EAX, 0;
                    mov EDX, 0;
                    mov EDI, src;
                    lock; cmpxchg8b [EDI];
                    pop EBX;
                    pop EDI;
                }
            }
            else version (D_InlineAsm_X86_64)
            {
                version (Windows)
                {
                    static if (RegisterReturn!T)
                    {
                        enum SrcPtr = SizedReg!CX;
                        enum RetPtr = null;
                    }
                    else
                    {
                        enum SrcPtr = SizedReg!DX;
                        enum RetPtr = SizedReg!CX;
                    }

                    mixin (simpleFormat(q{
                        asm pure nothrow @nogc @trusted
                        {
                            naked;
                            push RBX;
                            mov R8, %0;
    ?1                        mov R9, %1;
                            mov RBX, 0;
                            mov RCX, 0;
                            mov RAX, 0;
                            mov RDX, 0;
                            lock; cmpxchg16b [R8];
    ?1                        mov [R9], RAX;
    ?1                        mov 8[R9], RDX;
                            pop RBX;
                            ret;
                        }
                    }, [SrcPtr, RetPtr]));
                }
                else
                {
                    asm pure nothrow @nogc @trusted
                    {
                        naked;
                        push RBX;
                        mov RBX, 0;
                        mov RCX, 0;
                        mov RAX, 0;
                        mov RDX, 0;
                        lock; cmpxchg16b [RDI];
                        pop RBX;
                        ret;
                    }
                }
            }
        }
        else static if (needsLoadBarrier!order)
        {
            version (D_InlineAsm_X86)
            {
                enum SrcReg = SizedReg!CX;
                enum ZeroReg = SizedReg!(DX, T);
                enum ResReg = SizedReg!(AX, T);

                mixin (simpleFormat(q{
                    asm pure nothrow @nogc @trusted
                    {
                        mov %1, 0;
                        mov %2, 0;
                        mov %0, src;
                        lock; cmpxchg [%0], %1;
                    }
                }, [SrcReg, ZeroReg, ResReg]));
            }
            else version (D_InlineAsm_X86_64)
            {
                version (Windows)
                    enum SrcReg = SizedReg!CX;
                else
                    enum SrcReg = SizedReg!DI;
                enum ZeroReg = SizedReg!(DX, T);
                enum ResReg = SizedReg!(AX, T);

                mixin (simpleFormat(q{
                    asm pure nothrow @nogc @trusted
                    {
                        naked;
                        mov %1, 0;
                        mov %2, 0;
                        lock; cmpxchg [%0], %1;
                        ret;
                    }
                }, [SrcReg, ZeroReg, ResReg]));
            }
        }
        else
            return *src;
    }

    void atomicStore(MemoryOrder order = MemoryOrder.seq, T)(T* dest, T value) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        static assert(order != MemoryOrder.acq, "Invalid MemoryOrder for atomicStore()");

        static if (T.sizeof == size_t.sizeof * 2)
        {
            version (D_InlineAsm_X86)
            {
                asm pure nothrow @nogc @trusted
                {
                    push EDI;
                    push EBX;
                    lea EDI, value;
                    mov EBX, [EDI];
                    mov ECX, 4[EDI];
                    mov EDI, dest;
                    mov EAX, [EDI];
                    mov EDX, 4[EDI];
                L1: lock; cmpxchg8b [EDI];
                    jne L1;
                    pop EBX;
                    pop EDI;
                }
            }
            else version (D_InlineAsm_X86_64)
            {
                version (Windows)
                {
                    asm pure nothrow @nogc @trusted
                    {
                        naked;
                        push RBX;
                        mov R8, RDX;
                        mov RAX, [RDX];
                        mov RDX, 8[RDX];
                        mov RBX, [RCX];
                        mov RCX, 8[RCX];
                    L1: lock; cmpxchg16b [R8];
                        jne L1;
                        pop RBX;
                        ret;
                    }
                }
                else
                {
                    asm pure nothrow @nogc @trusted
                    {
                        naked;
                        push RBX;
                        mov RBX, RDI;
                        mov RCX, RSI;
                        mov RDI, RDX;
                        mov RAX, [RDX];
                        mov RDX, 8[RDX];
                    L1: lock; cmpxchg16b [RDI];
                        jne L1;
                        pop RBX;
                        ret;
                    }
                }
            }
        }
        else static if (needsStoreBarrier!order)
            atomicExchange!(order, false)(dest, value);
        else
            *dest = value;
    }

    T atomicFetchAdd(MemoryOrder order = MemoryOrder.seq, bool result = true, T)(T* dest, T value) pure nothrow @nogc @trusted
        if (is(T : ulong))
    {
        version (D_InlineAsm_X86)
        {
            static assert(T.sizeof <= 4, "64bit atomicFetchAdd not supported on 32bit target." );

            enum DestReg = SizedReg!DX;
            enum ValReg = SizedReg!(AX, T);

            mixin (simpleFormat(q{
                asm pure nothrow @nogc @trusted
                {
                    mov %1, value;
                    mov %0, dest;
                    lock; xadd[%0], %1;
                }
            }, [DestReg, ValReg]));
        }
        else version (D_InlineAsm_X86_64)
        {
            version (Windows)
            {
                enum DestReg = SizedReg!DX;
                enum ValReg = SizedReg!(CX, T);
            }
            else
            {
                enum DestReg = SizedReg!SI;
                enum ValReg = SizedReg!(DI, T);
            }
            enum ResReg = result ? SizedReg!(AX, T) : null;

            mixin (simpleFormat(q{
                asm pure nothrow @nogc @trusted
                {
                    naked;
                    lock; xadd[%0], %1;
    ?2                mov %2, %1;
                    ret;
                }
            }, [DestReg, ValReg, ResReg]));
        }
        else
            static assert (false, "Unsupported architecture.");
    }

    T atomicFetchSub(MemoryOrder order = MemoryOrder.seq, bool result = true, T)(T* dest, T value) pure nothrow @nogc @trusted
        if (is(T : ulong))
    {
        return atomicFetchAdd(dest, cast(T)-cast(IntOrLong!T)value);
    }

    T atomicExchange(MemoryOrder order = MemoryOrder.seq, bool result = true, T)(T* dest, T value) pure nothrow @nogc @trusted
    if (CanCAS!T)
    {
        version (D_InlineAsm_X86)
        {
            static assert(T.sizeof <= 4, "64bit atomicExchange not supported on 32bit target." );

            enum DestReg = SizedReg!CX;
            enum ValReg = SizedReg!(AX, T);

            mixin (simpleFormat(q{
                asm pure nothrow @nogc @trusted
                {
                    mov %1, value;
                    mov %0, dest;
                    xchg [%0], %1;
                }
            }, [DestReg, ValReg]));
        }
        else version (D_InlineAsm_X86_64)
        {
            version (Windows)
            {
                enum DestReg = SizedReg!DX;
                enum ValReg = SizedReg!(CX, T);
            }
            else
            {
                enum DestReg = SizedReg!SI;
                enum ValReg = SizedReg!(DI, T);
            }
            enum ResReg = result ? SizedReg!(AX, T) : null;

            mixin (simpleFormat(q{
                asm pure nothrow @nogc @trusted
                {
                    naked;
                    xchg [%0], %1;
    ?2                mov %2, %1;
                    ret;
                }
            }, [DestReg, ValReg, ResReg]));
        }
        else
            static assert (false, "Unsupported architecture.");
    }

    alias atomicCompareExchangeWeak = atomicCompareExchangeStrong;

    bool atomicCompareExchangeStrong(MemoryOrder succ = MemoryOrder.seq, MemoryOrder fail = MemoryOrder.seq, T)(T* dest, T* compare, T value) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        version (D_InlineAsm_X86)
        {
            static if (T.sizeof <= 4)
            {
                enum DestAddr = SizedReg!CX;
                enum CmpAddr = SizedReg!DI;
                enum Val = SizedReg!(DX, T);
                enum Cmp = SizedReg!(AX, T);

                mixin (simpleFormat(q{
                    asm pure nothrow @nogc @trusted
                    {
                        push %1;
                        mov %2, value;
                        mov %1, compare;
                        mov %3, [%1];
                        mov %0, dest;
                        lock; cmpxchg [%0], %2;
                        mov [%1], %3;
                        setz AL;
                        pop %1;
                    }
                }, [DestAddr, CmpAddr, Val, Cmp]));
            }
            else static if (T.sizeof == 8)
            {
                asm pure nothrow @nogc @trusted
                {
                    push EDI;
                    push EBX;
                    lea EDI, value;
                    mov EBX, [EDI];
                    mov ECX, 4[EDI];
                    mov EDI, compare;
                    mov EAX, [EDI];
                    mov EDX, 4[EDI];
                    mov EDI, dest;
                    lock; cmpxchg8b [EDI];
                    mov EDI, compare;
                    mov [EDI], EAX;
                    mov 4[EDI], EDX;
                    setz AL;
                    pop EBX;
                    pop EDI;
                }
            }
            else
                static assert(T.sizeof <= 8, "128bit atomicCompareExchangeStrong not supported on 32bit target." );
        }
        else version (D_InlineAsm_X86_64)
        {
            static if (T.sizeof <= 8)
            {
                version (Windows)
                {
                    enum DestAddr = SizedReg!R8;
                    enum CmpAddr = SizedReg!DX;
                    enum Val = SizedReg!(CX, T);
                }
                else
                {
                    enum DestAddr = SizedReg!DX;
                    enum CmpAddr = SizedReg!SI;
                    enum Val = SizedReg!(DI, T);
                }
                enum Res = SizedReg!(AX, T);

                mixin (simpleFormat(q{
                    asm pure nothrow @nogc @trusted
                    {
                        naked;
                        mov %3, [%1];
                        lock; cmpxchg [%0], %2;
                        jne compare_fail;
                        mov AL, 1;
                        ret;
                    compare_fail:
                        mov [%1], %3;
                        xor AL, AL;
                        ret;
                    }
                }, [DestAddr, CmpAddr, Val, Res]));
            }
            else
            {
                version (Windows)
                {
                    asm pure nothrow @nogc @trusted
                    {
                        naked;
                        push RBX;
                        mov R9, RDX;
                        mov RAX, [RDX];
                        mov RDX, 8[RDX];
                        mov RBX, [RCX];
                        mov RCX, 8[RCX];
                        lock; cmpxchg16b [R8];
                        pop RBX;
                        jne compare_fail;
                        mov AL, 1;
                        ret;
                    compare_fail:
                        mov [R9], RAX;
                        mov 8[R9], RDX;
                        xor AL, AL;
                        ret;
                    }
                }
                else
                {
                    asm pure nothrow @nogc @trusted
                    {
                        naked;
                        push RBX;
                        mov R8, RCX;
                        mov R9, RDX;
                        mov RAX, [RDX];
                        mov RDX, 8[RDX];
                        mov RBX, RDI;
                        mov RCX, RSI;
                        lock; cmpxchg16b [R8];
                        pop RBX;
                        jne compare_fail;
                        mov AL, 1;
                        ret;
                    compare_fail:
                        mov [R9], RAX;
                        mov 8[R9], RDX;
                        xor AL, AL;
                        ret;
                    }
                }
            }
        }
        else
            static assert (false, "Unsupported architecture.");
    }

    alias atomicCompareExchangeWeakNoResult = atomicCompareExchangeStrongNoResult;

    bool atomicCompareExchangeStrongNoResult(MemoryOrder succ = MemoryOrder.seq, MemoryOrder fail = MemoryOrder.seq, T)(T* dest, const T compare, T value) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        version (D_InlineAsm_X86)
        {
            static if (T.sizeof <= 4)
            {
                enum DestAddr = SizedReg!CX;
                enum Cmp = SizedReg!(AX, T);
                enum Val = SizedReg!(DX, T);

                mixin (simpleFormat(q{
                    asm pure nothrow @nogc @trusted
                    {
                        mov %2, value;
                        mov %1, compare;
                        mov %0, dest;
                        lock; cmpxchg [%0], %2;
                        setz AL;
                    }
                }, [DestAddr, Cmp, Val]));
            }
            else static if (T.sizeof == 8)
            {
                asm pure nothrow @nogc @trusted
                {
                    push EDI;
                    push EBX;
                    lea EDI, value;
                    mov EBX, [EDI];
                    mov ECX, 4[EDI];
                    lea EDI, compare;
                    mov EAX, [EDI];
                    mov EDX, 4[EDI];
                    mov EDI, dest;
                    lock; cmpxchg8b [EDI];
                    setz AL;
                    pop EBX;
                    pop EDI;
                }
            }
            else
                static assert(T.sizeof <= 8, "128bit atomicCompareExchangeStrong not supported on 32bit target." );
        }
        else version (D_InlineAsm_X86_64)
        {
            static if (T.sizeof <= 8)
            {
                version (Windows)
                {
                    enum DestAddr = SizedReg!R8;
                    enum Cmp = SizedReg!(DX, T);
                    enum Val = SizedReg!(CX, T);
                }
                else
                {
                    enum DestAddr = SizedReg!DX;
                    enum Cmp = SizedReg!(SI, T);
                    enum Val = SizedReg!(DI, T);
                }
                enum AXReg = SizedReg!(AX, T);

                mixin (simpleFormat(q{
                    asm pure nothrow @nogc @trusted
                    {
                        naked;
                        mov %3, %1;
                        lock; cmpxchg [%0], %2;
                        setz AL;
                        ret;
                    }
                }, [DestAddr, Cmp, Val, AXReg]));
            }
            else
            {
                version (Windows)
                {
                    asm pure nothrow @nogc @trusted
                    {
                        naked;
                        push RBX;
                        mov RAX, [RDX];
                        mov RDX, 8[RDX];
                        mov RBX, [RCX];
                        mov RCX, 8[RCX];
                        lock; cmpxchg16b [R8];
                        setz AL;
                        pop RBX;
                        ret;
                    }
                }
                else
                {
                    asm pure nothrow @nogc @trusted
                    {
                        naked;
                        push RBX;
                        mov RAX, RDX;
                        mov RDX, RCX;
                        mov RBX, RDI;
                        mov RCX, RSI;
                        lock; cmpxchg16b [R8];
                        setz AL;
                        pop RBX;
                        ret;
                    }
                }
            }
        }
        else
            static assert (false, "Unsupported architecture.");
    }

    void atomicFence(MemoryOrder order = MemoryOrder.seq)() pure nothrow @nogc @trusted
    {
        // TODO: `mfence` should only be required for seq_cst operations, but this depends on
        //       the compiler's backend knowledge to not reorder code inappropriately,
        //       so we'll apply it conservatively.
        static if (order != MemoryOrder.raw)
        {
            version (D_InlineAsm_X86)
            {
                import core.cpuid;

                // TODO: review this implementation; it seems way overly complicated
                asm pure nothrow @nogc @trusted
                {
                    naked;

                    call sse2;
                    test AL, AL;
                    jne Lcpuid;

                    // Fast path: We have SSE2, so just use mfence.
                    mfence;
                    jmp Lend;

                Lcpuid:

                    // Slow path: We use cpuid to serialize. This is
                    // significantly slower than mfence, but is the
                    // only serialization facility we have available
                    // on older non-SSE2 chips.
                    push EBX;

                    mov EAX, 0;
                    cpuid;

                    pop EBX;

                Lend:

                    ret;
                }
            }
            else version (D_InlineAsm_X86_64)
            {
                asm pure nothrow @nogc @trusted
                {
                    naked;
                    mfence;
                    ret;
                }
            }
            else
                static assert (false, "Unsupported architecture.");
        }
    }

    void pause() pure nothrow @nogc @trusted
    {
        version (D_InlineAsm_X86)
        {
            asm pure nothrow @nogc @trusted
            {
                naked;
                rep; nop;
                ret;
            }
        }
        else version (D_InlineAsm_X86_64)
        {
            asm pure nothrow @nogc @trusted
            {
                naked;
    //            pause; // TODO: DMD should add this opcode to its inline asm
                rep; nop;
                ret;
            }
        }
        else
        {
            // ARM should `yield`
            // other architectures? otherwise some sort of nop...
        }
    }
}
else version (GNU)
{
    import gcc.builtins;
    import gcc.config;

    inout(T) atomicLoad(MemoryOrder order = MemoryOrder.seq, T)(inout(T)* src) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        static assert(order != MemoryOrder.rel, "invalid MemoryOrder for atomicLoad()");

        static if (GNU_Have_Atomics || GNU_Have_LibAtomic)
        {
            static if (T.sizeof == ubyte.sizeof)
            {
                ubyte value = __atomic_load_1(cast(shared)src, order);
                return *cast(typeof(return)*)&value;
            }
            else static if (T.sizeof == ushort.sizeof)
            {
                ushort value = __atomic_load_2(cast(shared)src, order);
                return *cast(typeof(return)*)&value;
            }
            else static if (T.sizeof == uint.sizeof)
            {
                uint value = __atomic_load_4(cast(shared)src, order);
                return *cast(typeof(return)*)&value;
            }
            else static if (T.sizeof == ulong.sizeof && GNU_Have_64Bit_Atomics)
            {
                ulong value = __atomic_load_8(cast(shared)src, order);
                return *cast(typeof(return)*)&value;
            }
            else static if (GNU_Have_LibAtomic)
            {
                T value;
                __atomic_load(T.sizeof, cast(shared)src, &value, order);
                return *cast(typeof(return)*)&value;
            }
            else
                static assert(0, "Invalid template type specified.");
        }
        else
        {
            getAtomicMutex.lock();
            scope(exit) getAtomicMutex.unlock();
            return *cast(typeof(return)*)&src;
        }
    }

    void atomicStore(MemoryOrder order = MemoryOrder.seq, T)(T* dest, T value) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        static assert(order != MemoryOrder.acq, "Invalid MemoryOrder for atomicStore()");

        static if (GNU_Have_Atomics || GNU_Have_LibAtomic)
        {
            static if (T.sizeof == ubyte.sizeof)
                __atomic_store_1(cast(shared)dest, *cast(ubyte*)&value, order);
            else static if (T.sizeof == ushort.sizeof)
                __atomic_store_2(cast(shared)dest, *cast(ushort*)&value, order);
            else static if (T.sizeof == uint.sizeof)
                __atomic_store_4(cast(shared)dest, *cast(uint*)&value, order);
            else static if (T.sizeof == ulong.sizeof && GNU_Have_64Bit_Atomics)
                __atomic_store_8(cast(shared)dest, *cast(ulong*)&value, order);
            else static if (GNU_Have_LibAtomic)
                __atomic_store(T.sizeof, cast(shared)dest, cast(void*)&value, order);
            else
                static assert(0, "Invalid template type specified.");
        }
        else
        {
            getAtomicMutex.lock();
            *dest = value;
            getAtomicMutex.unlock();
        }
    }

    T atomicFetchAdd(MemoryOrder order = MemoryOrder.seq, bool result = true, T)(T* dest, T value) pure nothrow @nogc @trusted
        if (is(T : ulong))
    {
        static if (GNU_Have_Atomics || GNU_Have_LibAtomic)
        {
            static if (T.sizeof == ubyte.sizeof)
                return __atomic_fetch_add_1(cast(shared)dest, value, order);
            else static if (T.sizeof == ushort.sizeof)
                return __atomic_fetch_add_2(cast(shared)dest, value, order);
            else static if (T.sizeof == uint.sizeof)
                return __atomic_fetch_add_4(cast(shared)dest, value, order);
            else static if (T.sizeof == ulong.sizeof && GNU_Have_64Bit_Atomics)
                return __atomic_fetch_add_8(cast(shared)dest, value, order);
            else static if (GNU_Have_LibAtomic)
                return __atomic_fetch_add(T.sizeof, cast(shared)dest, cast(void*)&value, order);
            else
                static assert(0, "Invalid template type specified.");
        }
        else
        {
            getAtomicMutex.lock();
            scope(exit) getAtomicMutex.unlock();
            T tmp = *dest;
            *dest += value;
            return tmp;
        }
    }

    T atomicFetchSub(MemoryOrder order = MemoryOrder.seq, bool result = true, T)(T* dest, T value) pure nothrow @nogc @trusted
        if (is(T : ulong))
    {
        static if (GNU_Have_Atomics || GNU_Have_LibAtomic)
        {
            static if (T.sizeof == ubyte.sizeof)
                return __atomic_fetch_sub_1(cast(shared)dest, value, order);
            else static if (T.sizeof == ushort.sizeof)
                return __atomic_fetch_sub_2(cast(shared)dest, value, order);
            else static if (T.sizeof == uint.sizeof)
                return __atomic_fetch_sub_4(cast(shared)dest, value, order);
            else static if (T.sizeof == ulong.sizeof && GNU_Have_64Bit_Atomics)
                return __atomic_fetch_sub_8(cast(shared)dest, value, order);
            else static if (GNU_Have_LibAtomic)
                return __atomic_fetch_sub(T.sizeof, cast(shared)dest, cast(void*)&value, order);
            else
                static assert(0, "Invalid template type specified.");
        }
        else
        {
            getAtomicMutex.lock();
            scope(exit) getAtomicMutex.unlock();
            T tmp = *dest;
            *dest -= value;
            return tmp;
        }
    }

    T atomicExchange(MemoryOrder order = MemoryOrder.seq, bool result = true, T)(T* dest, T value) pure nothrow @nogc @trusted
        if (is(T : ulong) || is(T == class) || is(T == interface) || is(T U : U*))
    {
        static if (GNU_Have_Atomics || GNU_Have_LibAtomic)
        {
            static if (T.sizeof == byte.sizeof)
            {
                ubyte res = __atomic_exchange_1(cast(shared)dest, *cast(ubyte*)&value, order);
                return *cast(typeof(return)*)&res;
            }
            else static if (T.sizeof == short.sizeof)
            {
                ushort res = __atomic_exchange_2(cast(shared)dest, *cast(ushort*)&value, order);
                return *cast(typeof(return)*)&res;
            }
            else static if (T.sizeof == int.sizeof)
            {
                uint res = __atomic_exchange_4(cast(shared)dest, *cast(uint*)&value, order);
                return *cast(typeof(return)*)&res;
            }
            else static if (T.sizeof == long.sizeof && GNU_Have_64Bit_Atomics)
            {
                ulong res = __atomic_exchange_8(cast(shared)dest, *cast(ulong*)&value, order);
                return *cast(typeof(return)*)&res;
            }
            else static if (GNU_Have_LibAtomic)
            {
                T res = void;
                __atomic_exchange(T.sizeof, cast(shared)dest, cast(void*)&value, &res, order);
                return res;
            }
            else
                static assert(0, "Invalid template type specified.");
        }
        else
        {
            getAtomicMutex.lock();
            scope(exit) getAtomicMutex.unlock();

            T res = *dest;
            *dest = value;
            return res;
        }
    }

    bool atomicCompareExchangeWeak(MemoryOrder succ = MemoryOrder.seq, MemoryOrder fail = MemoryOrder.seq, T)(T* dest, T* compare, T value) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        return atomicCompareExchangeImpl!(succ, fail, true)(dest, compare, value);
    }

    bool atomicCompareExchangeStrong(MemoryOrder succ = MemoryOrder.seq, MemoryOrder fail = MemoryOrder.seq, T)(T* dest, T* compare, T value) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        return atomicCompareExchangeImpl!(succ, fail, false)(dest, compare, value);
    }

    bool atomicCompareExchangeStrongNoResult(MemoryOrder succ = MemoryOrder.seq, MemoryOrder fail = MemoryOrder.seq, T)(T* dest, const T compare, T value) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        return atomicCompareExchangeImpl!(succ, fail, false)(dest, cast(T*)&compare, value);
    }

    bool atomicCompareExchangeWeakNoResult(MemoryOrder succ = MemoryOrder.seq, MemoryOrder fail = MemoryOrder.seq, T)(T* dest, const T compare, T value) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        return atomicCompareExchangeImpl!(succ, fail, true)(dest, cast(T*)&compare, value);
    }

    private bool atomicCompareExchangeImpl(MemoryOrder succ = MemoryOrder.seq, MemoryOrder fail = MemoryOrder.seq, bool weak, T)(T* dest, T* compare, T value) pure nothrow @nogc @trusted
        if (CanCAS!T)
    {
        bool res = void;

        static if (GNU_Have_Atomics || GNU_Have_LibAtomic)
        {
            static if (T.sizeof == byte.sizeof)
                res = __atomic_compare_exchange_1(cast(shared)dest, compare, *cast(ubyte*)&value,
                                                  weak, succ, fail);
            else static if (T.sizeof == short.sizeof)
                res = __atomic_compare_exchange_2(cast(shared)dest, compare, *cast(ushort*)&value,
                                                  weak, succ, fail);
            else static if (T.sizeof == int.sizeof)
                res = __atomic_compare_exchange_4(cast(shared)dest, compare, *cast(uint*)&value,
                                                  weak, succ, fail);
            else static if (T.sizeof == long.sizeof && GNU_Have_64Bit_Atomics)
                res = __atomic_compare_exchange_8(cast(shared)dest, compare, *cast(ulong*)&value,
                                                  weak, succ, fail);
            else static if (GNU_Have_LibAtomic)
                res = __atomic_compare_exchange(T.sizeof, cast(shared)dest, compare, cast(void*)&value,
                                                succ, fail);
            else
                static assert(0, "Invalid template type specified.");
        }
        else
        {
            static if (T.sizeof == byte.sizeof)
                alias U = byte;
            else static if (T.sizeof == short.sizeof)
                alias U = short;
            else static if (T.sizeof == int.sizeof)
                alias U = int;
            else static if (T.sizeof == long.sizeof)
                alias U = long;
            else
                static assert(0, "Invalid template type specified.");

            getAtomicMutex.lock();
            scope(exit) getAtomicMutex.unlock();

            if (*cast(U*)dest == *cast(U*)&compare)
            {
                *dest = value;
                res = true;
            }
            else
            {
                *compare = *dest;
                res = false;
            }
        }

        return res;
    }

    void atomicFence(MemoryOrder order = MemoryOrder.seq)() pure nothrow @nogc @trusted
    {
        static if (GNU_Have_Atomics || GNU_Have_LibAtomic)
            __atomic_thread_fence(order);
        else
        {
            getAtomicMutex.lock();
            getAtomicMutex.unlock();
        }
    }

    void pause() pure nothrow @nogc @trusted
    {
        version (X86)
        {
            __builtin_ia32_pause();
        }
        else version (X86_64)
        {
            __builtin_ia32_pause();
        }
        else
        {
            // Other architectures? Some sort of nop or barrier.
        }
    }

    static if (!GNU_Have_Atomics && !GNU_Have_LibAtomic)
    {
        // Use system mutex for atomics, faking the purity of the functions so
        // that they can be used in pure/nothrow/@safe code.
        extern (C) private pure @trusted @nogc nothrow
        {
            static if (GNU_Thread_Model == ThreadModel.Posix)
            {
                import core.sys.posix.pthread;
                alias atomicMutexHandle = pthread_mutex_t;

                pragma(mangle, "pthread_mutex_init") int fakePureMutexInit(pthread_mutex_t*, pthread_mutexattr_t*);
                pragma(mangle, "pthread_mutex_lock") int fakePureMutexLock(pthread_mutex_t*);
                pragma(mangle, "pthread_mutex_unlock") int fakePureMutexUnlock(pthread_mutex_t*);
            }
            else static if (GNU_Thread_Model == ThreadModel.Win32)
            {
                import core.sys.windows.winbase;
                alias atomicMutexHandle = CRITICAL_SECTION;

                pragma(mangle, "InitializeCriticalSection") int fakePureMutexInit(CRITICAL_SECTION*);
                pragma(mangle, "EnterCriticalSection") void fakePureMutexLock(CRITICAL_SECTION*);
                pragma(mangle, "LeaveCriticalSection") int fakePureMutexUnlock(CRITICAL_SECTION*);
            }
            else
            {
                alias atomicMutexHandle = int;
            }
        }

        // Implements lock/unlock operations.
        private struct AtomicMutex
        {
            int lock() pure @trusted @nogc nothrow
            {
                static if (GNU_Thread_Model == ThreadModel.Posix)
                {
                    if (!_inited)
                    {
                        fakePureMutexInit(&_handle, null);
                        _inited = true;
                    }
                    return fakePureMutexLock(&_handle);
                }
                else
                {
                    static if (GNU_Thread_Model == ThreadModel.Win32)
                    {
                        if (!_inited)
                        {
                            fakePureMutexInit(&_handle);
                            _inited = true;
                        }
                        fakePureMutexLock(&_handle);
                    }
                    return 0;
                }
            }

            int unlock() pure @trusted @nogc nothrow
            {
                static if (GNU_Thread_Model == ThreadModel.Posix)
                    return fakePureMutexUnlock(&_handle);
                else
                {
                    static if (GNU_Thread_Model == ThreadModel.Win32)
                        fakePureMutexUnlock(&_handle);
                    return 0;
                }
            }

        private:
            atomicMutexHandle _handle;
            bool _inited;
        }

        // Internal static mutex reference.
        private AtomicMutex* _getAtomicMutex() @trusted @nogc nothrow
        {
            __gshared static AtomicMutex mutex;
            return &mutex;
        }

        // Pure alias for _getAtomicMutex.
        pragma(mangle, _getAtomicMutex.mangleof)
        private AtomicMutex* getAtomicMutex() pure @trusted @nogc nothrow @property;
    }
}

private:

version (Windows)
{
    enum RegisterReturn(T) = is(T : U[], U) || is(T : R delegate(A), R, A...);
}

enum CanCAS(T) = is(T : ulong) ||
                 is(T == class) ||
                 is(T == interface) ||
                 is(T : U*, U) ||
                 is(T : U[], U) ||
                 is(T : R delegate(A), R, A...) ||
                 (is(T == struct) && __traits(isPOD, T) &&
                  (T.sizeof <= size_t.sizeof*2 ||       // no more than 2 words
                   (T.sizeof == 16 && has128BitCAS)) && // or supports 128-bit CAS
                  (T.sizeof & (T.sizeof - 1)) == 0      // is power of 2
                 );

template IntOrLong(T)
{
    static if (T.sizeof > 4)
        alias IntOrLong = long;
    else
        alias IntOrLong = int;
}

// NOTE: x86 loads implicitly have acquire semantics so a memory
//       barrier is only necessary on releases.
template needsLoadBarrier( MemoryOrder ms )
{
    enum bool needsLoadBarrier = ms == MemoryOrder.seq;
}


// NOTE: x86 stores implicitly have release semantics so a memory
//       barrier is only necessary on acquires.
template needsStoreBarrier( MemoryOrder ms )
{
    enum bool needsStoreBarrier = ms == MemoryOrder.seq;
}

// this is a helper to build asm blocks
string simpleFormat(string format, scope string[] args)
{
    string result;
    outer: while (format.length)
    {
        foreach (i; 0 .. format.length)
        {
            if (format[i] == '%' || format[i] == '?')
            {
                bool isQ = format[i] == '?';
                result ~= format[0 .. i++];
                assert (i < format.length, "Invalid format string");
                if (format[i] == '%' || format[i] == '?')
                {
                    assert(!isQ, "Invalid format string");
                    result ~= format[i++];
                }
                else
                {
                    int index = 0;
                    assert (format[i] >= '0' && format[i] <= '9', "Invalid format string");
                    while (i < format.length && format[i] >= '0' && format[i] <= '9')
                        index = index * 10 + (ubyte(format[i++]) - ubyte('0'));
                    if (!isQ)
                        result ~= args[index];
                    else if (!args[index])
                    {
                        size_t j = i;
                        for (; j < format.length;)
                        {
                            if (format[j++] == '\n')
                                break;
                        }
                        i = j;
                    }
                }
                format = format[i .. $];
                continue outer;
            }
        }
        result ~= format;
        break;
    }
    return result;
}

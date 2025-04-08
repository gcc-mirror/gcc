module std.internal.windows.bcrypt;

version (Windows):

import core.sys.windows.bcrypt : BCryptGenRandom, BCRYPT_USE_SYSTEM_PREFERRED_RNG;
import core.sys.windows.windef : HMODULE, PUCHAR, ULONG;
import core.sys.windows.ntdef : NT_SUCCESS;

pragma(lib, "Bcrypt.lib");

package(std) bool bcryptGenRandom(T)(out T result) @trusted
{
    loadBcrypt();

    const gotRandom = ptrBCryptGenRandom(
        null,
        cast(PUCHAR) &result,
        ULONG(T.sizeof),
        BCRYPT_USE_SYSTEM_PREFERRED_RNG,
    );

    return NT_SUCCESS(gotRandom);
}

private
{
    HMODULE hBcrypt = null;
    typeof(BCryptGenRandom)* ptrBCryptGenRandom;
}

private void loadBcrypt() @nogc nothrow
{
    import core.sys.windows.winbase : GetProcAddress, LoadLibraryA;

    if (!hBcrypt)
    {
        hBcrypt = LoadLibraryA("Bcrypt.dll");
        if (!hBcrypt)
            assert(false, `LoadLibraryA("Bcrypt.dll") failed.`); // `@nogc`

        ptrBCryptGenRandom = cast(typeof(ptrBCryptGenRandom)) GetProcAddress(hBcrypt , "BCryptGenRandom");
        if (!ptrBCryptGenRandom)
            assert(false, `GetProcAddress(hBcrypt , "BCryptGenRandom") failed.`); // `@nogc`
    }
}

// Will free `Bcrypt.dll`.
private void freeBcrypt() @nogc nothrow
{
    import core.sys.windows.winbase : FreeLibrary;

    if (hBcrypt)
    {
        if (!FreeLibrary(hBcrypt))
            assert(false, `FreeLibrary("Bcrypt.dll") failed.`); // `@nogc`

        hBcrypt = null;
        ptrBCryptGenRandom = null;
    }
}

static ~this()
{
    freeBcrypt();
}

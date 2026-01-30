// Written in the D programming language.

/+
    CSPRNG library prototype.

    This code has not been audited.
    Do not use for cryptographic purposes.

    The terms $(I entropy) and $(I entropy sources) here do refer to
    cryptographically-safe random numbers and higher-level generators of such
    — typically powered by an entropy pool provided by the operating system.

    An example of similar usage of said terminology would be the `getentropy()`
    function provided by
    $(LINK2 https://man.freebsd.org/cgi/man.cgi?query=getentropy&apropos=0&sektion=3&manpath=FreeBSD+14.2-RELEASE&arch=default&format=html,
    FreeBSD).

    This library does not interact with any actual low-level entropy sources
    by itself. Instead it interfaces with system-provided CSPRNGs that are
    typically seeded through aforementioned entropy sources by the operating
    system as needed.

    See_also:
        $(LINK https://blog.cr.yp.to/20140205-entropy.html),
        $(LINK https://cr.yp.to/talks/2014.10.18/slides-djb-20141018-a4.pdf)

    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Elias Batek
    Source:    $(PHOBOSSRC std/internal/entropy.d)
 +/
module std.internal.entropy;

import std.meta;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

// Self-test: Detect potentially unsuitable default entropy source.
@safe unittest
{
    auto buffer = new ubyte[](32);
    forceEntropySource(defaultEntropySource);
    const result = getEntropy(buffer);

    assert(
        !result.isUnavailable,
        "The default entropy source for the target platform"
        ~ " is unavailable on this machine. Please consider"
        ~ " patching it to accommodate to your environment."
    );
    assert(result.isOK);
}

// Self-test: Detect faulty implementation.
@system unittest
{
    forceEntropySource(defaultEntropySource);

    bool test() @system
    {
        static immutable pattern = 0xDEAD_BEEF_1337_0000;
        long number = pattern;
        const result = getEntropy(&number, number.sizeof);
        assert(result.isOK);
        return number != pattern;
    }

    size_t timesFailed = 0;
    foreach (n; 0 .. 3)
        if (!test())
            ++timesFailed;

    assert(
        timesFailed <= 1,
        "Suspicious random data: Potential security issue or really unlucky; please retry."
    );
}

// Self-test: Detect faulty implementation.
@safe unittest
{
    forceEntropySource(defaultEntropySource);

    bool test() @safe
    {
        ubyte[32] data;
        data[] = 0;

        const result = getEntropy(data[]);
        assert(result.isOK);

        size_t zeros = 0;
        foreach (b; data)
            if (b == 0)
                ++zeros;

        enum threshold = 24;
        return zeros < threshold;
    }

    size_t timesFailed = 0;
    foreach (n; 0 .. 3)
        if (!test())
            ++timesFailed;

    assert(
        timesFailed <= 1,
        "Suspicious random data: Potential security issue or really unlucky; please retry."
    );
}

@nogc nothrow:

// Flagship function
/++
    Retrieves random data from an applicable system CSPRNG.

    Params:
        buffer = An output buffer to store the retrieved entropy in.
                 The length of it will determine the amount of random data to
                 be obtained.

                 This function (and all overloads) always attempt to fill
                 the entire buffer. Therefore, they can block, spin or report
                 an error.

    Returns:
        An `EntropyResult` that either reports success
        or the type of error that has occurred.

        In case of an error, the data in `buffer` MUST NOT be used.
        The recommended way to check for success is through the `isOK()`
        helper function.
 +/
EntropyResult getEntropy(scope void[] buffer) @safe
{
    return getEntropyImpl(buffer);
}

///
@safe unittest
{
    int[4] bytes;
    if (getEntropy(cast(void[]) bytes).isOK)
    {
        // Success; data in `bytes` may be used.
    }

    assert((cast(void[]) bytes).length == bytes.sizeof);
}

// Convenience overload
/// ditto
EntropyResult getEntropy(scope ubyte[] buffer) @safe
{
    return getEntropy(cast(void[]) buffer);
}

///
@safe unittest
{
    ubyte[16] bytes;
    if (getEntropy(bytes).isOK)
    {
        // Success; data in `bytes` may be used.
    }
}

// Convenience wrapper
/// ditto
/++
    Retrieves random data from an applicable system CSPRNG.

    Params:
        buffer = An output buffer to store the retrieved entropy in.
        length = Length of the provided `buffer`.
                 Specifying a wrong value here, will lead to memory corruption.

    Returns:
        An `EntropyResult` that either reports success
        or the type of error that has occurred.

        In case of an error, the data in `buffer` MUST NOT be used.
        The recommended way to check for success is through the `isOK()`
        helper function.
 +/
EntropyResult getEntropy(scope void* buffer, size_t length) @system
{
    return getEntropy(buffer[0 .. length]);
}

///
@system unittest
{
    ubyte[16] bytes;
    if (getEntropy(cast(void*) bytes.ptr, bytes.length).isOK)
    {
        // Success; data in `bytes` may be used.
    }
}

///
@system unittest
{
    int number = void;
    if (getEntropy(&number, number.sizeof).isOK)
    {
        // Success; value of `number` may be used.
    }
}

/++
    Manually set the entropy source to use for the current thread.

    As a rule of thumb, this SHOULD NOT be done.

    It might be useful in cases where the default entropy source — as chosen by
    the maintainer of the used compiler package — is unavailable on a system.
    Usually, `EntropySource.tryAll` will be the most reasonable option
    in such cases.

    Params:
        source = The requested default entropy source to use for the current thread.

    Examples:

    ---
    // Using `forceEntropySource` almost always is a bad idea.
    // As a rule of thumb, this SHOULD NOT be done.
    forceEntropySource(EntropySource.none);
    ---
 +/
void forceEntropySource(EntropySource source) @safe
{
    _entropySource = source;
}

// (In-)Convenience wrapper
/++
    Retrieves random data from the requested entropy source.

    In general, it’s a $(B bad idea) to let users pick sources themselves.
    A sane option should be used by default instead.

    This overload only exists because its used by Phobos.

    See_also:
        Use `forceEntropySource` instead.

    Params:
        buffer = An output buffer to store the retrieved entropy in.
                 The length of it will determine the amount of entropy to be
                 obtained.
        length = Length of the provided `buffer`.
                 Specifying a wrong value here, will lead to memory corruption.
        source = The entropy source to use for the operation.

    Returns:
        An `EntropyResult` that either reports success
        or the type of error that has occurred.

        In case of an error, the data in `buffer` MUST NOT be used.
        The recommended way to check for success is through the `isOK()`
        helper function.
 +/
EntropyResult getEntropy(scope void* buffer, size_t length, EntropySource source) @system
{
    const sourcePrevious = _entropySource;
    scope (exit) _entropySource = sourcePrevious;

    _entropySource = source;
    return getEntropy(buffer[0 .. length]);
}

///
@system unittest
{
    ubyte[4] bytes;

    // `EntropySource.none` always fails.
    assert(!getEntropy(bytes.ptr, bytes.length, EntropySource.none).isOK);
}

/++
    A CSPRNG suitable to retrieve cryptographically-secure random data from.

    (No actual low-level entropy sources are provided on purpose.)
 +/
enum EntropySource
{
    /// Implements a $(I hunting) strategy for finding an entropy source that
    /// is available at runtime.
    ///
    /// Try supported sources one-by-one until one is available.
    /// This exists to enable the use of this the entropy library
    /// in a backwards compatibility way.
    ///
    /// It is recommended against using this in places that do not strictly
    /// have to to meet compatibility requirements.
    /// Like any kind of crypto-agility, this approach may suffer from
    /// practical issues.
    ///
    /// See_also:
    /// While the following article focuses on cipher agility in protocols,
    /// it elaborates why agility can lead to problems:
    /// $(LINK https://web.archive.org/web/20191102211148/https://paragonie.com/blog/2019/10/against-agility-in-cryptography-protocols)
    tryAll = -1,

    /// Always fail.
    none = 0,

    /// `/dev/urandom`
    charDevURandom = 1,

    /// `/dev/random`
    charDevRandom = 2,

    /// `getrandom` syscall or wrapper
    getrandom = 3,

    /// `arc4random`
    arc4random = 4,

    // `getentropy`
    getentropy = 5,

    /// Windows legacy CryptoAPI
    cryptGenRandom = 6,

    /// Windows Cryptography API: Next Generation (“BCrypt”)
    bcryptGenRandom = 7,
}

///
enum EntropyStatus
{
    /// success
    ok = 0,

    /// catch-all error
    unknownError = 1,

    /// An entropy source was unavailable.
    unavailable,

    /// A dependency providing the entropy source turned out unavailable.
    unavailableLibrary,

    /// The requested entropy source is not supported on this platform.
    unavailablePlatform,

    /// Could not retrieve entropy from the selected source.
    readError,
}

/++
    Status report returned by `getEntropy` functions.

    Use the `isOK` helper function to test for success.
 +/
struct EntropyResult
{
    ///
    EntropyStatus status;

    ///
    EntropySource source;

    /++
        Returns:
            A human-readable status message.
     +/
    string toString() const @nogc nothrow pure @safe
    {
        if (status == EntropyStatus.ok)
            return "getEntropy(): OK.";

        if (source == EntropySource.none)
        {
            if (status == EntropyStatus.unavailable)
                return "getEntropy(): Error - No suitable entropy source was available.";
        }
        else if (source == EntropySource.getrandom)
        {
            if (status == EntropyStatus.unavailableLibrary)
                return "getEntropy(): `dlopen(\"libc\")` failed.";
            if (status == EntropyStatus.unavailable)
                return "getEntropy(): `dlsym(\"libc\", \"getrandom\")` failed.";
            if (status == EntropyStatus.readError)
                return "getEntropy(): `getrandom()` failed.";
        }
        else if (source == EntropySource.getentropy)
        {
            if (status == EntropyStatus.readError)
                return "getEntropy(): `getentropy()` failed.";
        }
        else if (source == EntropySource.charDevURandom)
        {
            if (status == EntropyStatus.unavailable)
                return "getEntropy(): `/dev/urandom` is unavailable.";
            if (status == EntropyStatus.readError)
                return "getEntropy(): Reading from `/dev/urandom` failed.";
        }
        else if (source == EntropySource.charDevURandom)
        {
            if (status == EntropyStatus.unavailable)
                return "getEntropy(): `/dev/random` is unavailable.";
            if (status == EntropyStatus.readError)
                return "getEntropy(): Reading from `/dev/random` failed.";
        }
        else if (source == EntropySource.bcryptGenRandom)
        {
            if (status == EntropyStatus.unavailableLibrary)
                return "getEntropy(): `LoadLibraryA(\"Bcrypt.dll\")` failed.";
            if (status == EntropyStatus.unavailable)
                return "getEntropy(): `GetProcAddress(hBcrypt , \"BCryptGenRandom\")` failed.";
            if (status == EntropyStatus.readError)
                return "getEntropy(): `BCryptGenRandom()` failed.";
        }

        // generic errors
        {
            if (status == EntropyStatus.unavailable ||
                status == EntropyStatus.unavailableLibrary)
                return "getEntropy(): An entropy source was unavailable.";
            if (status == EntropyStatus.unavailablePlatform)
                return "getEntropy(): The requested entropy source is not supported on this platform.";
            if (status == EntropyStatus.readError)
                return "getEntropy(): Could not retrieve entropy from the selected source.";

            return "getEntropy(): An unknown error occurred.";
        }
    }
}

///
@safe unittest
{
    ubyte[4] data;
    EntropyResult result = getEntropy(data[]);

    if (result.isOK)
    {
        // Success; data in `bytes` may be used.
    }
    else
    {
        // Failure

        if (result.isUnavailable)
        {
            // System’s entropy source was unavailable.
        }

        // Call `toString` to obtain a user-readable error message.
        assert(result.toString() !is null);
        assert(result.toString().length > 0);
    }
}

/++
    Determines whether an `EntropyResult` reports the success of an operation.

    Params:
        value = test subject

    Returns:
        `true` on success
 +/
pragma(inline, true) bool isOK(const EntropyResult value) pure @safe
{
    return value.status == EntropyStatus.ok;
}

/++
    Determines whether an `EntropyResult` reports the unvailability of the
    requested entropy source.

    Params:
        value = test subject

    Returns:
        `true` if entropy source requested to use with the operation was unavailable.
 +/
pragma(inline, true) bool isUnavailable(const EntropyResult value) pure @safe
{
    return (
        value.status == EntropyStatus.unavailable ||
        value.status == EntropyStatus.unavailableLibrary ||
        value.status == EntropyStatus.unavailablePlatform
    );
}

package(std):

// If the system let us down, we'll let the system down.
pragma(inline, true) void crashOnError(const EntropyResult value) pure @safe
{
    if (value.isOK)
        return;

    assert(false, value.toString());
}

/+
    Building blocks and implementation helpers
 +/
private
{
    /++
        A “Chunks” implementation that works with `void[]`.
     +/
    struct VoidChunks
    {
        void[] _data;
        size_t _chunkSize;

    @nogc nothrow pure @safe:

        this(void[] data, size_t chunkSize)
        {
            _data = data;
            _chunkSize = chunkSize;
        }

        bool empty() const
        {
            return _data.length == 0;
        }

        inout(void)[] front() inout
        {
            if (_data.length < _chunkSize)
                return _data;

            return _data[0 .. _chunkSize];
        }

        void popFront()
        {
            if (_data.length <= _chunkSize)
            {
                _data = null;
                return;
            }

            _data = _data[_chunkSize .. $];
        }
    }

    struct SrcFunPair(EntropySource source, alias func)
    {
        enum  src = source;
        alias fun = func;
    }

    template isValidSupportedSource(SupportedSource)
    {
        enum isValidSupportedSource = (
            is(SupportedSource == SrcFunPair!Args, Args...) &&
            SupportedSource.src != EntropySource.tryAll &&
            SupportedSource.src != EntropySource.none
        );
    }

    /++
        `getEntropyImpl()` implementation helper.
        To be instantiated and mixed in with platform-specific configuration.

        Params:
            defaultSource = Default entropy source of the platform
            SupportedSources = Sequence of `SrcFunPair`
                               representing the supported sources of the platform
    +/
    mixin template entropyImpl(EntropySource defaultSource, SupportedSources...)
    if (allSatisfy!(isValidSupportedSource, SupportedSources))
    {
    private:
        /// Preconfigured entropy source preset of the platform.
        enum defaultEntropySource = defaultSource;

        EntropyResult getEntropyImpl(scope void[] buffer) @safe
        {
            switch (_entropySource)
            {
                static foreach (source; SupportedSources)
                {
                    case source.src:
                        return source.fun(buffer);
                }

            case EntropySource.tryAll:
                {
                    const result = _tryEntropySources(buffer);
                    result.saveSourceForNextUse();
                    return result;
                }

            case EntropySource.none:
                return getEntropyViaNone(buffer);

            default:
                return EntropyResult(EntropyStatus.unavailablePlatform, _entropySource);
            }
        }

        EntropyResult _tryEntropySources(scope void[] buffer) @safe
        {
            EntropyResult result;

            static foreach (source; SupportedSources)
            {
                result = source.fun(buffer);
                if (!result.isUnavailable)
                    return result;
            }

            result = EntropyResult(
                EntropyStatus.unavailable,
                EntropySource.none,
            );

            return result;
        }
    }
}

version (Darwin) mixin entropyImpl!(
    EntropySource.arc4random,
    SrcFunPair!(EntropySource.arc4random, getEntropyViaARC4Random),
    SrcFunPair!(EntropySource.charDevURandom, getEntropyViaCharDevURandom),
    SrcFunPair!(EntropySource.charDevRandom, getEntropyViaCharDevRandom),
);
else version (DragonFlyBSD) mixin entropyImpl!(
    EntropySource.getentropy,
    SrcFunPair!(EntropySource.getentropy, getEntropyViaGetentropy),
    SrcFunPair!(EntropySource.charDevURandom, getEntropyViaCharDevURandom),
    SrcFunPair!(EntropySource.charDevRandom, getEntropyViaCharDevRandom),
);
else version (FreeBSD) mixin entropyImpl!(
    EntropySource.getentropy,
    SrcFunPair!(EntropySource.getentropy, getEntropyViaGetentropy),
    SrcFunPair!(EntropySource.charDevURandom, getEntropyViaCharDevURandom),
    SrcFunPair!(EntropySource.charDevRandom, getEntropyViaCharDevRandom),
);
else version (linux) mixin entropyImpl!(
    EntropySource.getrandom,
    SrcFunPair!(EntropySource.getrandom, getEntropyViaGetrandom),
    SrcFunPair!(EntropySource.charDevURandom, getEntropyViaCharDevURandom),
    SrcFunPair!(EntropySource.charDevRandom, getEntropyViaCharDevRandom),
);
else version (NetBSD) mixin entropyImpl!(
    EntropySource.arc4random,
    SrcFunPair!(EntropySource.arc4random, getEntropyViaARC4Random),
    SrcFunPair!(EntropySource.charDevURandom, getEntropyViaCharDevURandom),
    SrcFunPair!(EntropySource.charDevRandom, getEntropyViaCharDevRandom),
);
else version (OpenBSD) mixin entropyImpl!(
    EntropySource.arc4random,
    SrcFunPair!(EntropySource.arc4random, getEntropyViaARC4Random),
    SrcFunPair!(EntropySource.charDevURandom, getEntropyViaCharDevURandom),
    SrcFunPair!(EntropySource.charDevRandom, getEntropyViaCharDevRandom),
);
else version (Posix) mixin entropyImpl!(
    EntropySource.charDevURandom,
    SrcFunPair!(EntropySource.charDevURandom, getEntropyViaCharDevURandom),
    SrcFunPair!(EntropySource.charDevRandom, getEntropyViaCharDevRandom),
);
else version (Windows) mixin entropyImpl!(
    EntropySource.bcryptGenRandom,
    SrcFunPair!(EntropySource.bcryptGenRandom, getEntropyViaBCryptGenRandom),
);
else mixin entropyImpl!(
    EntropySource.none,
);

private
{
    static EntropySource _entropySource = defaultEntropySource;

    void saveSourceForNextUse(const EntropyResult result) @safe
    {
        if (!result.isOK)
            return;

        _entropySource = result.source;
    }
}

version (all)
{
private:

    EntropyResult getEntropyViaNone(scope void[]) @safe
    {
        return EntropyResult(EntropyStatus.unavailable, EntropySource.none);
    }
}

version (Posix)
{
private:

    EntropyResult getEntropyViaCharDevURandom(scope void[] buffer) @trusted
    {
        const status = getEntropyViaCharDev(buffer, "/dev/urandom".ptr);
        return EntropyResult(status, EntropySource.charDevURandom);
    }

    EntropyResult getEntropyViaCharDevRandom(scope void[] buffer) @trusted
    {
        const status = getEntropyViaCharDev(buffer, "/dev/random".ptr);
        return EntropyResult(status, EntropySource.charDevRandom);
    }

    EntropyStatus getEntropyViaCharDev(scope void[] buffer, const(char)* charDevName) @system
    {
        import core.stdc.stdio : fclose, fopen, fread;

        auto charDev = fopen(charDevName, "r");
        if (charDev is null)
            return EntropyStatus.unavailable;

        scope (exit)
            fclose(charDev);

        const bytesRead = fread(buffer.ptr, 1, buffer.length, charDev);
        if (bytesRead != buffer.length)
            return EntropyStatus.readError;

        return EntropyStatus.ok;
    }
}

version (linux)
{
private:

    EntropyResult getEntropyViaGetrandom(scope void[] buffer) @trusted
    {
        const status = syscallGetrandom(buffer, 0);
        return EntropyResult(status, EntropySource.getrandom);
    }

    EntropyStatus syscallGetrandom(scope void[] buffer, uint flags) @system
    {
        import core.sys.linux.errno : EINTR, ENOSYS, errno;
        import core.sys.linux.sys.syscall : SYS_getrandom;
        import core.sys.linux.unistd : syscall;

        while (buffer.length > 0)
        {
            const got = syscall(SYS_getrandom, buffer.ptr, buffer.length, flags);

            if (got == -1)
            {
                switch (errno)
                {
                case EINTR:
                    break; // That’s fine.
                case ENOSYS:
                    return EntropyStatus.unavailable;
                default:
                    return EntropyStatus.readError;
                }
            }

            if (got > 0)
                buffer = buffer[got .. $];
        }

        return EntropyStatus.ok;
    }
}

// BSD
private
{
    version (Darwin)
        version = SecureARC4Random;
    version (DragonFlyBSD)
        version = UseGetentropy;
    version (FreeBSD)
        version = UseGetentropy;
    version (NetBSD)
        version = SecureARC4Random;
    version (OpenBSD)
        version = SecureARC4Random;

    version (SecureARC4Random)
    {
        EntropyResult getEntropyViaARC4Random(scope void[] buffer) @trusted
        {
            arc4random_buf(buffer.ptr, buffer.length);
            return EntropyResult(EntropyStatus.ok, EntropySource.arc4random);
        }

        private extern(C) void arc4random_buf(scope void* buf, size_t nbytes) @system;
    }

    version (UseGetentropy)
    {
        EntropyResult getEntropyViaGetentropy(scope void[] buffer) @trusted
        {
            const status = callGetentropy(buffer);
            return EntropyResult(status, EntropySource.getentropy);
        }

        private EntropyStatus callGetentropy(scope void[] buffer) @system
        {
            /+
                genentropy(3):
                The maximum buflen permitted is 256 bytes.
            +/
            foreach (chunk; VoidChunks(buffer, 256))
            {
                const status = getentropy(buffer.ptr, buffer.length);
                if (status != 0)
                    return EntropyStatus.readError;
            }

            return EntropyStatus.ok;
        }

        private extern(C) int getentropy(scope void* buf, size_t buflen) @system;
    }
}

version (Windows)
{
    import core.sys.windows.bcrypt : BCryptGenRandom, BCRYPT_USE_SYSTEM_PREFERRED_RNG;
    import core.sys.windows.windef : HMODULE, PUCHAR, ULONG;
    import core.sys.windows.ntdef : NT_SUCCESS;

private:

    EntropyResult getEntropyViaBCryptGenRandom(scope void[] buffer) @trusted
    {
        const loaded = loadBcrypt();
        if (loaded != EntropyStatus.ok)
            return EntropyResult(loaded, EntropySource.bcryptGenRandom);

        const status = callBcryptGenRandom(buffer);
        return EntropyResult(status, EntropySource.bcryptGenRandom);
    }

    EntropyStatus callBcryptGenRandom(scope void[] buffer) @system
    {
        foreach (chunk; VoidChunks(buffer, ULONG.max))
        {
            assert(chunk.length <= ULONG.max, "Bad chunk length.");

            const gotRandom = ptrBCryptGenRandom(
                null,
                cast(PUCHAR) buffer.ptr,
                cast(ULONG) buffer.length,
                BCRYPT_USE_SYSTEM_PREFERRED_RNG,
            );

            if (!NT_SUCCESS(gotRandom))
                return EntropyStatus.readError;
        }

        return EntropyStatus.ok;
    }

    static
    {
        HMODULE hBcrypt = null;
        typeof(BCryptGenRandom)* ptrBCryptGenRandom;
    }

    EntropyStatus loadBcrypt() @system
    {
        import core.sys.windows.winbase : GetProcAddress, LoadLibraryA;

        if (hBcrypt !is null)
            return EntropyStatus.ok;

        hBcrypt = LoadLibraryA("Bcrypt.dll");
        if (!hBcrypt)
            return EntropyStatus.unavailableLibrary;

        ptrBCryptGenRandom = cast(typeof(ptrBCryptGenRandom)) GetProcAddress(hBcrypt, "BCryptGenRandom");
        if (!ptrBCryptGenRandom)
            return EntropyStatus.unavailable;

        return EntropyStatus.ok;
    }

    // Will free `Bcrypt.dll`.
    void freeBcrypt() @system
    {
        import core.sys.windows.winbase : FreeLibrary;

        if (hBcrypt is null)
            return;

        if (!FreeLibrary(hBcrypt))
        {
            return; // Error
        }

        hBcrypt = null;
        ptrBCryptGenRandom = null;
    }

    static ~this() @system
    {
        freeBcrypt();
    }
}

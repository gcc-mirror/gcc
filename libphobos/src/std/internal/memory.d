module std.internal.memory;

package(std):

version (D_Exceptions)
{
    import core.exception : onOutOfMemoryError;
    private enum allocationFailed = `onOutOfMemoryError();`;
}
else
{
    private enum allocationFailed = `assert(0, "Memory allocation failed");`;
}

// (below comments are non-DDOC, but are written in similar style)

/+
Mnemonic for `enforce!OutOfMemoryError(malloc(size))` that (unlike malloc)
can be considered pure because it causes the program to abort if the result
of the allocation is null, with the consequence that errno will not be
visibly changed by calling this function. Note that `malloc` can also
return `null` in non-failure situations if given an argument of 0. Hence,
it is a programmer error to use this function if the requested allocation
size is logically permitted to be zero. `enforceCalloc` and `enforceRealloc`
work analogously.

All these functions are usable in `betterC`.
+/
void* enforceMalloc()(size_t size) @nogc nothrow pure @safe
{
    auto result = fakePureMalloc(size);
    if (!result) mixin(allocationFailed);
    return result;
}

// ditto
void* enforceCalloc()(size_t nmemb, size_t size) @nogc nothrow pure @safe
{
    auto result = fakePureCalloc(nmemb, size);
    if (!result) mixin(allocationFailed);
    return result;
}

// ditto
void* enforceRealloc()(return scope void* ptr, size_t size) @nogc nothrow pure @system
{
    auto result = fakePureRealloc(ptr, size);
    if (!result) mixin(allocationFailed);
    return result;
}

// Purified for local use only.
extern (C) @nogc nothrow pure private
{
    pragma(mangle, "malloc") void* fakePureMalloc(size_t) @safe;
    pragma(mangle, "calloc") void* fakePureCalloc(size_t nmemb, size_t size) @safe;
    pragma(mangle, "realloc") void* fakePureRealloc(return scope void* ptr, size_t size) @system;
}

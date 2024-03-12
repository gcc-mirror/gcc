/// D wrapper for the Valgrind client API.
/// Note that you must include this file into your program's compilation
/// and compile with `-debug=VALGRIND` to access the declarations below.
module etc.valgrind.valgrind;

version (StdDdoc)
{
    /// Mark the memory covered by `mem` as unaddressable.
    void makeMemNoAccess (const(void)[] mem) nothrow @nogc;

    /// Similarly, mark memory covered by `mem` as addressable but undefined.
    void makeMemUndefined(const(void)[] mem) nothrow @nogc;

    /// Similarly, mark memory covered by `mem` as addressable and defined.
    void makeMemDefined  (const(void)[] mem) nothrow @nogc;

    /// Get the validity data for the address range covered by `mem` and copy it
    /// into the provided `bits` array.
    /// Returns:
    ///   - 0   if not running on valgrind
    ///   - 1   success
    ///   - 2   [previously indicated unaligned arrays;  these are now allowed]
    ///   - 3   if any parts of `mem`/`bits` are not addressable.
    /// The metadata is not copied in cases 0, 2 or 3 so it should be
    /// impossible to segfault your system by using this call.
    uint getVBits(const(void)[] mem, ubyte[] bits) nothrow @nogc;

    /// Set the validity data for the address range covered by `mem`, copying it
    /// from the provided `bits` array.
    /// Returns:
    ///   - 0   if not running on valgrind
    ///   - 1   success
    ///   - 2   [previously indicated unaligned arrays;  these are now allowed]
    ///   - 3   if any parts of `mem`/`bits` are not addressable.
    /// The metadata is not copied in cases 0, 2 or 3 so it should be
    /// impossible to segfault your system by using this call.
    uint setVBits(const(void)[] mem, ubyte[] bits) nothrow @nogc;

    /// Disable and re-enable reporting of addressing errors in the
    /// address range covered by `mem`.
    void disableAddrReportingInRange(const(void)[] mem) nothrow @nogc;

    /// ditto
    void enableAddrReportingInRange(const(void)[] mem) nothrow @nogc;
}
else:

debug(VALGRIND):

private extern(C) nothrow @nogc
{
    void _d_valgrind_make_mem_noaccess (const(void)* addr, size_t len);
    void _d_valgrind_make_mem_undefined(const(void)* addr, size_t len);
    void _d_valgrind_make_mem_defined  (const(void)* addr, size_t len);
    uint _d_valgrind_get_vbits(const(void)* addr, ubyte* bits, size_t len);
    uint _d_valgrind_set_vbits(const(void)* addr, ubyte* bits, size_t len);
    void _d_valgrind_disable_addr_reporting_in_range(const(void)* addr, size_t len);
    void _d_valgrind_enable_addr_reporting_in_range (const(void)* addr, size_t len);
}

void makeMemNoAccess (const(void)[] mem) nothrow @nogc { _d_valgrind_make_mem_noaccess (mem.ptr, mem.length); }
void makeMemUndefined(const(void)[] mem) nothrow @nogc { _d_valgrind_make_mem_undefined(mem.ptr, mem.length); }
void makeMemDefined  (const(void)[] mem) nothrow @nogc { _d_valgrind_make_mem_defined  (mem.ptr, mem.length); }

uint getVBits(const(void)[] mem, ubyte[] bits) nothrow @nogc
{
    assert(mem.length == bits.length);
    return _d_valgrind_get_vbits(mem.ptr, bits.ptr, mem.length);
}

uint setVBits(const(void)[] mem, ubyte[] bits) nothrow @nogc
{
    assert(mem.length == bits.length);
    return _d_valgrind_set_vbits(mem.ptr, bits.ptr, mem.length);
}

void disableAddrReportingInRange(const(void)[] mem) nothrow @nogc
{
    _d_valgrind_disable_addr_reporting_in_range(mem.ptr, mem.length);
}

void enableAddrReportingInRange(const(void)[] mem) nothrow @nogc
{
    _d_valgrind_enable_addr_reporting_in_range(mem.ptr, mem.length);
}

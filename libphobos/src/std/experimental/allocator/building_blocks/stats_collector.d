// Written in the D programming language.
/**
Allocator that collects useful statistics about allocations, both global and per
calling point. The statistics collected can be configured statically by choosing
combinations of `Options` appropriately.

Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/stats_collector.d)
*/
module std.experimental.allocator.building_blocks.stats_collector;

///
@safe unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    alias Allocator = StatsCollector!(GCAllocator, Options.bytesUsed);
}

import std.experimental.allocator.common;

/**
_Options for `StatsCollector` defined below. Each enables during
compilation one specific counter, statistic, or other piece of information.
*/
enum Options : ulong
{
    /**
    Counts the number of calls to `owns`.
    */
    numOwns = 1u << 0,
    /**
    Counts the number of calls to `allocate`. All calls are counted,
    including requests for zero bytes or failed requests.
    */
    numAllocate = 1u << 1,
    /**
    Counts the number of calls to `allocate` that succeeded, i.e. they
    returned a block as large as requested. (N.B. requests for zero bytes count
    as successful.)
    */
    numAllocateOK = 1u << 2,
    /**
    Counts the number of calls to `expand`, regardless of arguments or
    result.
    */
    numExpand = 1u << 3,
    /**
    Counts the number of calls to `expand` that resulted in a successful
    expansion.
    */
    numExpandOK = 1u << 4,
    /**
    Counts the number of calls to `reallocate`, regardless of arguments or
    result.
    */
    numReallocate = 1u << 5,
    /**
    Counts the number of calls to `reallocate` that succeeded.
    (Reallocations to zero bytes count as successful.)
    */
    numReallocateOK = 1u << 6,
    /**
    Counts the number of calls to `reallocate` that resulted in an in-place
    reallocation (no memory moved). If this number is close to the total number
    of reallocations, that indicates the allocator finds room at the current
    block's end in a large fraction of the cases, but also that internal
    fragmentation may be high (the size of the unit of allocation is large
    compared to the typical allocation size of the application).
    */
    numReallocateInPlace = 1u << 7,
    /**
    Counts the number of calls to `deallocate`.
    */
    numDeallocate = 1u << 8,
    /**
    Counts the number of calls to `deallocateAll`.
    */
    numDeallocateAll = 1u << 9,
    /**
    Counts the number of calls to `alignedAllocate`. All calls are counted,
    including requests for zero bytes or failed requests.
    */
    numAlignedAllocate = 1u << 10,
    /**
    Counts the number of calls to `alignedAllocate` that succeeded, i.e. they
    returned a block as large as requested. (N.B. requests for zero bytes count
    as successful.)
    */
    numAlignedAllocateOk = 1u << 11,
    /**
    Chooses all `numXxx` flags.
    */
    numAll = (1u << 12) - 1,
    /**
    Tracks bytes currently allocated by this allocator. This number goes up
    and down as memory is allocated and deallocated, and is zero if the
    allocator currently has no active allocation.
    */
    bytesUsed = 1u << 12,
    /**
    Tracks total cumulative bytes allocated by means of `allocate`,
    `expand`, and `reallocate` (when resulting in an expansion). This
    number always grows and indicates allocation traffic. To compute bytes
    deallocated cumulatively, subtract `bytesUsed` from `bytesAllocated`.
    */
    bytesAllocated = 1u << 13,
    /**
    Tracks the sum of all `delta` values in calls of the form
    $(D expand(b, delta)) that succeed (return `true`).
    */
    bytesExpanded = 1u << 14,
    /**
    Tracks the sum of all $(D b.length - s) with $(D b.length > s) in calls of
    the form $(D realloc(b, s)) that succeed (return `true`). In per-call
    statistics, also unambiguously counts the bytes deallocated with
    `deallocate`.
    */
    bytesContracted = 1u << 15,
    /**
    Tracks the sum of all bytes moved as a result of calls to `realloc` that
    were unable to reallocate in place. A large number (relative to $(D
    bytesAllocated)) indicates that the application should use larger
    preallocations.
    */
    bytesMoved = 1u << 16,
    /**
    Tracks the sum of all bytes NOT moved as result of calls to `realloc`
    that managed to reallocate in place. A large number (relative to $(D
    bytesAllocated)) indicates that the application is expansion-intensive and
    is saving a good amount of moves. However, if this number is relatively
    small and `bytesSlack` is high, it means the application is
    overallocating for little benefit.
    */
    bytesNotMoved = 1u << 17,
    /**
    Measures the sum of extra bytes allocated beyond the bytes requested, i.e.
    the $(HTTP goo.gl/YoKffF, internal fragmentation). This is the current
    effective number of slack bytes, and it goes up and down with time.
    */
    bytesSlack = 1u << 18,
    /**
    Measures the maximum bytes allocated over the time. This is useful for
    dimensioning allocators.
    */
    bytesHighTide = 1u << 19,
    /**
    Chooses all `byteXxx` flags.
    */
    bytesAll = ((1u << 20) - 1) & ~numAll,
    /**
    Combines all flags above.
    */
    all = (1u << 20) - 1
}

/**

Allocator that collects extra data about allocations. Since each piece of
information adds size and time overhead, statistics can be individually enabled
or disabled through compile-time `flags`.

All stats of the form `numXxx` record counts of events occurring, such as
calls to functions and specific results. The stats of the form `bytesXxx`
collect cumulative sizes.

In addition, the data `callerSize`, `callerModule`, `callerFile`, $(D
callerLine), and `callerTime` is associated with each specific allocation.
This data prefixes each allocation.

*/
struct StatsCollector(Allocator, ulong flags = Options.all,
    ulong perCallFlags = 0)
{
private:
    import std.traits : hasMember, Signed;
    import std.typecons : Ternary;

    static string define(string type, string[] names...)
    {
        string result;
        foreach (v; names)
            result ~= "static if (flags & Options."~v~") {"
                ~ "private "~type~" _"~v~";"
                ~ "public const("~type~") "~v~"() const { return _"~v~"; }"
                ~ "}";
        return result;
    }

    void add(string counter)(Signed!size_t n)
    {
        mixin("static if (flags & Options." ~ counter
            ~ ") _" ~ counter ~ " += n;");
        static if (counter == "bytesUsed" && (flags & Options.bytesHighTide))
        {
            if (bytesHighTide < bytesUsed ) _bytesHighTide = bytesUsed;
        }
    }

    void up(string counter)() { add!counter(1); }
    void down(string counter)() { add!counter(-1); }

    version (StdDdoc)
    {
        /**
        Read-only properties enabled by the homonym `flags` chosen by the
        user.

        Example:
        ----
        StatsCollector!(Mallocator,
            Options.bytesUsed | Options.bytesAllocated) a;
        auto d1 = a.allocate(10);
        auto d2 = a.allocate(11);
        a.deallocate(d1);
        assert(a.bytesAllocated == 21);
        assert(a.bytesUsed == 11);
        a.deallocate(d2);
        assert(a.bytesAllocated == 21);
        assert(a.bytesUsed == 0);
        ----
        */
        @property ulong numOwns() const;
        /// Ditto
        @property ulong numAllocate() const;
        /// Ditto
        @property ulong numAllocateOK() const;
        /// Ditto
        @property ulong numExpand() const;
        /// Ditto
        @property ulong numExpandOK() const;
        /// Ditto
        @property ulong numReallocate() const;
        /// Ditto
        @property ulong numReallocateOK() const;
        /// Ditto
        @property ulong numReallocateInPlace() const;
        /// Ditto
        @property ulong numDeallocate() const;
        /// Ditto
        @property ulong numDeallocateAll() const;
        /// Ditto
        @property ulong numAlignedAllocate() const;
        /// Ditto
        @property ulong numAlignedAllocateOk() const;
        /// Ditto
        @property ulong bytesUsed() const;
        /// Ditto
        @property ulong bytesAllocated() const;
        /// Ditto
        @property ulong bytesExpanded() const;
        /// Ditto
        @property ulong bytesContracted() const;
        /// Ditto
        @property ulong bytesMoved() const;
        /// Ditto
        @property ulong bytesNotMoved() const;
        /// Ditto
        @property ulong bytesSlack() const;
        /// Ditto
        @property ulong bytesHighTide() const;
    }

public:
    /**
    The parent allocator is publicly accessible either as a direct member if it
    holds state, or as an alias to `Allocator.instance` otherwise. One may use
    it for making calls that won't count toward statistics collection.
    */
    static if (stateSize!Allocator) Allocator parent;
    else alias parent = Allocator.instance;

private:
    // Per-allocator state
    mixin(define("ulong",
        "numOwns",
        "numAllocate",
        "numAllocateOK",
        "numExpand",
        "numExpandOK",
        "numReallocate",
        "numReallocateOK",
        "numReallocateInPlace",
        "numDeallocate",
        "numDeallocateAll",
        "numAlignedAllocate",
        "numAlignedAllocateOk",
        "bytesUsed",
        "bytesAllocated",
        "bytesExpanded",
        "bytesContracted",
        "bytesMoved",
        "bytesNotMoved",
        "bytesSlack",
        "bytesHighTide",
    ));

public:

    /// Alignment offered is equal to `Allocator.alignment`.
    alias alignment = Allocator.alignment;

    /**
    Increments `numOwns` (per instance and and per call) and forwards to $(D
    parent.owns(b)).
    */
    static if (hasMember!(Allocator, "owns"))
    {
        static if ((perCallFlags & Options.numOwns) == 0)
        Ternary owns(void[] b)
        { return ownsImpl(b); }
        else
        Ternary owns(string f = __FILE__, uint n = __LINE__)(void[] b)
        { return ownsImpl!(f, n)(b); }
    }

    private Ternary ownsImpl(string f = null, uint n = 0)(void[] b)
    {
        up!"numOwns";
        addPerCall!(f, n, "numOwns")(1);
        return parent.owns(b);
    }

    /**
    Forwards to `parent.allocate`. Affects per instance: `numAllocate`,
    `bytesUsed`, `bytesAllocated`, `bytesSlack`, `numAllocateOK`,
    and `bytesHighTide`. Affects per call: `numAllocate`, $(D
    numAllocateOK), and `bytesAllocated`.
    */
    static if (!(perCallFlags
        & (Options.numAllocate | Options.numAllocateOK
            | Options.bytesAllocated)))
    {
        void[] allocate(size_t n)
        { return allocateImpl(n); }
    }
    else
    {
        void[] allocate(string f = __FILE__, ulong n = __LINE__)
            (size_t bytes)
        { return allocateImpl!(f, n)(bytes); }
    }

    // Common code currently shared between allocateImpl and allocateZeroedImpl.
    private enum _updateStatsForAllocateResult =
    q{
        add!"bytesUsed"(result.length);
        add!"bytesAllocated"(result.length);
        immutable slack = this.goodAllocSize(result.length) - result.length;
        add!"bytesSlack"(slack);
        up!"numAllocate";
        add!"numAllocateOK"(result.length == bytes); // allocating 0 bytes is OK
        addPerCall!(f, n, "numAllocate", "numAllocateOK", "bytesAllocated")
            (1, result.length == bytes, result.length);
    };

    private void[] allocateImpl(string f = null, ulong n = 0)(size_t bytes)
    {
        auto result = parent.allocate(bytes);
        mixin(_updateStatsForAllocateResult);
        return result;
    }

    static if (hasMember!(Allocator, "allocateZeroed"))
    {
        static if (!(perCallFlags
            & (Options.numAllocate | Options.numAllocateOK
                | Options.bytesAllocated)))
        {
            package(std) void[] allocateZeroed()(size_t n)
            { return allocateZeroedImpl(n); }
        }
        else
        {
            package(std) void[] allocateZeroed(string f = __FILE__, ulong n = __LINE__)
                (size_t bytes)
            { return allocateZeroedImpl!(f, n)(bytes); }
        }

        private void[] allocateZeroedImpl(string f = null, ulong n = 0)(size_t bytes)
        {
            auto result = parent.allocateZeroed(bytes);
            // Note: calls to `allocateZeroed` are counted for statistical purposes
            // as if they were calls to `allocate`. If/when `allocateZeroed` is made
            // public it might be of interest to count such calls separately.
            mixin(_updateStatsForAllocateResult);
            return result;
        }
    }

    /**
    Forwards to `parent.alignedAllocate`. Affects per instance: `numAlignedAllocate`,
    `bytesUsed`, `bytesAllocated`, `bytesSlack`, `numAlignedAllocateOk`,
    and `bytesHighTide`. Affects per call: `numAlignedAllocate`, `numAlignedAllocateOk`,
    and `bytesAllocated`.
    */
    static if (!(perCallFlags
        & (Options.numAlignedAllocate | Options.numAlignedAllocateOk
            | Options.bytesAllocated)))
    {
        void[] alignedAllocate(size_t n, uint a)
        { return alignedAllocateImpl(n, a); }
    }
    else
    {
        void[] alignedAllocate(string f = __FILE__, ulong n = __LINE__)
            (size_t bytes, uint a)
        { return alignedAllocateImpl!(f, n)(bytes, a); }
    }

    private void[] alignedAllocateImpl(string f = null, ulong n = 0)(size_t bytes, uint a)
    {
        up!"numAlignedAllocate";
        static if (!hasMember!(Allocator, "alignedAllocate"))
        {
            if (bytes == 0)
                up!"numAlignedAllocateOk";
            void[] result = null;
        }
        else
        {
            auto result = parent.alignedAllocate(bytes, a);
            add!"bytesUsed"(result.length);
            add!"bytesAllocated"(result.length);
            immutable slack = this.goodAllocSize(result.length) - result.length;
            add!"bytesSlack"(slack);
            add!"numAlignedAllocateOk"(result.length == bytes); // allocating 0 bytes is OK
        }
        addPerCall!(f, n, "numAlignedAllocate", "numAlignedAllocateOk", "bytesAllocated")
            (1, result.length == bytes, result.length);

        return result;
    }

    /**
    Defined whether or not `Allocator.expand` is defined. Affects
    per instance: `numExpand`, `numExpandOK`, `bytesExpanded`,
    `bytesSlack`, `bytesAllocated`, and `bytesUsed`. Affects per call:
    `numExpand`, `numExpandOK`, `bytesExpanded`, and
    `bytesAllocated`.
    */
    static if (!(perCallFlags
        & (Options.numExpand | Options.numExpandOK | Options.bytesExpanded)))
    {
        bool expand(ref void[] b, size_t delta)
        { return expandImpl(b, delta); }
    }
    else
    {
        bool expand(string f = __FILE__, uint n = __LINE__)
            (ref void[] b, size_t delta)
        { return expandImpl!(f, n)(b, delta); }
    }

    private bool expandImpl(string f = null, uint n = 0)(ref void[] b, size_t s)
    {
        up!"numExpand";
        Signed!size_t slack = 0;
        static if (!hasMember!(Allocator, "expand"))
        {
            auto result = s == 0;
        }
        else
        {
            immutable bytesSlackB4 = this.goodAllocSize(b.length) - b.length;
            auto result = parent.expand(b, s);
            if (result)
            {
                up!"numExpandOK";
                add!"bytesUsed"(s);
                add!"bytesAllocated"(s);
                add!"bytesExpanded"(s);
                slack = Signed!size_t(this.goodAllocSize(b.length) - b.length
                    - bytesSlackB4);
                add!"bytesSlack"(slack);
            }
        }
        immutable xtra = result ? s : 0;
        addPerCall!(f, n, "numExpand", "numExpandOK", "bytesExpanded",
            "bytesAllocated")
            (1, result, xtra, xtra);
        return result;
    }

    /**
    Defined whether or not `Allocator.reallocate` is defined. Affects
    per instance: `numReallocate`, `numReallocateOK`, $(D
    numReallocateInPlace), `bytesNotMoved`, `bytesAllocated`, $(D
    bytesSlack), `bytesExpanded`, and `bytesContracted`. Affects per call:
    `numReallocate`, `numReallocateOK`, `numReallocateInPlace`,
    `bytesNotMoved`, `bytesExpanded`, `bytesContracted`, and
    `bytesMoved`.
    */
    static if (!(perCallFlags
        & (Options.numReallocate | Options.numReallocateOK
            | Options.numReallocateInPlace | Options.bytesNotMoved
            | Options.bytesExpanded | Options.bytesContracted
            | Options.bytesMoved)))
    {
        bool reallocate(ref void[] b, size_t s)
        { return reallocateImpl(b, s); }
    }
    else
    {
        bool reallocate(string f = __FILE__, ulong n = __LINE__)
            (ref void[] b, size_t s)
        { return reallocateImpl!(f, n)(b, s); }
    }

    private bool reallocateImpl(string f = null, uint n = 0)
        (ref void[] b, size_t s)
    {
        up!"numReallocate";
        const bytesSlackB4 = this.goodAllocSize(b.length) - b.length;
        const oldB = b.ptr;
        const oldLength = b.length;

        const result = parent.reallocate(b, s);

        Signed!size_t slack = 0;
        bool wasInPlace = false;
        Signed!size_t delta = 0;

        if (result)
        {
            up!"numReallocateOK";
            slack = (this.goodAllocSize(b.length) - b.length) - bytesSlackB4;
            add!"bytesSlack"(slack);
            add!"bytesUsed"(Signed!size_t(b.length - oldLength));
            if (oldB == b.ptr)
            {
                // This was an in-place reallocation, yay
                wasInPlace = true;
                up!"numReallocateInPlace";
                add!"bytesNotMoved"(oldLength);
                delta = b.length - oldLength;
                if (delta >= 0)
                {
                    // Expansion
                    add!"bytesAllocated"(delta);
                    add!"bytesExpanded"(delta);
                }
                else
                {
                    // Contraction
                    add!"bytesContracted"(-delta);
                }
            }
            else
            {
                // This was a allocate-move-deallocate cycle
                add!"bytesAllocated"(b.length);
                add!"bytesMoved"(oldLength);
            }
        }
        addPerCall!(f, n, "numReallocate", "numReallocateOK",
            "numReallocateInPlace", "bytesNotMoved",
            "bytesExpanded", "bytesContracted", "bytesMoved")
            (1, result, wasInPlace, wasInPlace ? oldLength : 0,
                delta >= 0 ? delta : 0, delta < 0 ? -delta : 0,
                wasInPlace ? 0 : oldLength);
        return result;
    }

    /**
    Defined whether or not `Allocator.deallocate` is defined. Affects
    per instance: `numDeallocate`, `bytesUsed`, and `bytesSlack`.
    Affects per call: `numDeallocate` and `bytesContracted`.
    */
    static if (!(perCallFlags &
            (Options.numDeallocate | Options.bytesContracted)))
        bool deallocate(void[] b)
        { return deallocateImpl(b); }
    else
        bool deallocate(string f = __FILE__, uint n = __LINE__)(void[] b)
        { return deallocateImpl!(f, n)(b); }

    private bool deallocateImpl(string f = null, uint n = 0)(void[] b)
    {
        up!"numDeallocate";
        add!"bytesUsed"(-Signed!size_t(b.length));
        add!"bytesSlack"(-(this.goodAllocSize(b.length) - b.length));
        addPerCall!(f, n, "numDeallocate", "bytesContracted")(1, b.length);
        static if (hasMember!(Allocator, "deallocate"))
            return parent.deallocate(b);
        else
            return false;
    }

    static if (hasMember!(Allocator, "deallocateAll"))
    {
        /**
        Defined only if `Allocator.deallocateAll` is defined. Affects
        per instance and per call `numDeallocateAll`.
        */
        static if (!(perCallFlags & Options.numDeallocateAll))
            bool deallocateAll()
            { return deallocateAllImpl(); }
        else
            bool deallocateAll(string f = __FILE__, uint n = __LINE__)()
            { return deallocateAllImpl!(f, n)(); }

        private bool deallocateAllImpl(string f = null, uint n = 0)()
        {
            up!"numDeallocateAll";
            addPerCall!(f, n, "numDeallocateAll")(1);
            static if ((flags & Options.bytesUsed))
                _bytesUsed = 0;
            return parent.deallocateAll();
        }
    }

    /**
    Defined only if `Options.bytesUsed` is defined. Returns $(D bytesUsed ==
    0).
    */
    static if (flags & Options.bytesUsed)
    pure nothrow @safe @nogc
    Ternary empty()
    {
        return Ternary(_bytesUsed == 0);
    }

    /**
    Reports per instance statistics to `output` (e.g. `stdout`). The
    format is simple: one kind and value per line, separated by a colon, e.g.
    `bytesAllocated:7395404`
    */
    void reportStatistics(R)(auto ref R output)
    {
        import std.conv : to;
        import std.traits : EnumMembers;
        foreach (e; EnumMembers!Options)
        {
            static if ((flags & e) && e != Options.numAll
                    && e != Options.bytesAll && e != Options.all)
                output.write(e.to!string, ":", mixin(e.to!string), '\n');
        }
    }

    static if (perCallFlags)
    {
        /**
        Defined if `perCallFlags` is nonzero.
        */
        struct PerCallStatistics
        {
            /// The file and line of the call.
            string file;
            /// Ditto
            uint line;
            /// The options corresponding to the statistics collected.
            Options[] opts;
            /// The values of the statistics. Has the same length as `opts`.
            ulong[] values;
            // Next in the chain.
            private PerCallStatistics* next;

            /**
            Format to a string such as:
            $(D mymodule.d(655): [numAllocate:21, numAllocateOK:21, bytesAllocated:324202]).
            */
            string toString() const
            {
                import std.conv : text, to;
                auto result = text(file, "(", line, "): [");
                foreach (i, opt; opts)
                {
                    if (i) result ~= ", ";
                    result ~= opt.to!string;
                    result ~= ':';
                    result ~= values[i].to!string;
                }
                return result ~= "]";
            }
        }
        private static PerCallStatistics* root;

        /**
        Defined if `perCallFlags` is nonzero. Iterates all monitored
        file/line instances. The order of iteration is not meaningful (items
        are inserted at the front of a list upon the first call), so
        preprocessing the statistics after collection might be appropriate.
        */
        static auto byFileLine()
        {
            static struct Voldemort
            {
                PerCallStatistics* current;
                bool empty() { return !current; }
                ref PerCallStatistics front() { return *current; }
                void popFront() { current = current.next; }
                auto save() { return this; }
            }
            return Voldemort(root);
        }

        /**
        Defined if `perCallFlags` is nonzero. Outputs (e.g. to a `File`)
        a simple report of the collected per-call statistics.
        */
        static void reportPerCallStatistics(R)(auto ref R output)
        {
            output.write("Stats for: ", StatsCollector.stringof, '\n');
            foreach (ref stat; byFileLine)
            {
                output.write(stat, '\n');
            }
        }

        private PerCallStatistics* statsAt(string f, uint n, opts...)()
        {
            import std.array : array;
            import std.range : repeat;

            static PerCallStatistics s = { f, n, [ opts ],
                repeat(0UL, opts.length).array };
            static bool inserted;

            if (!inserted)
            {
                // Insert as root
                s.next = root;
                root = &s;
                inserted = true;
            }
            return &s;
        }

        private void addPerCall(string f, uint n, names...)(ulong[] values...)
        {
            import std.array : join;
            enum ulong mask = mixin("Options."~[names].join("|Options."));
            static if (perCallFlags & mask)
            {
                // Per allocation info
                auto ps = mixin("statsAt!(f, n,"
                    ~ "Options."~[names].join(", Options.")
                ~")");
                foreach (i; 0 .. names.length)
                {
                    ps.values[i] += values[i];
                }
            }
        }
    }
    else
    {
        private void addPerCall(string f, uint n, names...)(ulong[]...)
        {
        }
    }
}

///
@system unittest
{
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    alias Allocator = StatsCollector!(GCAllocator, Options.all, Options.all);

    Allocator alloc;
    auto b = alloc.allocate(10);
    alloc.reallocate(b, 20);
    alloc.deallocate(b);

    import std.file : deleteme, remove;
    import std.range : walkLength;
    import std.stdio : File;

    auto f = deleteme ~ "-dlang.std.experimental.allocator.stats_collector.txt";
    scope(exit) remove(f);
    Allocator.reportPerCallStatistics(File(f, "w"));
    alloc.reportStatistics(File(f, "a"));
    assert(File(f).byLine.walkLength == 24);
}

@system unittest
{
    void test(Allocator)()
    {
        import std.range : walkLength;
        import std.typecons : Ternary;

        Allocator a;
        assert((() pure nothrow @safe @nogc => a.empty)() == Ternary.yes);
        auto b1 = a.allocate(100);
        assert(a.numAllocate == 1);
        assert((() nothrow @safe => a.expand(b1, 0))());
        assert(a.reallocate(b1, b1.length + 1));
        auto b2 = a.allocate(101);
        assert(a.numAllocate == 2);
        assert(a.bytesAllocated == 202);
        assert(a.bytesUsed == 202);
        auto b3 = a.allocate(202);
        assert(a.numAllocate == 3);
        assert(a.bytesAllocated == 404);
        assert((() pure nothrow @safe @nogc => a.empty)() == Ternary.no);

        () nothrow @nogc { a.deallocate(b2); }();
        assert(a.numDeallocate == 1);
        () nothrow @nogc { a.deallocate(b1); }();
        assert(a.numDeallocate == 2);
        () nothrow @nogc { a.deallocate(b3); }();
        assert(a.numDeallocate == 3);
        assert(a.numAllocate == a.numDeallocate);
        assert(a.bytesUsed == 0);
     }

    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    test!(StatsCollector!(GCAllocator, Options.all, Options.all));
    test!(StatsCollector!(FreeList!(GCAllocator, 128), Options.all,
        Options.all));
}

@system unittest
{
    void test(Allocator)()
    {
        import std.range : walkLength;
        Allocator a;
        auto b1 = a.allocate(100);
        assert((() nothrow @safe => a.expand(b1, 0))());
        assert(a.reallocate(b1, b1.length + 1));
        auto b2 = a.allocate(101);
        auto b3 = a.allocate(202);

        () nothrow @nogc { a.deallocate(b2); }();
        () nothrow @nogc { a.deallocate(b1); }();
        () nothrow @nogc { a.deallocate(b3); }();
    }
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    test!(StatsCollector!(GCAllocator, 0, 0));
}

@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    StatsCollector!(GCAllocator, 0, 0) a;

    // calls std.experimental.allocator.common.goodAllocSize
    assert((() pure nothrow @safe @nogc => a.goodAllocSize(1))());
}

@system unittest
{
    import std.experimental.allocator.building_blocks.region : BorrowedRegion;

    auto a = StatsCollector!(BorrowedRegion!(), Options.all, Options.all)(BorrowedRegion!()(new ubyte[1024 * 64]));
    auto b = a.allocate(42);
    assert(b.length == 42);
    // Test that reallocate infers from parent
    assert((() nothrow @nogc => a.reallocate(b, 100))());
    assert(b.length == 100);
    // Test that deallocateAll infers from parent
    assert((() nothrow @nogc => a.deallocateAll())());
}

@system unittest
{
    import std.experimental.allocator.building_blocks.region : BorrowedRegion;

    auto a = StatsCollector!(BorrowedRegion!(), Options.all)(BorrowedRegion!()(new ubyte[1024 * 64]));
    auto b = a.alignedAllocate(42, 128);
    assert(b.length == 42);
    assert(b.ptr.alignedAt(128));
    assert(a.numAlignedAllocate == 1);
    assert(a.numAlignedAllocateOk == 1);
    assert(a.bytesUsed == 42);

    b = a.alignedAllocate(23, 256);
    assert(b.length == 23);
    assert(b.ptr.alignedAt(256));
    assert(a.numAlignedAllocate == 2);
    assert(a.numAlignedAllocateOk == 2);
    assert(a.bytesUsed == 65);

    b = a.alignedAllocate(0, 512);
    assert(b.length == 0);
    assert(a.numAlignedAllocate == 3);
    assert(a.numAlignedAllocateOk == 3);
    assert(a.bytesUsed == 65);

    b = a.alignedAllocate(1024 * 1024, 512);
    assert(b is null);
    assert(a.numAlignedAllocate == 4);
    assert(a.numAlignedAllocateOk == 3);
    assert(a.bytesUsed == 65);
}

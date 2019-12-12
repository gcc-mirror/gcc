// Written in the D programming language.
/**
Allocator that collects useful statistics about allocations, both global and per
calling point. The statistics collected can be configured statically by choosing
combinations of `Options` appropriately.

Example:
----
import std.experimental.allocator.gc_allocator : GCAllocator;
import std.experimental.allocator.building_blocks.free_list : FreeList;
alias Allocator = StatsCollector!(GCAllocator, Options.bytesUsed);
----
*/
module std.experimental.allocator.building_blocks.stats_collector;

import std.experimental.allocator.common;

/**
_Options for $(D StatsCollector) defined below. Each enables during
compilation one specific counter, statistic, or other piece of information.
*/
enum Options : ulong
{
    /**
    Counts the number of calls to $(D owns).
    */
    numOwns = 1u << 0,
    /**
    Counts the number of calls to $(D allocate). All calls are counted,
    including requests for zero bytes or failed requests.
    */
    numAllocate = 1u << 1,
    /**
    Counts the number of calls to $(D allocate) that succeeded, i.e. they
    returned a block as large as requested. (N.B. requests for zero bytes count
    as successful.)
    */
    numAllocateOK = 1u << 2,
    /**
    Counts the number of calls to $(D expand), regardless of arguments or
    result.
    */
    numExpand = 1u << 3,
    /**
    Counts the number of calls to $(D expand) that resulted in a successful
    expansion.
    */
    numExpandOK = 1u << 4,
    /**
    Counts the number of calls to $(D reallocate), regardless of arguments or
    result.
    */
    numReallocate = 1u << 5,
    /**
    Counts the number of calls to $(D reallocate) that succeeded.
    (Reallocations to zero bytes count as successful.)
    */
    numReallocateOK = 1u << 6,
    /**
    Counts the number of calls to $(D reallocate) that resulted in an in-place
    reallocation (no memory moved). If this number is close to the total number
    of reallocations, that indicates the allocator finds room at the current
    block's end in a large fraction of the cases, but also that internal
    fragmentation may be high (the size of the unit of allocation is large
    compared to the typical allocation size of the application).
    */
    numReallocateInPlace = 1u << 7,
    /**
    Counts the number of calls to $(D deallocate).
    */
    numDeallocate = 1u << 8,
    /**
    Counts the number of calls to $(D deallocateAll).
    */
    numDeallocateAll = 1u << 9,
    /**
    Chooses all $(D numXxx) flags.
    */
    numAll = (1u << 10) - 1,
    /**
    Tracks bytes currently allocated by this allocator. This number goes up
    and down as memory is allocated and deallocated, and is zero if the
    allocator currently has no active allocation.
    */
    bytesUsed = 1u << 10,
    /**
    Tracks total cumulative bytes allocated by means of $(D allocate),
    $(D expand), and $(D reallocate) (when resulting in an expansion). This
    number always grows and indicates allocation traffic. To compute bytes
    deallocated cumulatively, subtract $(D bytesUsed) from $(D bytesAllocated).
    */
    bytesAllocated = 1u << 11,
    /**
    Tracks the sum of all $(D delta) values in calls of the form
    $(D expand(b, delta)) that succeed (return $(D true)).
    */
    bytesExpanded = 1u << 12,
    /**
    Tracks the sum of all $(D b.length - s) with $(D b.length > s) in calls of
    the form $(D realloc(b, s)) that succeed (return $(D true)). In per-call
    statistics, also unambiguously counts the bytes deallocated with
    $(D deallocate).
    */
    bytesContracted = 1u << 13,
    /**
    Tracks the sum of all bytes moved as a result of calls to $(D realloc) that
    were unable to reallocate in place. A large number (relative to $(D
    bytesAllocated)) indicates that the application should use larger
    preallocations.
    */
    bytesMoved = 1u << 14,
    /**
    Tracks the sum of all bytes NOT moved as result of calls to $(D realloc)
    that managed to reallocate in place. A large number (relative to $(D
    bytesAllocated)) indicates that the application is expansion-intensive and
    is saving a good amount of moves. However, if this number is relatively
    small and $(D bytesSlack) is high, it means the application is
    overallocating for little benefit.
    */
    bytesNotMoved = 1u << 15,
    /**
    Measures the sum of extra bytes allocated beyond the bytes requested, i.e.
    the $(HTTP goo.gl/YoKffF, internal fragmentation). This is the current
    effective number of slack bytes, and it goes up and down with time.
    */
    bytesSlack = 1u << 16,
    /**
    Measures the maximum bytes allocated over the time. This is useful for
    dimensioning allocators.
    */
    bytesHighTide = 1u << 17,
    /**
    Chooses all $(D byteXxx) flags.
    */
    bytesAll = ((1u << 18) - 1) & ~numAll,
    /**
    Combines all flags above.
    */
    all = (1u << 18) - 1
}

/**

Allocator that collects extra data about allocations. Since each piece of
information adds size and time overhead, statistics can be individually enabled
or disabled through compile-time $(D flags).

All stats of the form $(D numXxx) record counts of events occurring, such as
calls to functions and specific results. The stats of the form $(D bytesXxx)
collect cumulative sizes.

In addition, the data $(D callerSize), $(D callerModule), $(D callerFile), $(D
callerLine), and $(D callerTime) is associated with each specific allocation.
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
        Read-only properties enabled by the homonym $(D flags) chosen by the
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

    /// Alignment offered is equal to $(D Allocator.alignment).
    alias alignment = Allocator.alignment;

    /**
    Increments $(D numOwns) (per instance and and per call) and forwards to $(D
    parent.owns(b)).
    */
    static if (hasMember!(Allocator, "owns"))
    {
        static if ((perCallFlags & Options.numOwns) == 0)
        Ternary owns(void[] b)
        { return ownsImpl(b); }
        else
        Ternary owns(string f = __FILE, uint n = line)(void[] b)
        { return ownsImpl!(f, n)(b); }
    }

    private Ternary ownsImpl(string f = null, uint n = 0)(void[] b)
    {
        up!"numOwns";
        addPerCall!(f, n, "numOwns")(1);
        return parent.owns(b);
    }

    /**
    Forwards to $(D parent.allocate). Affects per instance: $(D numAllocate),
    $(D bytesUsed), $(D bytesAllocated), $(D bytesSlack), $(D numAllocateOK),
    and $(D bytesHighTide). Affects per call: $(D numAllocate), $(D
    numAllocateOK), and $(D bytesAllocated).
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

    private void[] allocateImpl(string f = null, ulong n = 0)(size_t bytes)
    {
        auto result = parent.allocate(bytes);
        add!"bytesUsed"(result.length);
        add!"bytesAllocated"(result.length);
        immutable slack = this.goodAllocSize(result.length) - result.length;
        add!"bytesSlack"(slack);
        up!"numAllocate";
        add!"numAllocateOK"(result.length == bytes); // allocating 0 bytes is OK
        addPerCall!(f, n, "numAllocate", "numAllocateOK", "bytesAllocated")
            (1, result.length == bytes, result.length);
        return result;
    }

    /**
    Defined whether or not $(D Allocator.expand) is defined. Affects
    per instance: $(D numExpand), $(D numExpandOK), $(D bytesExpanded),
    $(D bytesSlack), $(D bytesAllocated), and $(D bytesUsed). Affects per call:
    $(D numExpand), $(D numExpandOK), $(D bytesExpanded), and
    $(D bytesAllocated).
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
    Defined whether or not $(D Allocator.reallocate) is defined. Affects
    per instance: $(D numReallocate), $(D numReallocateOK), $(D
    numReallocateInPlace), $(D bytesNotMoved), $(D bytesAllocated), $(D
    bytesSlack), $(D bytesExpanded), and $(D bytesContracted). Affects per call:
    $(D numReallocate), $(D numReallocateOK), $(D numReallocateInPlace),
    $(D bytesNotMoved), $(D bytesExpanded), $(D bytesContracted), and
    $(D bytesMoved).
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
    Defined whether or not $(D Allocator.deallocate) is defined. Affects
    per instance: $(D numDeallocate), $(D bytesUsed), and $(D bytesSlack).
    Affects per call: $(D numDeallocate) and $(D bytesContracted).
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
        Defined only if $(D Allocator.deallocateAll) is defined. Affects
        per instance and per call $(D numDeallocateAll).
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
    Defined only if $(D Options.bytesUsed) is defined. Returns $(D bytesUsed ==
    0).
    */
    static if (flags & Options.bytesUsed)
    Ternary empty()
    {
        return Ternary(_bytesUsed == 0);
    }

    /**
    Reports per instance statistics to $(D output) (e.g. $(D stdout)). The
    format is simple: one kind and value per line, separated by a colon, e.g.
    $(D bytesAllocated:7395404)
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
        Defined if $(D perCallFlags) is nonzero.
        */
        struct PerCallStatistics
        {
            /// The file and line of the call.
            string file;
            /// Ditto
            uint line;
            /// The options corresponding to the statistics collected.
            Options[] opts;
            /// The values of the statistics. Has the same length as $(D opts).
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
        Defined if $(D perCallFlags) is nonzero. Iterates all monitored
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
        Defined if $(D perCallFlags) is nonzero. Outputs (e.g. to a $(D File))
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
            enum uint mask = mixin("Options."~[names].join("|Options."));
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
    assert(File(f).byLine.walkLength == 22);
}

@system unittest
{
    void test(Allocator)()
    {
        import std.range : walkLength;
        import std.stdio : writeln;
        Allocator a;
        auto b1 = a.allocate(100);
        assert(a.numAllocate == 1);
        assert(a.expand(b1, 0));
        assert(a.reallocate(b1, b1.length + 1));
        auto b2 = a.allocate(101);
        assert(a.numAllocate == 2);
        assert(a.bytesAllocated == 202);
        assert(a.bytesUsed == 202);
        auto b3 = a.allocate(202);
        assert(a.numAllocate == 3);
        assert(a.bytesAllocated == 404);

        a.deallocate(b2);
        assert(a.numDeallocate == 1);
        a.deallocate(b1);
        assert(a.numDeallocate == 2);
        a.deallocate(b3);
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
        import std.stdio : writeln;
        Allocator a;
        auto b1 = a.allocate(100);
        assert(a.expand(b1, 0));
        assert(a.reallocate(b1, b1.length + 1));
        auto b2 = a.allocate(101);
        auto b3 = a.allocate(202);

        a.deallocate(b2);
        a.deallocate(b1);
        a.deallocate(b3);
    }
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    test!(StatsCollector!(GCAllocator, 0, 0));
}

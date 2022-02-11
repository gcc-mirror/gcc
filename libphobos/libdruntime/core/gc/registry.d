/**
 * Contains a registry for GC factories.
 *
 * Copyright: Copyright Digital Mars 2016.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 */
module core.gc.registry;

import core.gc.gcinterface : GC;

/*@nogc nothrow:*/

/**
 * A factory function that instantiates an implementation of the GC interface.
 * In case the instance was allocated on the C heap, it is supposed to
 * free itself upon calling it's destructor.
 *
 * The factory should print an error and abort the program if it
 * cannot successfully initialize the GC instance.
 */
alias GCFactory = GC function();

/**
 * Register a GC factory under the given `name`.  This function must be called
 * from a C constructor before druntime is initialized.
 *
 * To use the registered GC, it's name must be specified gcopt runtime option,
 * e.g. by passing $(TT, --DRT-gcopt=gc:my_gc_name) as application argument.
 *
 * Params:
 *   name = name of the GC implementation; should be unique
 *   factory = function to instantiate the implementation
 * Note: The registry does not perform synchronization, as registration is
 *   assumed to be executed serially, as is the case for C constructors.
 * See_Also:
 *   $(LINK2 https://dlang.org/spec/garbage.html#gc_config, Configuring the Garbage Collector)
 */
void registerGCFactory(string name, GCFactory factory) nothrow @nogc
{
    import core.stdc.stdlib : realloc;

    auto ptr = cast(Entry*)realloc(entries.ptr, (entries.length + 1) * Entry.sizeof);
    entries = ptr[0 .. entries.length + 1];
    entries[$ - 1] = Entry(name, factory);
}

/**
 * Called during runtime initialization to initialize a GC instance of given `name`.
 *
 * Params:
 *   name = name of the GC to instantiate
 * Returns:
 *   The created GC instance or `null` if no factory for that name was registered
 */
GC createGCInstance(string name)
{
    import core.stdc.stdlib : free;

    foreach (entry; entries)
    {
        if (entry.name != name)
            continue;
        auto instance = entry.factory();
        // only one GC at a time for now, so free the registry to not leak
        free(entries.ptr);
        entries = null;
        return instance;
    }
    return null;
}

// list of all registerd GCs
const(Entry[]) registeredGCFactories(scope int dummy=0) nothrow @nogc
{
    return entries;
}

private:

struct Entry
{
    string name;
    GCFactory factory;
}

__gshared Entry[] entries;

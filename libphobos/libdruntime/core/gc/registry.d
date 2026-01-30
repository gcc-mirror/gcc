/**
 * Contains a registry for GC factories.
 *
 * Copyright: Copyright Digital Mars 2016.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 */
module core.gc.registry;

import core.gc.gcinterface : GC;
import core.thread.threadbase : ThreadBase;

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
 * A function that will initialize a thread before the GC has been initialized.
 * Once the GC is initialized, the interface method GC.initThread for each new
 * thread.
 */
alias GCThreadInitFunction = void function(ThreadBase base) nothrow @nogc;

/**
 * Register a GC factory under the given `name`.  This function must be called
 * from a C constructor before druntime is initialized.
 *
 * To use the registered GC, it's name must be specified gcopt runtime option,
 * e.g. by passing $(TT, --DRT-gcopt=gc:my_gc_name) as application argument.
 *
 * The thread init function will be called *only before* the GC has been
 * initialized to the registered GC. It is called as the first step in starting
 * the new thread before the thread is registered with the runtime as a running
 * thread. This allows any specific thread data that is needed for running the
 * GC to be registered with the thread object.
 *
 * After the GC is initialized, the GC interface function `initThread` is
 * called instead. This function should expect the possibility that the
 * reciprocal `cleanupThread` method may not be called if the GC is never
 * initialized.
 *
 * Params:
 *   name = name of the GC implementation; should be unique
 *   factory = function to instantiate the implementation
 *   threadInit = function to call from a new thread *before* registration in
 *       the list of running threads, to set up any GC-specific data.
 * Note: The registry does not perform synchronization, as registration is
 *   assumed to be executed serially, as is the case for C constructors.
 * See_Also:
 *   $(LINK2 https://dlang.org/spec/garbage.html#gc_config, Configuring the Garbage Collector)
 */
void registerGCFactory(string name, GCFactory factory,
        GCThreadInitFunction threadInit = null) nothrow @nogc
{
    import core.stdc.stdlib : realloc;

    auto ptr = cast(Entry*)realloc(entries.ptr, (entries.length + 1) * Entry.sizeof);
    entries = ptr[0 .. entries.length + 1];
    entries[$ - 1] = Entry(name, factory, threadInit);
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

/**
 * Get the thread init function used for the selected GC.
 *
 * Note, this must be called before `createGCInstance`. It is typically called by
 * `rt_init`.
 */
GCThreadInitFunction threadInit(string name) nothrow @nogc
{
    foreach (entry; entries)
    {
        if (entry.name == name)
            return entry.threadInit;
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
    GCThreadInitFunction threadInit;
}

__gshared Entry[] entries;

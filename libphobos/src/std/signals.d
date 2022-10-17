// Written in the D programming language.

/**
 * Signals and Slots are an implementation of the Observer Pattern.
 * Essentially, when a Signal is emitted, a list of connected Observers
 * (called slots) are called.
 *
 * There have been several D implementations of Signals and Slots.
 * This version makes use of several new features in D, which make
 * using it simpler and less error prone. In particular, it is no
 * longer necessary to instrument the slots.
 *
 * References:
 *      $(LUCKY A Deeper Look at Signals and Slots)$(BR)
 *      $(LINK2 http://en.wikipedia.org/wiki/Observer_pattern, Observer pattern)$(BR)
 *      $(LINK2 http://en.wikipedia.org/wiki/Signals_and_slots, Wikipedia)$(BR)
 *      $(LINK2 http://boost.org/doc/html/$(SIGNALS).html, Boost Signals)$(BR)
 *      $(LINK2 http://qt-project.org/doc/qt-5/signalsandslots.html, Qt)$(BR)
 *
 *      There has been a great deal of discussion in the D newsgroups
 *      over this, and several implementations:
 *
 *      $(LINK2 http://www.digitalmars.com/d/archives/digitalmars/D/announce/signal_slots_library_4825.html, signal slots library)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/digitalmars/D/Signals_and_Slots_in_D_42387.html, Signals and Slots in D)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/digitalmars/D/Dynamic_binding_--_Qt_s_Signals_and_Slots_vs_Objective-C_42260.html, Dynamic binding -- Qt's Signals and Slots vs Objective-C)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/digitalmars/D/Dissecting_the_SS_42377.html, Dissecting the SS)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/digitalmars/D/dwt/about_harmonia_454.html, about harmonia)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/digitalmars/D/announce/1502.html, Another event handling module)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/digitalmars/D/41825.html, Suggestion: signal/slot mechanism)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/digitalmars/D/13251.html, Signals and slots?)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/digitalmars/D/10714.html, Signals and slots ready for evaluation)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/digitalmars/D/1393.html, Signals &amp; Slots for Walter)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/28456.html, Signal/Slot mechanism?)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/19470.html, Modern Features?)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/16592.html, Delegates vs interfaces)$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/16583.html, The importance of component programming (properties$(COMMA) signals and slots$(COMMA) etc))$(BR)
 *      $(LINK2 http://www.digitalmars.com/d/archives/16368.html, signals and slots)$(BR)
 *
 * Bugs:
 *      $(RED Slots can only be delegates referring directly to
 *      class or interface member functions. If a delegate to something else
 *      is passed to connect(), such as a struct member function,
 *      a nested function, a COM interface, a closure, undefined behavior
 *      will result.)
 *
 *      Not safe for multiple threads operating on the same signals
 *      or slots.
 * Macros:
 *      SIGNALS=signals
 *
 * Copyright: Copyright The D Language Foundation 2000 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright)
 * Source:    $(PHOBOSSRC std/signals.d)
 *
 * $(SCRIPT inhibitQuickIndex = 1;)
 */
/*          Copyright The D Language Foundation 2000 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.signals;

import core.exception : onOutOfMemoryError;
import core.stdc.stdlib : calloc, realloc, free;
import std.stdio;

// Special function for internal use only.
// Use of this is where the slot had better be a delegate
// to an object or an interface that is part of an object.
extern (C) Object _d_toObject(void* p);

// Used in place of Object.notifyRegister and Object.notifyUnRegister.
alias DisposeEvt = void delegate(Object);
extern (C) void  rt_attachDisposeEvent( Object obj, DisposeEvt evt );
extern (C) void  rt_detachDisposeEvent( Object obj, DisposeEvt evt );
//debug=signal;

/************************
 * Mixin to create a signal within a class object.
 *
 * Different signals can be added to a class by naming the mixins.
 */

mixin template Signal(T1...)
{
    static import core.exception;
    static import core.stdc.stdlib;
    /***
     * A slot is implemented as a delegate.
     * The slot_t is the type of the delegate.
     * The delegate must be to an instance of a class or an interface
     * to a class instance.
     * Delegates to struct instances or nested functions must not be
     * used as slots. This applies even if the nested function does not access
     * it's parent function variables.
     */
    alias slot_t = void delegate(T1);

    /***
     * Call each of the connected slots, passing the argument(s) i to them.
     * Nested call will be ignored.
     */
    final void emit( T1 i )
    {
        if (status >= ST.inemitting || !slots.length)
            return; // should not nest

        status = ST.inemitting;
        scope (exit)
            status = ST.idle;

        foreach (slot; slots[0 .. slots_idx])
        {   if (slot)
                slot(i);
        }

        assert(status >= ST.inemitting);
        if (status == ST.inemitting_disconnected)
        {
            for (size_t j = 0; j < slots_idx;)
            {
                if (slots[j] is null)
                {
                    slots_idx--;
                    slots[j] = slots[slots_idx];
                }
                else
                    j++;
            }
        }
    }

    /***
     * Add a slot to the list of slots to be called when emit() is called.
     */
    final void connect(slot_t slot)
    {
        /* Do this:
         *    slots ~= slot;
         * but use malloc() and friends instead
         */
        auto len = slots.length;
        if (slots_idx == len)
        {
            if (slots.length == 0)
            {
                len = 4;
                auto p = core.stdc.stdlib.calloc(slot_t.sizeof, len);
                if (!p)
                    core.exception.onOutOfMemoryError();
                slots = (cast(slot_t*) p)[0 .. len];
            }
            else
            {
                import core.checkedint : addu, mulu;
                bool overflow;
                len = addu(mulu(len, 2, overflow), 4, overflow); // len = len * 2 + 4
                const nbytes = mulu(len, slot_t.sizeof, overflow);
                if (overflow) assert(0);

                auto p = core.stdc.stdlib.realloc(slots.ptr, nbytes);
                if (!p)
                    core.exception.onOutOfMemoryError();
                slots = (cast(slot_t*) p)[0 .. len];
                slots[slots_idx + 1 .. $] = null;
            }
        }
        slots[slots_idx++] = slot;

     L1:
        Object o = _d_toObject(slot.ptr);
        rt_attachDisposeEvent(o, &unhook);
    }

    /***
     * Remove a slot from the list of slots to be called when emit() is called.
     */
    final void disconnect(slot_t slot)
    {
        debug (signal) writefln("Signal.disconnect(slot)");
        size_t disconnectedSlots = 0;
        size_t instancePreviousSlots = 0;
        if (status >= ST.inemitting)
        {
            foreach (i, sloti; slots[0 .. slots_idx])
            {
                if (sloti.ptr == slot.ptr &&
                    ++instancePreviousSlots &&
                    sloti == slot)
                {
                    disconnectedSlots++;
                    slots[i] = null;
                    status = ST.inemitting_disconnected;
                }
            }
        }
        else
        {
            for (size_t i = 0; i < slots_idx; )
            {
                if (slots[i].ptr == slot.ptr &&
                    ++instancePreviousSlots &&
                    slots[i] == slot)
                {
                    slots_idx--;
                    disconnectedSlots++;
                    slots[i] = slots[slots_idx];
                    slots[slots_idx] = null;        // not strictly necessary
                }
                else
                    i++;
            }
        }

         // detach object from dispose event if all its slots have been removed
        if (instancePreviousSlots == disconnectedSlots)
        {
            Object o = _d_toObject(slot.ptr);
            rt_detachDisposeEvent(o, &unhook);
        }
     }

    /***
     * Disconnect all the slots.
     */
    final void disconnectAll()
    {
        debug (signal) writefln("Signal.disconnectAll");
        __dtor();
        slots_idx = 0;
        status = ST.idle;
    }

    /* **
     * Special function called when o is destroyed.
     * It causes any slots dependent on o to be removed from the list
     * of slots to be called by emit().
     */
    final void unhook(Object o)
    in { assert( status == ST.idle ); }
    do
    {
        debug (signal) writefln("Signal.unhook(o = %s)", cast(void*) o);
        for (size_t i = 0; i < slots_idx; )
        {
            if (_d_toObject(slots[i].ptr) is o)
            {   slots_idx--;
                slots[i] = slots[slots_idx];
                slots[slots_idx] = null;        // not strictly necessary
            }
            else
                i++;
        }
    }

    /* **
     * There can be multiple destructors inserted by mixins.
     */
    ~this()
    {
        /* **
         * When this object is destroyed, need to let every slot
         * know that this object is destroyed so they are not left
         * with dangling references to it.
         */
        if (slots.length)
        {
            foreach (slot; slots[0 .. slots_idx])
            {
                if (slot)
                {   Object o = _d_toObject(slot.ptr);
                    rt_detachDisposeEvent(o, &unhook);
                }
            }
            core.stdc.stdlib.free(slots.ptr);
            slots = null;
        }
    }

  private:
    slot_t[] slots;             // the slots to call from emit()
    size_t slots_idx;           // used length of slots[]

    enum ST { idle, inemitting, inemitting_disconnected }
    ST status;
}

///
@system unittest
{
    import std.signals;

    int observedMessageCounter = 0;

    class Observer
    {   // our slot
        void watch(string msg, int value)
        {
            switch (observedMessageCounter++)
            {
                case 0:
                    assert(msg == "setting new value");
                    assert(value == 4);
                    break;
                case 1:
                    assert(msg == "setting new value");
                    assert(value == 6);
                    break;
                default:
                    assert(0, "Unknown observation");
            }
        }
    }

    class Observer2
    {   // our slot
        void watch(string msg, int value)
        {
        }
    }

    class Foo
    {
        int value() { return _value; }

        int value(int v)
        {
            if (v != _value)
            {   _value = v;
                // call all the connected slots with the two parameters
                emit("setting new value", v);
            }
            return v;
        }

        // Mix in all the code we need to make Foo into a signal
        mixin Signal!(string, int);

      private :
        int _value;
    }

    Foo a = new Foo;
    Observer o = new Observer;
    auto o2 = new Observer2;
    auto o3 = new Observer2;
    auto o4 = new Observer2;
    auto o5 = new Observer2;

    a.value = 3;                // should not call o.watch()
    a.connect(&o.watch);        // o.watch is the slot
    a.connect(&o2.watch);
    a.connect(&o3.watch);
    a.connect(&o4.watch);
    a.connect(&o5.watch);
    a.value = 4;                // should call o.watch()
    a.disconnect(&o.watch);     // o.watch is no longer a slot
    a.disconnect(&o3.watch);
    a.disconnect(&o5.watch);
    a.disconnect(&o4.watch);
    a.disconnect(&o2.watch);
    a.value = 5;                // so should not call o.watch()
    a.connect(&o2.watch);
    a.connect(&o.watch);        // connect again
    a.value = 6;                // should call o.watch()
    destroy(o);                 // destroying o should automatically disconnect it
    a.value = 7;                // should not call o.watch()

    assert(observedMessageCounter == 2);
}

// A function whose sole purpose is to get this module linked in
// so the unittest will run.
void linkin() { }

@system unittest
{
    class Observer
    {
        void watch(string msg, int i)
        {
            //writefln("Observed msg '%s' and value %s", msg, i);
            captured_value = i;
            captured_msg   = msg;
        }

        int    captured_value;
        string captured_msg;
    }

    class Foo
    {
        @property int value() { return _value; }

        @property int value(int v)
        {
            if (v != _value)
            {   _value = v;
                emit("setting new value", v);
            }
            return v;
        }

        mixin Signal!(string, int);

      private:
        int _value;
    }

    Foo a = new Foo;
    Observer o = new Observer;

    // check initial condition
    assert(o.captured_value == 0);
    assert(o.captured_msg == "");

    // set a value while no observation is in place
    a.value = 3;
    assert(o.captured_value == 0);
    assert(o.captured_msg == "");

    // connect the watcher and trigger it
    a.connect(&o.watch);
    a.value = 4;
    assert(o.captured_value == 4);
    assert(o.captured_msg == "setting new value");

    // disconnect the watcher and make sure it doesn't trigger
    a.disconnect(&o.watch);
    a.value = 5;
    assert(o.captured_value == 4);
    assert(o.captured_msg == "setting new value");

    // reconnect the watcher and make sure it triggers
    a.connect(&o.watch);
    a.value = 6;
    assert(o.captured_value == 6);
    assert(o.captured_msg == "setting new value");

    // destroy the underlying object and make sure it doesn't cause
    // a crash or other problems
    destroy(o);
    a.value = 7;
}

@system unittest
{
    class Observer
    {
        int    i;
        long   l;
        string str;

        void watchInt(string str, int i)
        {
            this.str = str;
            this.i = i;
        }

        void watchLong(string str, long l)
        {
            this.str = str;
            this.l = l;
        }
    }

    class Bar
    {
        @property void value1(int v)  { s1.emit("str1", v); }
        @property void value2(int v)  { s2.emit("str2", v); }
        @property void value3(long v) { s3.emit("str3", v); }

        mixin Signal!(string, int)  s1;
        mixin Signal!(string, int)  s2;
        mixin Signal!(string, long) s3;
    }

    void test(T)(T a) {
        auto o1 = new Observer;
        auto o2 = new Observer;
        auto o3 = new Observer;

        // connect the watcher and trigger it
        a.s1.connect(&o1.watchInt);
        a.s2.connect(&o2.watchInt);
        a.s3.connect(&o3.watchLong);

        assert(!o1.i && !o1.l && o1.str == null);
        assert(!o2.i && !o2.l && o2.str == null);
        assert(!o3.i && !o3.l && o3.str == null);

        a.value1 = 11;
        assert(o1.i == 11 && !o1.l && o1.str == "str1");
        assert(!o2.i && !o2.l && o2.str == null);
        assert(!o3.i && !o3.l && o3.str == null);
        o1.i = -11; o1.str = "x1";

        a.value2 = 12;
        assert(o1.i == -11 && !o1.l && o1.str == "x1");
        assert(o2.i == 12 && !o2.l && o2.str == "str2");
        assert(!o3.i && !o3.l && o3.str == null);
        o2.i = -12; o2.str = "x2";

        a.value3 = 13;
        assert(o1.i == -11 && !o1.l && o1.str == "x1");
        assert(o2.i == -12 && !o1.l && o2.str == "x2");
        assert(!o3.i && o3.l == 13 && o3.str == "str3");
        o3.l = -13; o3.str = "x3";

        // disconnect the watchers and make sure it doesn't trigger
        a.s1.disconnect(&o1.watchInt);
        a.s2.disconnect(&o2.watchInt);
        a.s3.disconnect(&o3.watchLong);

        a.value1 = 21;
        a.value2 = 22;
        a.value3 = 23;
        assert(o1.i == -11 && !o1.l && o1.str == "x1");
        assert(o2.i == -12 && !o1.l && o2.str == "x2");
        assert(!o3.i && o3.l == -13 && o3.str == "x3");

        // reconnect the watcher and make sure it triggers
        a.s1.connect(&o1.watchInt);
        a.s2.connect(&o2.watchInt);
        a.s3.connect(&o3.watchLong);

        a.value1 = 31;
        a.value2 = 32;
        a.value3 = 33;
        assert(o1.i == 31 && !o1.l && o1.str == "str1");
        assert(o2.i == 32 && !o1.l && o2.str == "str2");
        assert(!o3.i && o3.l == 33 && o3.str == "str3");

        // destroy observers
        destroy(o1);
        destroy(o2);
        destroy(o3);
        a.value1 = 41;
        a.value2 = 42;
        a.value3 = 43;
    }

    test(new Bar);

    class BarDerived: Bar
    {
        @property void value4(int v)  { s4.emit("str4", v); }
        @property void value5(int v)  { s5.emit("str5", v); }
        @property void value6(long v) { s6.emit("str6", v); }

        mixin Signal!(string, int)  s4;
        mixin Signal!(string, int)  s5;
        mixin Signal!(string, long) s6;
    }

    auto a = new BarDerived;

    test!Bar(a);
    test!BarDerived(a);

    auto o4 = new Observer;
    auto o5 = new Observer;
    auto o6 = new Observer;

    // connect the watcher and trigger it
    a.s4.connect(&o4.watchInt);
    a.s5.connect(&o5.watchInt);
    a.s6.connect(&o6.watchLong);

    assert(!o4.i && !o4.l && o4.str == null);
    assert(!o5.i && !o5.l && o5.str == null);
    assert(!o6.i && !o6.l && o6.str == null);

    a.value4 = 44;
    assert(o4.i == 44 && !o4.l && o4.str == "str4");
    assert(!o5.i && !o5.l && o5.str == null);
    assert(!o6.i && !o6.l && o6.str == null);
    o4.i = -44; o4.str = "x4";

    a.value5 = 45;
    assert(o4.i == -44 && !o4.l && o4.str == "x4");
    assert(o5.i == 45 && !o5.l && o5.str == "str5");
    assert(!o6.i && !o6.l && o6.str == null);
    o5.i = -45; o5.str = "x5";

    a.value6 = 46;
    assert(o4.i == -44 && !o4.l && o4.str == "x4");
    assert(o5.i == -45 && !o4.l && o5.str == "x5");
    assert(!o6.i && o6.l == 46 && o6.str == "str6");
    o6.l = -46; o6.str = "x6";

    // disconnect the watchers and make sure it doesn't trigger
    a.s4.disconnect(&o4.watchInt);
    a.s5.disconnect(&o5.watchInt);
    a.s6.disconnect(&o6.watchLong);

    a.value4 = 54;
    a.value5 = 55;
    a.value6 = 56;
    assert(o4.i == -44 && !o4.l && o4.str == "x4");
    assert(o5.i == -45 && !o4.l && o5.str == "x5");
    assert(!o6.i && o6.l == -46 && o6.str == "x6");

    // reconnect the watcher and make sure it triggers
    a.s4.connect(&o4.watchInt);
    a.s5.connect(&o5.watchInt);
    a.s6.connect(&o6.watchLong);

    a.value4 = 64;
    a.value5 = 65;
    a.value6 = 66;
    assert(o4.i == 64 && !o4.l && o4.str == "str4");
    assert(o5.i == 65 && !o4.l && o5.str == "str5");
    assert(!o6.i && o6.l == 66 && o6.str == "str6");

    // destroy observers
    destroy(o4);
    destroy(o5);
    destroy(o6);
    a.value4 = 44;
    a.value5 = 45;
    a.value6 = 46;
}

// Triggers bug from issue 15341
@system unittest
{
    class Observer
    {
       void watch() { }
       void watch2() { }
    }

    class Bar
    {
       mixin Signal!();
    }

   auto a = new Bar;
   auto o = new Observer;

   //Connect both observer methods for the same instance
   a.connect(&o.watch);
   a.connect(&o.watch2); // not connecting watch2() or disconnecting it manually fixes the issue

   //Disconnect a single method of the two
   a.disconnect(&o.watch); // NOT disconnecting watch() fixes the issue

   destroy(o); // destroying o should automatically call unhook and disconnect the slot for watch2
   a.emit(); // should not raise segfault since &o.watch2 is no longer connected
}

version (none) // Disabled because of https://issues.dlang.org/show_bug.cgi?id=5028
@system unittest
{
    class A
    {
        mixin Signal!(string, int) s1;
    }

    class B : A
    {
        mixin Signal!(string, int) s2;
    }
}

// Triggers bug from issue 16249
@system unittest
{
    class myLINE
    {
        mixin Signal!( myLINE, int );

        void value( int v )
        {
            if ( v >= 0 ) emit( this, v );
            else          emit( new myLINE, v );
        }
    }

    class Dot
    {
        int value;

        myLINE line_;
        void line( myLINE line_x )
        {
            if ( line_ is line_x ) return;

            if ( line_ !is null )
            {
                line_.disconnect( &watch );
            }
            line_ = line_x;
            line_.connect( &watch );
        }

        void watch( myLINE line_x, int value_x )
        {
            line = line_x;
            value = value_x;
        }
    }

    auto dot1 = new Dot;
    auto dot2 = new Dot;
    auto line = new myLINE;
    dot1.line = line;
    dot2.line = line;

    line.value = 11;
    assert( dot1.value == 11 );
    assert( dot2.value == 11 );

    line.value = -22;
    assert( dot1.value == -22 );
    assert( dot2.value == -22 );
}

@system unittest
{
    import std.signals;

    class Observer
    {   // our slot
        void watch(string msg, int value)
        {
            if (value != 0)
            {
                assert(msg == "setting new value");
                assert(value == 1);
            }
        }
    }

    class Foo
    {
        int value() { return _value; }

        int value(int v)
        {
            if (v != _value)
            {
                _value = v;
                // call all the connected slots with the parameters
                emit("setting new value", v);
            }
            return v;
        }

        // Mix in all the code we need to make Foo into a signal
        mixin Signal!(string, int);

      private :
        int _value;
    }

    Foo a = new Foo;
    Observer o = new Observer;
    auto o2 = new Observer;

    a.value = 3;                // should not call o.watch()
    a.connect(&o.watch);        // o.watch is the slot
    a.connect(&o2.watch);
    a.value = 1;                // should call o.watch()
    a.disconnectAll();
    a.value = 5;                // so should not call o.watch()
    a.connect(&o.watch);        // connect again
    a.connect(&o2.watch);
    a.value = 1;                // should call o.watch()
    destroy(o);                 // destroying o should automatically disconnect it
    destroy(o2);
    a.value = 7;                // should not call o.watch()
}

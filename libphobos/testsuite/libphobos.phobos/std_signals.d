@system unittest
{
    import std.signals;

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


@system unittest
{
    import std.concurrency;

    __gshared string received;
    static void spawnedFunc(Tid ownerTid)
    {
        import std.conv : text;
        // Receive a message from the owner thread.
        receive((int i){
            received = text("Received the number ", i);

            // Send a message back to the owner thread
            // indicating success.
            send(ownerTid, true);
        });
    }

    // Start spawnedFunc in a new thread.
    auto childTid = spawn(&spawnedFunc, thisTid);

    // Send the number 42 to this new thread.
    send(childTid, 42);

    // Receive the result code.
    auto wasSuccessful = receiveOnly!(bool);
    assert(wasSuccessful);
    assert(received == "Received the number 42");
}

@system unittest
{
    import std.concurrency;

    static void f(string msg)
    {
        assert(msg == "Hello World");
    }

    auto tid = spawn(&f, "Hello World");
}

@system unittest
{
    import std.concurrency;

    string msg = "Hello, World!";

    static void f1(string msg) {}
    static assert(!__traits(compiles, spawn(&f1, msg.dup)));
    static assert( __traits(compiles, spawn(&f1, msg.idup)));

    static void f2(char[] msg) {}
    static assert(!__traits(compiles, spawn(&f2, msg.dup)));
    static assert(!__traits(compiles, spawn(&f2, msg.idup)));
}

@system unittest
{
    import std.concurrency;

    spawn({
        ownerTid.send("This is so great!");
    });
    assert(receiveOnly!string == "This is so great!");
}

@system unittest
{
    import std.concurrency;

    import std.variant : Variant;

    auto process = ()
    {
        receive(
            (int i) { ownerTid.send(1); },
            (double f) { ownerTid.send(2); },
            (Variant v) { ownerTid.send(3); }
        );
    };

    {
        auto tid = spawn(process);
        send(tid, 42);
        assert(receiveOnly!int == 1);
    }

    {
        auto tid = spawn(process);
        send(tid, 3.14);
        assert(receiveOnly!int == 2);
    }

    {
        auto tid = spawn(process);
        send(tid, "something else");
        assert(receiveOnly!int == 3);
    }
}

@system unittest
{
    import std.concurrency;

    auto tid = spawn(
    {
        assert(receiveOnly!int == 42);
    });
    send(tid, 42);
}

@system unittest
{
    import std.concurrency;

    auto tid = spawn(
    {
        assert(receiveOnly!string == "text");
    });
    send(tid, "text");
}

@system unittest
{
    import std.concurrency;

    struct Record { string name; int age; }

    auto tid = spawn(
    {
        auto msg = receiveOnly!(double, Record);
        assert(msg[0] == 0.5);
        assert(msg[1].name == "Alice");
        assert(msg[1].age == 31);
    });

    send(tid, 0.5, Record("Alice", 31));
}

@system unittest
{
    import std.concurrency;

    auto tid = spawn({
        int i;
        while (i < 9)
            i = receiveOnly!int;

        ownerTid.send(i * 2);
    });

    auto r = new Generator!int({
        foreach (i; 1 .. 10)
            yield(i);
    });

    foreach (e; r)
        tid.send(e);

    assert(receiveOnly!int == 18);
}

@system unittest
{
    import std.concurrency;

    import std.range;

    InputRange!int myIota = iota(10).inputRangeObject;

    myIota.popFront();
    myIota.popFront();
    assert(myIota.moveFront == 2);
    assert(myIota.front == 2);
    myIota.popFront();
    assert(myIota.front == 3);

    //can be assigned to std.range.interfaces.InputRange directly
    myIota = new Generator!int(
    {
        foreach (i; 0 .. 10) yield(i);
    });

    myIota.popFront();
    myIota.popFront();
    assert(myIota.moveFront == 2);
    assert(myIota.front == 2);
    myIota.popFront();
    assert(myIota.front == 3);

    size_t[2] counter = [0, 0];
    foreach (i, unused; myIota) counter[] += [1, i];

    assert(myIota.empty);
    assert(counter == [7, 21]);
}

@system unittest
{
    import std.concurrency;

    static class MySingleton
    {
        static MySingleton instance()
        {
            __gshared MySingleton inst;
            return initOnce!inst(new MySingleton);
        }
    }

    assert(MySingleton.instance !is null);
}

@system unittest
{
    import std.concurrency;

    import core.sync.mutex : Mutex;

    static shared bool varA, varB;
    static shared Mutex m;
    m = new shared Mutex;

    spawn({
        // use a different mutex for varB to avoid a dead-lock
        initOnce!varB(true, m);
        ownerTid.send(true);
    });
    // init depends on the result of the spawned thread
    initOnce!varA(receiveOnly!bool);
    assert(varA == true);
    assert(varB == true);
}


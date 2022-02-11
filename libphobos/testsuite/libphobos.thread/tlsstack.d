module core.thread.test; // needs access to getStackTop()/getStackBottom()

import core.stdc.stdio;
import core.thread;

ubyte[16384] data;

void showThreadInfo() nothrow
{
    try
    {
        auto top = getStackTop();
        auto bottom = getStackBottom();
        printf("tlsdata:     %p\n", data.ptr);
        printf("stack top:   %p\n", getStackTop());
        printf("stack bottom:%p\n", getStackBottom());
        printf("used stack:  %lld\n", cast(ulong)(bottom - top));
    }
    catch(Exception e)
    {
        assert(false, e.msg);
    }
}

void main()
{
    printf("### main\n");
    showThreadInfo();

    printf("### thread\n");
    auto th = new Thread(&showThreadInfo, 16384);
    th.start();
    th.join();

    printf("### lowlevel thread\n");
    auto llth = createLowLevelThread(() { showThreadInfo(); });
    joinLowLevelThread(llth);
}

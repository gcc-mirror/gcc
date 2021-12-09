
// https://issues.dlang.org/show_bug.cgi?id=20256

extern(C) __gshared string[] rt_options = [ "gcopt=parallel:1" ];

void main()
{
    version (Posix)
    {
        import core.sys.posix.signal;
        import core.sys.posix.unistd;
        import core.thread;
        import core.memory;

        sigset_t m;
        sigemptyset(&m);
        sigaddset(&m, SIGHUP);

        auto x = new int[](10000);
        foreach (i; 0 .. 10000)
        {
            x ~= i;
        }
        GC.collect();  // GC create thread

        sigprocmask(SIG_BLOCK, &m, null); // block SIGHUP from delivery to main thread

        auto parent_pid = getpid();
        auto child_pid = fork();
        assert(child_pid >= 0);
        if (child_pid == 0)
        {
            kill(parent_pid, SIGHUP); // send signal to parent
            _exit(0);
        }
        // parent
        Thread.sleep(100.msecs);
        // if we are here, then GC threads didn't receive SIGHUP,
        // otherwise whole process killed
        _exit(0);
    }
}

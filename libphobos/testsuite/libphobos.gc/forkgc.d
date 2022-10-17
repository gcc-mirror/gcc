import core.memory;
import core.stdc.stdio;
import core.sys.posix.sys.wait;
import core.sys.posix.unistd;

void main()
{
    printf("[parent] Creating garbage...\n");
    foreach (n; 0 .. 1_000)
        new uint[10_000];
    printf("[parent] Collecting garbage...\n");
    GC.collect();
    printf("[parent] Forking...\n");
    auto i = fork();
    if (i < 0)
        assert(false, "Fork failed");
    if (i == 0)
    {
        printf("[child] In fork.\n");
        printf("[child] Creating garbage...\n");
        foreach (n; 0 .. 1_000)
            new uint[10_000];
        printf("[child] Collecting garbage...\n");
        GC.collect();
        printf("[child] Exiting fork.\n");
    }
    else
    {
        printf("[parent] Waiting for fork (PID %d).\n", i);
        int status;
        i = waitpid(i, &status, 0);
        printf("[parent] Fork %d exited (%d).\n", i, status);
        if (status != 0)
            assert(false, "child had errors");
    }
}

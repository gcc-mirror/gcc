// https://issues.dlang.org/show_bug.cgi?id=20270
import core.sys.posix.sys.wait : waitpid;
import core.sys.posix.unistd : fork, _exit;
import core.thread : Thread;

void main()
{
    foreach (t; 0 .. 10)
        new Thread({
            foreach (n; 0 .. 100)
            {
                foreach (x; 0 .. 100)
                    new ubyte[x];
                auto f = fork();
                assert(f >= 0);
                if (f == 0)
                    _exit(0);
                else
                    waitpid(f, null, 0);
            }
        }).start();
}

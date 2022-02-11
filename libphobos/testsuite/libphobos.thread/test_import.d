// https://issues.dlang.org/show_bug.cgi?id=20447
void main()
{
    import core.thread;
    int[] x;
    auto b = x.dup;
}

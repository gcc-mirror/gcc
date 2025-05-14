// https://issues.dlang.org/show_bug.cgi?id=22523

import core.stdc.stdio : puts;

int main()
{
    puts("Executed main although it should be skipped!");
    return 1;
}

unittest {}

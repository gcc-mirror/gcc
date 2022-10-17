/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=20088

struct S {
    int i;
}

extern(C) int main() @nogc nothrow pure
{
    S[2] s = [S(1),S(2)];
    void[] v = cast(void[])s;
    S[] p = cast(S[])v; // cast of void[] to S[] triggers __ArrayCast template function
    return 0;
}

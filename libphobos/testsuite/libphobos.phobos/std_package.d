@safe unittest
{
    import std;

    import std;

    int len;
    const r = 6.iota
              .filter!(a => a % 2) // 1 3 5
              .map!(a => a * 2) // 2 6 10
              .tee!(_ => len++)
              .substitute(6, -6) // 2 -6 10
              .sum
              .reverseArgs!format("Sum: %d");

    assert(len == 3);
    assert(r == "Sum: 6");
}

@safe unittest
{
    import std;

    import std;
    assert(10.iota.map!(a => pow(2, a)).sum == 1023);
}


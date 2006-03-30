class One
{
    long l;    // no ICE if this is int, not long
    int b;     // no ICE if this line is gone; type doesn't matter
}

public class pr26042
{
    class Three extends One { }
    Three three () { return new Three (); }
}


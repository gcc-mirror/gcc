// { dg-shouldfail "static_dtor_exception" }
// { dg-output "object.Exception@.*: static_dtor_exception" }
// https://issues.dlang.org/show_bug.cgi?id=16594
import core.stdc.stdio;

shared static ~this()
{
    __gshared int count;

    if (count++) fprintf(stderr, "dtor_called_more_than_once");
    else throw new Exception("static_dtor_exception");
}

void main()
{
}

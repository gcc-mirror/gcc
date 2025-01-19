/++
For testing only.

Overload set used in std.sumtype example. Needs its own internal module so that
it can be available for `make publictests` without polluting the public API.
+/
module std.internal.test.sumtype_example_overloads;

import std.sumtype;

@safe
{
    string handle(int) { return "got an int"; }
    string handle(string) { return "got a string"; }
    string handle(double) { return "got a double"; }
    alias handle = match!handle;
}

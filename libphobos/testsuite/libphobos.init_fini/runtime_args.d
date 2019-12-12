// Bugzilla 11149 - Runtime.args no longer available in static constructors
import core.runtime;

shared static this()
{
    assert(Runtime.cArgs.argc > 0);
    assert(Runtime.cArgs.argv !is null);
    assert(Runtime.args.length > 0);
}

void main()
{
}

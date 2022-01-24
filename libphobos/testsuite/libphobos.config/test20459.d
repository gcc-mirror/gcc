void main (string[] args)
{
    assert(args.length == 5);
    assert(args[1 .. $] == [ "foo", "bar", "--", "--DRT-gcopts=profile:1" ]);
}

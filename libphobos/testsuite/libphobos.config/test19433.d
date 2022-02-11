extern(C) __gshared bool rt_cmdline_enabled = false;

void main(string[] args)
{
    assert(args.length == 2);
    assert(args[1] == "--DRT-dont-eat-me");
}

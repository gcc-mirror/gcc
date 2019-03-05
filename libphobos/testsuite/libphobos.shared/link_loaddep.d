import libloaddep;

void main(string[] args)
{
    auto libname = args[0][0..$-"link_loaddep".length] ~ "lib.so\0";
    runDepTests(libname.ptr);
}

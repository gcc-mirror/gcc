import core.runtime, core.time : MonoTime;
import core.stdc.stdio;

ModuleInfo* getModuleInfo(string name)
{
    foreach (m; ModuleInfo)
        if (m.name == name) return m;
    assert(0, "module '"~name~"' not found");
}

bool tester()
{
    return Runtime.args.length > 1 ? testModules() : printAll();
}

string mode;


bool testModules()
{
    bool ret = true;
    foreach(name; Runtime.args[1..$])
    {
        immutable pkg = ".package";
        immutable pkgLen = pkg.length;

        if (name.length > pkgLen && name[$ - pkgLen .. $] == pkg)
            name = name[0 .. $ - pkgLen];

        doTest(getModuleInfo(name), ret);
    }

    return ret;
}

bool printAll()
{
    foreach (m; ModuleInfo)
    {
        if (m.unitTest)
        {
            string name = m.name;
            printf("%.*s\n", cast(int)name.length, name.ptr);
        }
    }
    return true;
}


void doTest(ModuleInfo* moduleInfo, ref bool ret)
{
    if (auto fp = moduleInfo.unitTest)
    {
        auto name = moduleInfo.name;
        try
        {
            immutable t0 = MonoTime.currTime;
            fp();
            printf("%.3fs PASS %.*s %.*s\n",
                   (MonoTime.currTime - t0).total!"msecs" / 1000.0,
                   cast(uint)mode.length, mode.ptr,
                   cast(uint)name.length, name.ptr);
        }
        catch (Throwable e)
        {
            auto msg = e.toString();
            printf("****** FAIL %.*s %.*s\n%.*s\n",
                   cast(uint)mode.length, mode.ptr,
                   cast(uint)name.length, name.ptr,
                   cast(uint)msg.length, msg.ptr);
            ret = false;
        }
    }
}


shared static this()
{
    version(D_Coverage)
    {
        import core.runtime : dmd_coverSetMerge;
        dmd_coverSetMerge(true);
    }
    Runtime.moduleUnitTester = &tester;

    debug mode = "debug";
    else  mode =  "release";
    static if ((void*).sizeof == 4) mode ~= "32";
    else static if ((void*).sizeof == 8) mode ~= "64";
    else static assert(0, "You must be from the future!");
}

void main()
{
}

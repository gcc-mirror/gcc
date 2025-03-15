import core.runtime;

UnitTestResult customModuleUnitTester()
{
    version(GoodTests) return UnitTestResult(100, 100, false, true);
    version(FailedTests) return UnitTestResult(100, 0, false, true);
    version(NoTests) return UnitTestResult(0, 0, true, false);
    version(FailNoPrintout) return UnitTestResult(100, 0, false, false);
    version(PassNoPrintout) return UnitTestResult(100, 100, false, false);
}

shared static this()
{
    Runtime.extendedModuleUnitTester = &customModuleUnitTester;
}

void main()
{
    import core.stdc.stdio : fprintf, stderr;
    fprintf(stderr, "main\n");
}

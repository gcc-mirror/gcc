import core.stdc.stdio : fprintf, stderr;

class MyException : Exception
{
    this() { super(typeof(this).stringof); }
}

void throw_catch()
{
    try
    {
        throw new MyException;
    }
    catch (MyException)
    {
    }
    catch (Exception)
    {
        assert(false);
    }
}

// Test that exceptions that are entirely thrown and caught in finally blocks don't affect exception handling.
void test1()
{
    try
    {
        try
        {
            throw new Exception("p");
        }
        finally
        {
            throw_catch();
        }
    }
    catch (Exception e)
    {
        assert(e.msg == "p");
    }
}

// Test that exceptions that are entirely thrown and caught in finally blocks don't interfere with chaining.
void test2()
{
    try
    {
        try
        {
            try
            {
                throw new Exception("p");
            }
            finally
            {
                throw new Exception("q");
            }
        }
        finally
        {
            throw_catch();
        }
    }
    catch(Exception e)
    {
        assert(e.msg == "p");
        assert(e.next.msg == "q");
        assert(!e.next.next);
    }
}

void test3()
{
    try
    {
        try
        {
            try
            {
                throw new Exception("p");
            }
            finally
            {
                throw_catch();
            }
        }
        finally
        {
            throw new Exception("q");
        }
    }
    catch(Exception e)
    {
        assert(e.msg == "p");
        assert(e.next.msg == "q");
        assert(!e.next.next);
    }
}

// Test order of exception handler operations.
void test4()
{
    string result;
    void throw_catch()
    {
        pragma(inline, false);
        try
        {
            result ~= "b";
            throw new MyException;
        }
        catch (MyException)
        {
            result ~= "c";
        }
        catch (Exception)
        {
            assert(false);
        }
    }
    try
    {
        try
        {
            result ~= "a";
            throw new Exception("");
        }
        finally
        {
            throw_catch();
        }
    }
    catch(Exception e)
    {
        result ~= "d";
    }
    assert(result == "abcd");
}

void test5()
{
    string result;
    void fail()
    {
        result ~= "b";
        throw new Exception("a");
    }

    void throw_catch()
    {
        pragma(inline, false);
        try
        {
            fail();
        }
        catch(Exception e)
        {
            assert(e.msg == "a");
            assert(!e.next);
            result ~= "c";
        }
    }
    try
    {
        try
        {
            result ~= "a";
            throw new Exception("x");
        }
        finally
        {
            throw_catch();
        }
    }
    catch (Exception e)
    {
        assert(e.msg == "x");
        assert(!e.next);
        result ~= "d";
    }
    assert(result == "abcd");
}

void main() {
    test1();
    test2();
    test3();
    test4();
    test5();
    fprintf(stderr, "success.\n");
}

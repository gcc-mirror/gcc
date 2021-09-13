// { dg-output "object.Exception@.*: exception" }
void main()
{
    try
    {
        f1();
    }
    catch (Exception e)
    {
        import core.stdc.stdio;
        auto str = e.toString();
        printf("%.*s\n", cast(int)str.length, str.ptr);
    }
}

void f1()
{
    throw new Exception("exception");
}

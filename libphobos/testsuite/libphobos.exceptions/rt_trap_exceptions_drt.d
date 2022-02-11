// { dg-shouldfail "uncaught exception" }
void test()
{
    int innerLocal = 20;
    throw new Exception("foo");
}
void main(string[] args)
{
    string myLocal = "bar";
    test();
}

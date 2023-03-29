// { dg-options "-fpreview=dip1008" }
class E : Exception
{
    static int instances;
    this(string msg = "", Throwable nextInChain = null)
    {
        super(msg, nextInChain);
        instances++;
    }

    ~this()
    {
        instances--;
    }
}

void main()
{
    alias chain = Exception.chainTogether;

    assert(chain(null, null) is null);

    try
    {
        throw new E();
    }
    catch (E e)
    {
        assert(E.instances == 1);
        assert(e.refcount == 2);
    }

    assert(E.instances == 0);

    try
    {
        throw new E();
    }
    catch (E e)
    {
        assert(chain(null, e) is e);
        assert(e.refcount == 2); // "Owned by e" + 1
    }

    assert(E.instances == 0);

    try
    {
        throw new E();
    }
    catch (E e)
    {
        assert(chain(e, null) is e);
        assert(e.refcount == 2); // "Owned by e" + 1
    }

    assert(E.instances == 0);

    try
    {
        throw new E("first");
    }
    catch (E first)
    {
        try
        {
            throw new E("second");
        }
        catch (E second)
        {
            try
            {
                throw new E("third");
            }
            catch (E third)
            {
                assert(chain(first, second) is first);
                assert(first.next is second);
                assert(second.next is null);

                assert(chain(first, third) is first);
                assert(first.next is second);
                assert(second.next is third);
                assert(third.next is null);

                assert(first.refcount == 2);
                assert(second.refcount == 3);
                assert(third.refcount == 3);
            }
        }

        assert(E.instances == 3);
    }

    assert(E.instances == 0);

    try
    {
        throw new E("first");
    }
    catch (E first)
    {
        assert(first.refcount == 2);
        assert(E.instances == 1);

        try
        {
            throw new E("second", first);
        }
        catch (E second)
        {
            assert(first.next is null);
            assert(second.next is first);

            assert(first.refcount == 3);
            assert(second.refcount == 2);

            assert(E.instances == 2);
        }

        assert(first.refcount == 2);
        assert(E.instances == 1);
    }

    assert(E.instances == 0);
}

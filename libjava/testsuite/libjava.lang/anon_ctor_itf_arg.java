/* From java/3285, By p.thio@valescom.com */

interface I
{
    void print ();
};

class C1
implements I
{
    public void print () { System.out.println ("C1: Message"); }
}

abstract
class C2
{
    C2(I i)
    {
	i.print ();
    }
    abstract void h();
}

public
class anon_ctor_itf_arg
{
    public static
    void main(String argv[])
    {
        C1 c1 = new C1();
        new C2(c1)
        {
            void h()
            {
            }
        };
    }
}

public class pr21540
{
    public static final long xxx = 555;
    
    public boolean fn (int v)
    {
	switch (v)
	    {
	    case ((int) xxx >>> 32):
		return true;
	    default:
		return false;
	    }
    }
}

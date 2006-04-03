// Test floating-point to integer conversion.  We do this twice, once
// with literal conversions that can be optimized away and once using
// a static field that can't.

public class Float_2
{
  public static double zero = 0.0;

  public static void main (String argv[])
  {
    {
      int itest = (int)(float)(0.0/0.0);
      if (itest != 0)
	System.err.println ("literal inf error 1: " + itest);	
    }
    {
      int itest = (int)(0.0/0.0);
      if (itest != 0)
	System.err.println ("literal inf error 2" + itest);	
    }
    {
      long ltest = (long)(0.0/0.0);
      if (ltest != 0)
	System.err.println ("literal inf error 3" + ltest);	
    }
    {
      long ltest = (long)(float)(0.0/0.0);
      if (ltest != 0)
	System.err.println ("literal inf error 4" + ltest);	
    }
      
    {
      int itest = (int)(float)(1.0/0.0);
      if (itest != Integer.MAX_VALUE)
	System.err.println ("literal max error 1: " + itest);	
    }
    {
      int itest = (int)(1.0/0.0);
      if (itest != Integer.MAX_VALUE)
	System.err.println ("literal max error 2" + itest);	
    }
    {
      long ltest = (long)(1.0/0.0);
      if (ltest != Long.MAX_VALUE)
	System.err.println ("literal max error 3" + ltest);	
    }
    {
      long ltest = (long)(float)(1.0/0.0);
      if (ltest != Long.MAX_VALUE)
	System.err.println ("literal max error 4" + ltest);	
    }
      
    {
      int itest = (int)(float)(-1.0/0.0);
      if (itest != Integer.MIN_VALUE)
	System.err.println ("literal min error 1: " + itest);	
    }
    {
      int itest = (int)(-1.0/0.0);
      if (itest != Integer.MIN_VALUE)
	System.err.println ("literal min error 2" + itest);	
    }
    {
      long ltest = (long)(-1.0/0.0);
      if (ltest != Long.MIN_VALUE)
	System.err.println ("literal min error 3" + ltest);	
    }
    {
      long ltest = (long)(float)(-1.0/0.0);
      if (ltest != Long.MIN_VALUE)
	System.err.println ("literal min error 4" + ltest);	
    }
      
    {
      int itest = (int)(float)(zero/zero);
      if (itest != 0)
	System.err.println ("calc inf error 1: " + itest);	
    }
    {
      int itest = (int)(zero/zero);
      if (itest != 0)
	System.err.println ("calc inf error 2" + itest);	
    }
    {
      long ltest = (long)(zero/zero);
      if (ltest != 0)
	System.err.println ("calc inf error 3" + ltest);	
    }
    {
      long ltest = (long)(float)(zero/zero);
      if (ltest != 0)
	System.err.println ("calc inf error 4" + ltest);	
    }
      
    {
      int itest = (int)(float)(1.0/zero);
      if (itest != Integer.MAX_VALUE)
	System.err.println ("calc max error 1: " + itest);	
    }
    {
      int itest = (int)(1.0/zero);
      if (itest != Integer.MAX_VALUE)
	System.err.println ("calc max error 2" + itest);	
    }
    {
      long ltest = (long)(1.0/zero);
      if (ltest != Long.MAX_VALUE)
	System.err.println ("calc max error 3" + ltest);	
    }
    {
      long ltest = (long)(float)(1.0/zero);
      if (ltest != Long.MAX_VALUE)
	System.err.println ("calc max error 4" + ltest);	
    }
      
    {
      int itest = (int)(float)(-1.0/zero);
      if (itest != Integer.MIN_VALUE)
	System.err.println ("calc min error 1: " + itest);	
    }
    {
      int itest = (int)(-1.0/zero);
      if (itest != Integer.MIN_VALUE)
	System.err.println ("calc min error 2" + itest);	
    }
    {
      long ltest = (long)(-1.0/zero);
      if (ltest != Long.MIN_VALUE)
	System.err.println ("calc min error 3" + ltest);	
    }
    {
      long ltest = (long)(float)(-1.0/zero);
      if (ltest != Long.MIN_VALUE)
	System.err.println ("calc min error 4" + ltest);	
    }
      
  }
}

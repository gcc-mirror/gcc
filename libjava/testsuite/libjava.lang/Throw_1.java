// Some tests of `throw'.

public class Throw_1
{
  public static Throwable get ()
    {
      return null;
    }

  public static void main (String[] args)
    {
      Throwable t = get ();
      try
	{
	  throw t;
	}
      catch (NullPointerException y)
	{
	}
      catch (Throwable x)
	{
	  System.out.println ("no");
	}
    }
}

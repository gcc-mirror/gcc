// Test of failing method invocation.

public class Invoke_1
{
  public void call_me ()
    {
      System.out.println ("no");
    }

  public static Invoke_1 get_i ()
    {
      return null;
    }

  public static void main (String[] args)
    {
      Invoke_1 i = get_i ();
      try
	{
	  i.call_me ();
	}
      catch (NullPointerException ok)
	{
	  System.out.println ("ok");
	}
    }
}

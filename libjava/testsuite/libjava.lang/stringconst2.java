// Test to make sure a string constant is correctly initialized.

import java.lang.reflect.*;

public class stringconst2
{
  public static final String q = "zardoz";

  public static void main (String[] args)
  {
    try
      {
	Class k = Class.forName ("stringconst2");
	Field f = k.getField ("q");
	System.out.println (f.get (null));
      }
    catch (Throwable t)
      {
	t.printStackTrace();
      }
  }
}

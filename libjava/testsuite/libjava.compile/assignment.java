// A definite assignment test.

public class assignment
{
  public static Byte foo ()
    {
      Byte b;
	
      while (true) {
	try {
	  b = Byte.decode ("42");
	  break;
	} catch (NumberFormatException ignored) {}
      }

      return b;
    }
}


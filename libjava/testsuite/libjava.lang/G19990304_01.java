public class G19990304_01
{
  public static void main (String[] args)
  {
    Object[] arrayObj = new String[3];
    String[] arrayStr = new String[3];
    try
      {
	System.out.println ("pass 1");
	arrayObj[0] = arrayStr;
      }
    catch (RuntimeException e)
      {
	System.out.println ("RuntimeException");
      }
    System.out.println ("pass 2");
  }
}

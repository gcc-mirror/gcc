public class G19990310_01
{
  public static void main (String[] args)
  {
    int i = 0;
    try
      {
	System.out.println ("pass 1");
	i++;
      }
    finally
      {
	System.out.println ("pass 2");
	i++;
      }
    if (i == 2)
      System.out.println ("OK");
    else
      System.out.println ("NG i = " + i);
  }
}

public class G19990303_01
{
  public static void main (String[] args)
  {
    try
      {
	Object[] ar = new String[3];
	String[] as = new String[3];
	System.out.println("1");
	ar[0] = as;
	System.out.println("2");
      }
    catch (Exception _)
      {
	System.out.println("X");
      }
    System.out.println("3");
  }
}

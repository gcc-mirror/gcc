/* Test to make sure <clinit> is generated correctly.  */

public class PR5057
{
  public static int x;

  static
  {
    x = 72;
  }

  public static void main (String[] args)
  {
    System.out.println (x);
  }
}

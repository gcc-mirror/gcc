/* Test to make sure <clinit> is generated correctly.  */

class R
{
  public static int z = 23;
}

public class PR5057_2 extends R
{
  static
  {
    R.z = 72;
  }

  public static void main (String[] args)
  {
    System.out.println (R.z);
  }
}

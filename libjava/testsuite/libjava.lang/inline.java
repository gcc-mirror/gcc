public class inline
{
  static int factorial_1 (int n)
  {
    if (n > 0)
      return n * factorial_1(n-1);
    return 1;
  }

  static int factorial_2 (int n)
  {
    if (n > 0)
      return n * factorial_3(n-1);
    return 1;
  }

  static int factorial_3 (int n)
  {
    if (n > 0)
      return n * factorial_2(n-1);
    return 1;
  }

  public static void main(String args[])
    {
      if (factorial_1 (5) != 120)
	System.out.println("This should not happen");
      else
	System.out.println("OK");
      if (factorial_2 (5) != 120)
	System.out.println("This should not happen");
      else
	System.out.println("OK");
    }
}

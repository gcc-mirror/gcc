class Overflow
{
  static int test(int x)
  {
    return (2*x)/2;
  }

  public static void main(String argv[])
  {
    int x = Integer.MAX_VALUE;

    if (test(x) == x)
      throw new RuntimeException ();
  }
}


public class pr13107_2
{
  public static int foo (boolean b)
  {
    int i;
    try {
	if (b) return 1;
	i= 2;
      }
    finally {
      if (b) i = 3;
    }
    return i;
  }

  public static void main(String[] args)
  {
  }
}

// Test for an array assignment bug we've had.

public class assign
{
  public static class base
  {
  }

  public static class derived extends base
  {
  }

  public static void main(String[] args)
  {
    base[][] x1 = new base[3][3];
    derived[] x2 = new derived[3];
    x1[0] = x2;
  }
}

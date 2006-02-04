public class pr25676
{
  public static double g(double a, double b)
  {
    return Math.min(a, b);
  }
  public static void main(String a[])
  {
    System.out.println (g(0.0, -0.0));
    System.out.println (g(-0.0, 0.0));
  }
}

public class PR7482
{
  private interface I { }
  private static class B { }
  private static class U extends B implements I { }
  private static class V extends B implements I { }

  static I field;

  private static void g1(Object o)
  {
    I val;
    if (o == null)
      val = new U();
    else
      val = new V();
    field = val;
  }

  private static I g2(Object o)
  {
    I val;
    if (o == null)
      val = new U();
    else
      val = new V();
    return val;
  }

  public static void main(String[] args)
  {
    g1(null);
    g2(null);
  }
}

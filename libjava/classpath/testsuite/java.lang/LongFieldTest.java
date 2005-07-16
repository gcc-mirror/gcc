public class LongFieldTest
{
  static long field;

  public static void main(String args[])
  {
    field = 1L;

    if (field == 1)
      System.out.println("PASSED: field = " + field);
  }
}

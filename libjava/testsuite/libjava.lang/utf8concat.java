public class utf8concat
{
  private static String s;

  public static void main (String[] args)
  {
    // This causes a crash at runtime because the compiler is
    // producing an invalid UTF-8 string literal.
    s = "abc" + (char)183;
  }
}

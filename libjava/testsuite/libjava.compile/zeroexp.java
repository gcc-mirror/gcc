public class zeroexp
{
  public static void main (String[] argv)
  {
    // gcj used to give an error about this literal.
    float f = 0E-6F;
    double d = 0E-9;
    System.out.println ("" + f + " " + d);
  }
}

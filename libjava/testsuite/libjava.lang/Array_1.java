// Test of array stuff.  Technically this probably isn't in java.lang.

public class Array_1
{
  public static void main (String[] args)
  {
    int x[][] = { { 1, 2}, null };

    System.out.println(Cloneable.class.isInstance(x));

    // This example is from the Java Spec book.
    int y[][] = (int[][]) x.clone();
    System.out.println(x == y);
    System.out.println(x[0] == y[0] && x[1] == y[1]);

    System.out.println(x.getClass().getSuperclass());
  }
}

class C {}
class D extends C implements I {}
interface I {}
interface J extends I {}

public class Class_1
{
  static void printIsAssignableFrom(Class a, Class b, boolean c)
    {
      // The field 'c' can be viewed to see the expected value.
      System.out.println(a.isAssignableFrom(b));
    }

  public static void main (String arg[])
  {
    System.out.println("Testing class `Class_1'...");
    printIsAssignableFrom(C.class, Object.class, false);
    printIsAssignableFrom(C.class, C.class, true);
    printIsAssignableFrom(C.class, D.class, true);
    printIsAssignableFrom(D.class, C.class, false);

    printIsAssignableFrom(Object.class, int[].class, true);
    printIsAssignableFrom(int[].class, int[].class, true);
    printIsAssignableFrom(C[].class, D.class, false);
    printIsAssignableFrom(C[].class, D[].class, true);
    printIsAssignableFrom(C[].class, C[][].class, false);

    printIsAssignableFrom(Object.class, I.class, true);
    printIsAssignableFrom(I.class, I.class, true);
    printIsAssignableFrom(D.class, I.class, false);
    printIsAssignableFrom(I.class, D.class, true);

    printIsAssignableFrom(D.class, J.class, false);
    printIsAssignableFrom(J.class, D.class, false);

    printIsAssignableFrom(I.class, J.class, true);
    printIsAssignableFrom(J.class, J.class, true);
    printIsAssignableFrom(J.class, I.class, false);

    // Returns true iff both Class objects are equal.
    printIsAssignableFrom(long.class, long.class, true);

    // Does not work for primitive types in general.
    printIsAssignableFrom(long.class, int.class, false);
  }
}

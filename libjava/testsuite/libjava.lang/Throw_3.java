// Check that a NPE likely thrown from the first instruction of a
// method (foo) is properly caught.
public class Throw_3
{
  public static void main(String[] args)
  {
    Throw_3 al = new Throw_3();
    try
      {
        al.foo(null);
      }
    catch (NullPointerException npe)
      {
        StackTraceElement ste[] = npe.getStackTrace();
        StackTraceElement top = ste[0];
        if ("foo".equals(top.getMethodName()))
          {
            System.out.println("ok");
            return;
          }
      }
    System.out.println("bad");
  }

  public int bar(int[] a)
  {
    System.out.println("Bar");
    return 5;
  }

  /**
   * If the second parameter ('this' being the first) is passed in a
   * register, then the first machine instruction in foo is likely to
   * fault when null is passed.
   */
  public int foo(int[] a)
  {
    int l = a.length;
    return l + l;
  }
}

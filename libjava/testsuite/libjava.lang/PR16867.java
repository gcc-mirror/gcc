/* SSA-DCE was removing the initialization of the temporary object
   in getFoo because it wasn't realizing that the pointer was needed
   outside of it.  */

public class PR16867
{
  public static Object[] getFoo()
  {
    return new Object[] {"OK"};
  }

  public static void main(String[] args)
  {
    Object[] a = getFoo();
    System.out.println(a[0]);
  }
}

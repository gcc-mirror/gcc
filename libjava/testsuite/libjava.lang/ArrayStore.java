public class ArrayStore
{
  public static void main(String[] args)
  {
    ArrayStore s = new ArrayStore();

    /* Check that bounds check takes precedence over array store check. */
    try 
    {
      s.a(new String[1]);
    }
    catch (Exception x)
    {
      System.out.println (x.getClass().getName());
    }

    try 
    {
      s.a(new String[2]);
    }
    catch (Exception x)
    {
      System.out.println (x.getClass().getName());
    }

    /* Check that += operator on String[] element works and throws bounds 
       exception. */
    try 
    {
      s.b(new String[1]);
    }
    catch (Exception x)
    {
      System.out.println (x.getClass().getName());
    }

    String[] sb = new String[2];
    sb[1] = "foo";
    s.b(sb);
    System.out.println (sb[1]);
  }

  void a(Object[] oa)
  {
    oa[1] = new Integer(2);
  }

  void b(String[] sa)
  {
    sa[1] += "bar";
  }
}

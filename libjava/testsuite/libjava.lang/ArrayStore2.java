public class ArrayStore2
{
  public static void main(String[] args)
  {
    new ArrayStore2().a(new Object[2], 3);
  }

  void a(Object[] oa, int i)
  {
    try
    {
      oa[index()] = obj();
    }
    catch (Exception x)
    {
      System.out.println (x.getClass().getName());
    }
  }

  int index()
  {
    System.out.println ("index");
    return 3;
  }

  Object obj()
  {
    System.out.println ("rhs");
    return new Object();
  }
}

public class pr184
{
  public static void main(String[] args)
  {
    pr184 n = null;
    try
    {
      n.foo();
    }
    catch (NullPointerException x)
    {
      System.out.println(x);
    }
  }
  
  int x = 2;
  
  final int foo() 
  {
    return x;
  };
}

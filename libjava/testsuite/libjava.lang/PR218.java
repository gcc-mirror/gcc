// Bug in gcj 20000427: Java executables can abort trying to access a null 
// pointer in a leaf function.

public class PR218
{
  private int i = 5;
  
  public static void main(String[] args)
  {
    try
    {
      new PR218().foo(null);
    }
    catch (NullPointerException x)
    {
      System.out.println(x);
    }
  }
  
  void foo(PR218 e)
  {
    e.i += 4;
  };
}

// Expected output:
//
// java.lang.NullPointerException

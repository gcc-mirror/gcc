/* Check for incorrectly aligned byte args.  */

public class err14
{
  protected final String getClearToolCommand(Object a, Object b,
                                             Object c, Object d, 
                                             int e, int f, boolean x) 
  {
    return x ? "hi" : "byte";
  }
  
  
  public static void main(String[] args)
  {
    System.out.println(new err14().getClearToolCommand(null, null,
                                                       null, null, 0, 0, false));
    System.out.println(new err14().getClearToolCommand(null, null,
                                                       null, null, 0, 0, true));
  }
}

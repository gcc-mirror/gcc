public class pr26990
{
  public static void main (String args[]) throws Exception
  {
    System.setSecurityManager(new SecurityManager()
                              {
                                public void checkExit(int status)
                                {
                                  throw new SecurityException("This is a bug");
                                }
                              });
  }
}

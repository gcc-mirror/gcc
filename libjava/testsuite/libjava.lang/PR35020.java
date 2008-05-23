public class PR35020
{
   class inner
   {
   }
   public static void main(String[] args)
   {
      System.out.println(inner.class.getSimpleName());
      System.out.println(PR35020.class.getSimpleName());
      System.out.println(Class.class.getSimpleName());
      System.out.println((new int[7]).getClass().getSimpleName());
      System.out.println((new Object[1][1][1][1][1][1][1][1]).getClass().getSimpleName());
      System.out.println((new java.security.PrivilegedAction() 
	{
	  public Object run() {
	    return null;
	  }
	}).getClass().getSimpleName());
   }
}


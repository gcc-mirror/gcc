class IsInstanceTest extends Thread implements Cloneable
{
  static void main(String args[])
    {
      IsInstanceTest test = new IsInstanceTest();

      if (test instanceof java.lang.Object)
	pass("IsInstanceTest is instance of java.lang.Object");
      else
	fail("IsInstanceTest is not instance of java.lang.Object");

      if (test instanceof java.lang.Cloneable)
	pass("IsInstanceTest is instance of java.lang.Cloneable");
      else
	fail("IsInstanceTest is not instance of java.lang.Cloneable");

      if (test instanceof java.lang.Runnable)
	pass("IsInstanceTest is instance of java.lang.Runnable");
      else
	fail("IsInstanceTest is not instance of java.lang.Runnable");


    }
  static void pass(String message)
    {
      System.out.println("PASSED: "+message);
    }
  static void fail(String message)
    {
      System.out.println("FAILED: "+message);
    }
}

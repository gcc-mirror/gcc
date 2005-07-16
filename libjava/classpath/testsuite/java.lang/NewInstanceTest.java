public class NewInstanceTest
{
  public NewInstanceTest() {
    static_field = 1;
  }

  public static void main(String args[]) {
    try {
      Class cls = Class.forName("NewInstanceTest");
      Object instance = cls.newInstance();
      
      if (static_field == 1)
	System.out.println("PASSED: static_field = " + static_field);
      else
	System.out.println("FAILED: static_field = " + static_field);
    }
    catch (Exception e)
      {
	System.out.println("FAILED: exception " + e.toString());
      }
  }

  public static int static_field;
}

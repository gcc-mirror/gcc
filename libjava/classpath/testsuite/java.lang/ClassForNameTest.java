public class ClassForNameTest
{
  public static void main(String args[]) {
    Class c;
    /* test for both success and failure */

    try {
      c = Class.forName("ClassForNameTest");
    }
    catch (Exception e) {
      System.out.println("FAILED: Couldn't find ClassForNameTest.");
      System.exit(0);
    }

    try {
      c = Class.forName("ClazzForNameT3st");
    }
    catch (Exception e) {
      System.out.println("PASSED: passed both success and failure cases for Class.forName");
      System.exit(0);
    }

    System.out.println("FAILED: Didn't raise exception for incorrect class name.");
  }
}

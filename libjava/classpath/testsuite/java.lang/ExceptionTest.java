public class ExceptionTest 
{
  static int foo() throws ArrayIndexOutOfBoundsException {
    int f[] = new int[10];

    return f[26]; 
  }

  public static void main (String args[]) {
    int f;

    try {
      f = foo();
    }
    catch (ArrayIndexOutOfBoundsException e) {
      System.out.println("PASSED: " + e.toString());
    } catch (Exception e) {
      System.out.println("FAILED: " + e.toString());
    }
  }
}

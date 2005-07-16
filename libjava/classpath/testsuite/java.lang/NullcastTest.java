import java.lang.*;

public class NullcastTest
{
  static String retString(String str1, String str2)
    {
      return str1;
    }
  public static void main (String args[]) {
    try {

      String tmp = retString((String) null, (String)null);

      System.out.println("PASSED: (String)null");
      System.exit(0);
    } catch (Exception e) {
      System.out.println("FAILED: "+e);
    }
  }
}

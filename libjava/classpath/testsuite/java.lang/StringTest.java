/*
 * Aaron M. Renn reported a bug in Japhar having string length 17 for
 * this string
 */

public class StringTest
{
  public static void
    main(String[] argv)
    {
      UnicodeStringLength();
    }
  static void UnicodeStringLength()
    {
      String str = "a-->\u01FF\uA000\u6666\u0200RRR";
      int len = str.length();
      if (11 == len) {
	System.out.println("PASSED: " + str + " has len=" +str.length());
      } else {
	System.out.println("FAILED: " + str +
			   " has len=" +str.length() + " != 11");
      }
    }
}

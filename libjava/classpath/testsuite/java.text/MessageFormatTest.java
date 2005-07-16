import java.text.*;

public class MessageFormatTest
{
  public static void main(String args[])
    {
      try {
	String[] sa = new String[3];
	String format = "{0}.{1}() {2}";

	sa[0] = "MessageFormat";
	sa[1] = "format";
	sa[2] = "worked";

	String message = MessageFormat.format(format, (Object[])sa);

	if (null == message)
	  throw new Exception("Unable to format message");

	System.out.println("PASSED: " + message);
      } catch (Exception e) {
	System.out.println("FAILED: " + e);
      }
    }
}

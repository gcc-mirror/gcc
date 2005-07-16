import java.util.*;

public class ResourceBundleTest
{
  public static void main(String args[])
    {
      try {
	ResourceBundle messageRB =
	  ResourceBundle.getBundle("sun.tools.javac.resources.javac"); 

	String bundle = (String)messageRB.getObject("main.usage");
	if (null == bundle)
	  throw new Exception("javac.main.usage resource is null");

	System.out.println("PASSED: Resource javac.main.usage existed");
      } catch (Exception e) {
	System.out.println("FAILED: " + e);
      }
    }
}

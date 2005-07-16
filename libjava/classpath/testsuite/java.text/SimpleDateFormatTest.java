import java.util.*; // for Date()
import java.text.*;

public class SimpleDateFormatTest
{
  public static void main(String args[])
    {
      try {
	SimpleDateFormat formatter
	  = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss z");
	Date current = new Date();
	System.out.println("PASSED: time="+formatter.format(current));
      } catch (Exception e) {
	System.out.println("FAILED: "+e);
      }
    }
}

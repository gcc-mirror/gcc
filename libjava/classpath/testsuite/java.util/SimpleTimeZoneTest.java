import java.util.*;

public class SimpleTimeZoneTest
{
  public static void main(String args[])
    {
      try {
	SimpleTimeZone gmt = new SimpleTimeZone(0, "GMT");
	System.out.println("PASSED: timezone="+gmt.toString());
      } catch (Exception e) {
	System.out.println("FAILED: "+e);
      }
    }
}

import java.util.Vector;

/**
 * Under JavaSoft's VM they arbitarily limit the amount of memory
 * a Java application can use (though this can be overridden).  The
 * point here is to check to see whether or not an application being
 * run by Japhar will ever get the OutOfMemoryError or not when resources
 * are scarce. --brian
 */
public class OutOfMemoryErrorTest
{
  public static void main(String[] argv)
    {
      Vector v = null;
      Runtime r = null;
      long free = 0, total = 0;
      // quickly approach memory limit 1M at a time
      try {
	r = Runtime.getRuntime();
	v = new Vector();
	while(true)
	  {
	    v.addElement(new byte[1048576]);
	  }
      }
      // out of memory error
      catch (OutOfMemoryError oomerr1)
	{
	  // slowly encroach on memory limit 2 bytes+ at a time
	  try {
	    while(true)
	      {
		v.addElement(new byte[2]);
	      }
	  }
	  // out of memory error
	  catch (OutOfMemoryError oomerr2)
	    {
	      if (r != null)
		{
		  free = r.freeMemory();
		  total = r.totalMemory();
		  v = null;
		  r.gc();
// 		  System.out.println("free = " + free);
// 		  System.out.println("total = " + total);
		  System.out.println("PASSED: ");
		}
	      else
		System.out.println("FAILED: runtime unknown");
	    }	  
	}
      // generic error
      catch (Error err)
	{
	  System.out.println("FAILED: unexpected error");
	}
      // generic exception
      catch (Exception e)
	{
	  System.out.println("FAILED: unexpected exception");
	}
    }
}

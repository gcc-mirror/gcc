// Create a running process for a non existent executable.
// Verify that IOException is thrown.
import java.io.IOException;


public class Process_6
{
  public static void main(String[] args)
  {
    try
      {
	int c;
	Runtime r = Runtime.getRuntime();
	String[] a = { "blablabla_failure" };

	try
	  {
	    Process p = r.exec(a);
	    System.out.println("bad");
	  }
	catch (IOException ioe)
	  {
	    System.out.println("ok");
	  }
      }
    catch (Exception ex)
      {
	System.out.println(ex.toString());
      }
  }
}

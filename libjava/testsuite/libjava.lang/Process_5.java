// Create a long running process and verify that the exitValue is not
// immediately available.  Then destroy() it and verify that it
// terminates quickly with a non-zero exitValue.
public class Process_5
{
  public static void main(String[] args)
  {
    try
      {
	int c;
	long startTime = System.currentTimeMillis();
	Runtime r = Runtime.getRuntime();
	String[] a = { "sleep", "120" };
	Process p = r.exec(a);

	try
	  {
	    c = p.exitValue();
	    System.out.println("bad 1");
	    return;
	  }
	catch (IllegalThreadStateException itse)
	  {
	    // Ignore as this is good here.
	  }

	p.destroy();

	c = p.waitFor();

	long endTime = System.currentTimeMillis();

	if (endTime - startTime > 110000L)
	  System.out.println("bad 2");

	System.out.println(c != 0 ? "ok" : "bad 3");
      }
    catch (Exception ex)
      {
	System.out.println(ex.toString());
      }
  }
}

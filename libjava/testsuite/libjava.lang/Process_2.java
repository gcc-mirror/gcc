// Create a process and read from its standard error.
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;


public class Process_2
{
  public static void main(String[] args)
  {
    try
      {
	Runtime r = Runtime.getRuntime();
	String s = "Hello World";
	String[] a = { "sh", "-c", "echo " + s + " >&2" };
	Process p = r.exec(a);
	InputStream is = p.getErrorStream();
	InputStreamReader isr = new InputStreamReader(is);
	BufferedReader br = new BufferedReader(isr);
	String result = br.readLine();
	if (! s.equals(result))
	  {
	    System.out.println("bad 1");
	    return;
	  }
	result = br.readLine();
	if (result != null)
	  {
	    System.out.println("bad 2");
	    return;
	  }
	int c = p.waitFor();
	System.out.println(c == 0 ? "ok" : "bad 3");
      }
    catch (Exception ex)
      {
	System.out.println(ex.toString());
      }
  }
}

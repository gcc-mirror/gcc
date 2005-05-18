// Create a process and pipe data through it.  waitFor() the process
// in a different thread than the one that created it.
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;


public class Process_3 implements Runnable
{
  Process p;

  public void run()
  {
    try
      {
	Runtime r = Runtime.getRuntime();
	String[] a = { "sed", "-e", "s/Hello/Goodbye/" };
	synchronized (this)
	  {
	    p = r.exec(a);
	    this.notifyAll();
	  }
	OutputStream os = p.getOutputStream();
	PrintStream ps = new PrintStream(os);
	ps.println("Hello World");
	ps.close();
      }
    catch (Exception ex)
      {
	System.out.println(ex.toString());
        System.exit(1);
      }
  }

  public static void main(String[] args)
  {
    try
      {
	Process_3 p3 = new Process_3();
	Thread t = new Thread(p3);
	t.start();
	synchronized (p3)
	  {
	    while (p3.p == null)
	      p3.wait();
	  }

	InputStream is = p3.p.getInputStream();
	InputStreamReader isr = new InputStreamReader(is);
	BufferedReader br = new BufferedReader(isr);
	String result = br.readLine();
	if (! "Goodbye World".equals(result))
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
	int c = p3.p.waitFor();
	System.out.println(c == 0 ? "ok" : "bad 3");
      }
    catch (Exception ex)
      {
	System.out.println(ex.toString());
      }
  }
}

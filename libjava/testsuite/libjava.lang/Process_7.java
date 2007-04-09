// Verify we can modify the environment.
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;



public class Process_7
{
  public static void main(String[] args)
  {
    try
      {
        ProcessBuilder pb = new ProcessBuilder("env");
        Map<String, String> e = pb.environment();
        e.clear();
        String v = "process7_value";
        String k = "PROCESS_7_KEY";
        e.put(k, v);
	Process p = pb.start();
	InputStream is = p.getInputStream();
	InputStreamReader isr = new InputStreamReader(is);
	BufferedReader br = new BufferedReader(isr);
        boolean found = false;

	String result;
        while ((result = br.readLine()) != null)
          {
            if (result.equals(k + '=' + v))
              found = true;
          }
	if (!found)
	  {
	    System.out.println("bad");
	    return;
	  }
	System.out.println("ok");
      }
    catch (Exception ex)
      {
	System.out.println(ex.toString());
      }
  }
}

// Create a process and verify failure exit code.
public class Process_4
{
  public static void main(String[] args)
  {
    try
      {
	Runtime r = Runtime.getRuntime();
	String[] a = { "false" };
	Process p = r.exec(a);
	int c = p.waitFor();
	System.out.println(c == 1 ? "ok" : "bad");
      }
    catch (Exception ex)
      {
	System.out.println(ex.toString());
      }
  }
}

import java.io.*;

public class IsAbsoluteTest {
  public static void main (String args[]) {
    try {
      File f1 = new File("/etc/passwd");
      File f2 = new File("\\autoexec.bat");
      File f3 = new File("c:\\autoexec.bat");

      File u1 = new File("tmp/somefile");

      if ( u1.isAbsolute() )
	    throw new Exception("Claims "+u1+" is absolute!");

      if ( ! f1.isAbsolute() )
	{ /* Hm, might be on MSDOS platform, test those cases */
	  if ( ! f2.isAbsolute() || ! f3.isAbsolute() )
	    throw new Exception("Claims file isn't absolute!");
	}
      
      System.out.println("PASSED: All ok");
    } catch (Exception e) {
      System.out.println("FAILED: "+e);
    }
  }
}

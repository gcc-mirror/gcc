import java.lang.*;

public class JavacTest {
 
  public static void main (String args[]) {
    try {


      sun.tools.javac.Main javac = new sun.tools.javac.Main(System.err, "javac");

      String[] strarr = new String[1];
      strarr[0] = "java.sun.tools/JavacTest.java";

      javac.compile(strarr);

      System.out.println("PASSED: javac worked");
      System.exit(0);
    } catch (Exception e) {
      System.out.println("FAILED: "+e);
    }
  }
}

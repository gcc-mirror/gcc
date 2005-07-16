import sun.tools.java.*;

public class ClassPathTest {
  public static void main(String args[]) {
    ClassPath cp =
      new ClassPath((String)System.getProperties().get("java.class.path"));
    ClassFile cf = cp.getFile("ClassPathTest.class");
    try {
      System.out.println("PASSED: "+cp.toString() +" "+ cf.toString());
    } catch (Exception e) {
      System.out.println("FAILED: " + e.toString());
    }      
  }
}

// Test case from Martin Kahlert <martin.kahlert@infineon.com>

public class martin {
  public native void myNative(String s);

  public void myJava(String s) {
    s = s + ", Java";
    System.out.println(s);
  }

  public static void main(String args[]) {
    martin x = new martin();
    x.myJava("Hello");
    x.myNative("Hello, Java (from C)");
    x.myJava("Goodbye");
  }
  
  static {
    System.loadLibrary("martin");
  }
}

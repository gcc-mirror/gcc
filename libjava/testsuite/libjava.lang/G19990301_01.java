public class G19990301_01 {
  public static void main(String args[]) {
    foo pd = new foo();
    System.out.println ("Pass 1");
    pd.s = "test";
    System.out.println ("Pass 2");
  }
}
class foo {
  static String s = "test";
}


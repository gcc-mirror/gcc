// Demonstrate that private methods can be reflected even if they are
// not referenced at compile-time (i.e. -fkeep-inline-functions works).
import java.lang.reflect.Method;
public class KeepInline {
  private void example() {
    System.out.println("example");
  }
  public static void main(String[] args) {
    try {
      KeepInline pr = new KeepInline();
      Method[] meths = pr.getClass().getDeclaredMethods();
      for (int n = 0; n < meths.length; n++)
        System.out.println(meths[n]);
    } catch (Throwable t) {
      t.printStackTrace();
    }
  }
}

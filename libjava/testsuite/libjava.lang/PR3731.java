// Check whether "instanceof" initializes its class argument.

public class PR3731 {
  static B b;
  public static void main(String[] args) {
    System.out.println(b instanceof B);
  }
}

class B {
  static {
    System.out.println("Initialized");
  }
}

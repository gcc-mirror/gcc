public class pr24321 {
  static class Z {
    static {
      System.out.println("init");
    }
  }

  static class Y extends Z { }

  public static Object x () { return new Object(); }

  public static void main(String[] args) throws Throwable
  {
    System.out.println(x() instanceof Z);

    ClassLoader cl = pr24321.class.getClassLoader();
    Class zk = Class.forName("pr24321$Z", false, cl);
    Class yk = Class.forName("pr24321$Y", false, cl);
    System.out.println(zk.isAssignableFrom(yk));
  }
}

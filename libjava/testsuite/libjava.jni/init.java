// Regression test for JNI and static initializers.

public class init
{
  public static class NativeClass
  {
    static
    {
      System.out.println("static initializer 2");
      System.loadLibrary("init"); // if it's here, this app doesn't work
    }

    public static native void printHello();
  }

  static
  {
    System.out.println("static initializer 1");
  }

  public static void main(String[] args)
  {
    //System.loadLibrary("test"); // if it's here, this app works
    NativeClass.printHello();
  }
}

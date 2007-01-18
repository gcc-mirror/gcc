// Test that FindClass initializes the class.

public class findclass2
{
  public static class inner
  {
    static
    {
      System.out.println("hello");
    }
  }

  public static native void searchClass();

  static
  {
    System.loadLibrary("findclass2");
  }

  public static void main(String[] args)
  {
    searchClass();
  }
}

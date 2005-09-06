public class pr23739
{
  static
  {
    System.loadLibrary ("pr23739");
  }

  public static class A
  {
  }

  public static class B extends A
  {
  }

  static native void checkOrder (Class clazz1, Class clazz2);

  public static void main (String[] args)
  {
    checkOrder (A.class, B.class);
  }
}

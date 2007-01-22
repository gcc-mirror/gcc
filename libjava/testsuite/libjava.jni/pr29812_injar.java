public class pr29812_injar
{
  public class inner
  {
  }

  static {
    System.loadLibrary("pr29812_injar");
  }

  public static native void doit();
}

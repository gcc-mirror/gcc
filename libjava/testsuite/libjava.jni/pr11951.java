public class pr11951
{
  public static Object dosomething()
  {
    throw new Error();
  }

  public static native void nmethod();

  public static void main(String[] args)
  {
    nmethod();
  }
}

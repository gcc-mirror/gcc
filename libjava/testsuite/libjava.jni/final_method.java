// Minimal test a non-static final method.

public class final_method
{
  static
  {
    System.loadLibrary ("final_method");
  }

  public final native String meth ();

  public static void main (String[] args)
  {
    final_method fm = new final_method ();
    System.out.println (fm.meth ());
  }
}

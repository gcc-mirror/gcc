// Test for array field lookup.

public class cxxtest
{
  // A field for us to look up.
  public Object[] F = new Object[7];

  public native Object[] fetch ();

  public void doit ()
  {
    System.out.println (F == fetch ());
  }

  public static void main (String[] args)
  {
    cxxtest q = new cxxtest ();
    q.doit ();
  }

  static
  {
    System.loadLibrary ("cxxtest");
  }
}

// Test for array field lookup.

public class field
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
    field q = new field ();
    q.doit ();
  }

  static
  {
    System.loadLibrary ("field");
  }
}

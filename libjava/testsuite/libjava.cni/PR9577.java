// Check if a method name is mangled properly in the presence
// of an array parameter sharing a part of the type name 
// with a subsequent parameter.

public class PR9577
{
  private native void sayHello (String[] s, Object o);

  public static void main (String[] args)
  {
    PR9577 x = new PR9577( );
    x.sayHello( null, null);
  }
}

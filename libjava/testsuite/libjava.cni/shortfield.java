class shortfieldbase
{
   short modCount;
}

public class shortfield extends shortfieldbase
{
  short size__;
  int data;
  
  native void ouch ();
  
  public static void main (String[] s)
  {
    shortfield f = new shortfield();
    f.modCount = 99;
    f.size__ = 2;
    f.data = 0x12345678;
    f.ouch();
  }
}

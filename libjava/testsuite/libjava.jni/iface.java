// JNI calls via an interface method were broken in a couple releases.

interface mycomp
{
  int compareTo(Object x);
}

public class iface implements mycomp
{
  static
  {
    System.loadLibrary("iface");
  }

  public int compareTo (Object x)
  {
    System.out.println ("hi maude");
    return 3;
  }

  public native void doCalls(Object x);

  public static void main (String[] args)
  {
    new iface().doCalls(args);
  }
}

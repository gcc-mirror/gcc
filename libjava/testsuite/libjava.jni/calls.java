// Test a bunch of different calls.

class base
{
  public int int_f ()
  {
    return 27;
  }
}

public class calls extends base
{
  static
  {
    System.loadLibrary ("calls");
  }

  public native int docall ();

  public byte byte_f ()
  {
    return 23;
  }

  public char char_f (int z)
  {
    return (char) ('a' + z);
  }

  public int int_f ()
  {
    return 1023;
  }

  public static long long_f (long q)
  {
    return q + 2023;
  }

  public static long longpb_f (byte b1, long q1, byte b2, long q2,
			       byte b3, long q3)
  {
    return q1 + q2 + q3 + 3023;
  }

  public void void_f ()
  {
    System.out.println ("void");
  }

  public static short short_f ()
  {
    return 2;
  }

  public double double_f ()
  {
    return -1.0;
  }

  public float float_f ()
  {
    return (float) 1.0;
  }

  public static void main (String[] args)
  {
    calls c = new calls ();
    if (c.docall () != 0)
      System.out.println ("fail");
  }
}

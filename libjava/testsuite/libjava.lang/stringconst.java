// A reference to a String shouldn't cause an interface to be
// initialized.

interface I
{
  String z = "zardoz";
  int q = stringconst.out ("q", 0);
}

public class stringconst
{
  public static int out (String s, int i)
  {
    System.out.println (s + "=" + i);
    return i;
  }

  public static void main (String[] args)
  {
    System.out.println (I.z);
  }
}

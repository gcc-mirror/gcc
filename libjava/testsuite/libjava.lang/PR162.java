interface I
{
  int i = 1, ii = Test.out ("ii", 2);
}

interface J extends I
{
  int j = Test.out ("j", 3), jj = Test.out ("jj", 4);
}

interface K extends J
{
  int k = Test.out ("k", 5);
}

public class PR162
{
  public static void main (String[] args)
  {
    System.out.println (J.i);
    System.out.println (K.j);
  }

  public static int out (String s, int i)
  {
    System.out.println (s + "=" + i);
    return i;
  }
}

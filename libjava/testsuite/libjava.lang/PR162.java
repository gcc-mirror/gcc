interface I
{
  int i = 1, ii = PR162.out ("ii", 2);
}

interface J extends I
{
  int j = PR162.out ("j", 3), jj = PR162.out ("jj", 4);
}

interface K extends J
{
  int k = PR162.out ("k", 5);
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

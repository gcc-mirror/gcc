class pr21844base
{
  int modCount;

  public pr21844base(int x)
  {
    modCount = x;
  }
}

public class pr21844 extends pr21844base
{
  class inner
  {
    public int doit ()
    {
      ++modCount;
      return modCount;
    }
  }

  public pr21844(int x)
  {
    super(x);
  }

  public static void main(String[] args)
  {
    pr21844 val = new pr21844(7);
    inner i = val.new inner();
    System.out.println(i.doit());
  }
}

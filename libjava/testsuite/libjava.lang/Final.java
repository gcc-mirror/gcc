public final class Final
{
  public static void main(String args[])
    {
      Final f = null;
      try
        {
          f.doSomething();
        }
      catch (NullPointerException x)
        {
          System.out.println("NullPointerException - ok");
        }
    }

  void doSomething()
    {
      System.out.println("This should not happen");
    }
}

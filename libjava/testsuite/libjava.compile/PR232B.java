// This triggers a failure when compiling from bytecode (only) with 20000519

public class PR232B
{
  private static Object lock = new Object();
  private static PR232B instance = null;

  public void a()
  {   
    synchronized(lock)
    {
      instance = new PR232B();
    } 
  }
};

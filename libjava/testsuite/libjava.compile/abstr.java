// This fails to compile from bytecode for some versions of the compiler.

interface foo
{
  public void start ();
}

public abstract class abstr implements foo
{
  public void doit ()
  {
    start ();
  }
}

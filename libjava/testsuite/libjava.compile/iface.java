// Test for searching through interface extension.

interface basei
{
  public int method ();
}

interface basei2
{
  public int confuse ();
}

interface derivedi extends basei, basei2
{
  public void nothing ();
}

public class iface
{
  public int try_it (derivedi x)
  {
    return x.method ();
  }
}

// Test for a verification regression.

interface I { }
class D implements I { }
class E extends D { }

public class verify
{
  static void call(I v) { }

  static void doit (Object x)
  {
    call ((x instanceof I) ? (I) x : new E ());
  }

  public static void main(String[] args)
  {
    doit(null);
  }
}

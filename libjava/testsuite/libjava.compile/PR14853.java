class tt
{
  static final tt tt1 = new tt();
  tt()
  {
  }
}

public class PR14853
{
  public static void main (String[] args)
  {
    // This is an invalid assignment.  gcj would get confused in
    // definite assignment when compiling to object code.
    tt.tt1 = new tt();
  }
}

public class longfield
{
  long lval = 232300;
  boolean bval = true;
  String sval = "maude";

  public native void doitc ();

  public void doitj()
  {
    System.out.println(lval);
    System.out.println(bval);
    System.out.println(sval);
  }

  public static void main(String[] args)
  {
    longfield f = new longfield();
    f.doitc();
    f.doitj();
  }
}

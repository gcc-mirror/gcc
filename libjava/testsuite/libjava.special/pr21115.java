public abstract class pr21115
{
  public static void main(String[] args) throws Exception
  {
    Class c = Class.forName("pr21115I");
    Object o = c.newInstance();
    pr21115 t = (pr21115) o;
    // Try to fill the stack with 0xff.
    t.test (0xffffffff, 0xffffffff,
	    0xffffffff, 0xffffffff,
	    0xffffffff, 0xffffffff,
	    0xffffffff, 0xffffffff,
	    0xffffffff, 0xffffffff);
    // Pass a bunch of false (0) values.
    if (t.test (false, false,
		false, false,
		false, false,
		false, false,
		false, false))
      System.out.println ("FAILED - expecting false return value.");
  }

  public abstract boolean test(boolean a, boolean b,
			       boolean c, boolean d,
			       boolean e, boolean f,
			       boolean g, boolean h,
			       boolean i, boolean j);
  
  public abstract boolean test(int a, int b,
			       int c, int d,
			       int e, int f,
			       int g, int h,
			       int i, int j);
}

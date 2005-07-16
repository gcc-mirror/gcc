
public class ArrayTest {
  public static void main (String args[])
  {
    BooleanArrayInit();
    ByteArrayInit();
    CharArrayInit();
    ShortArrayInit();
    IntArrayInit();
    ArrayName(args);
  }
  public static void BooleanArrayInit()
    {
      try {
	boolean val = true;
	boolean [] x = { true };
	if (x[0] == val)
	  passed("BooleanArrayInit() boolean[] x = {"+val+"}");
	else
	  failed("BooleanArrayInit() boolean[] x = {"+val+"}");
      } catch (Exception e) {
	failed("BooleanArrayInit() "+e);
      }
    }
  public static void ByteArrayInit()
    {
      try {
	byte val = 42;
	byte [] x = { 42 };
	if (x[0] == val)
	  passed("ByteArrayInit() byte[] x = {"+val+"}");
	else
	  failed("ByteArrayInit() byte[] x = {"+val+"}");
      } catch (Exception e) {
	failed("ByteArrayInit() "+e);
      }
    }
  public static void CharArrayInit()
    {
      try {
	char val = 'X';
	char [] x = { 'X' };
	if (x[0] == val)
	  passed("CharArrayInit() char[] x = {'"+val+"'}");
	else
	  failed("CharArrayInit() char[] x = {'"+val+"'}");
      } catch (Exception e) {
	failed("CharArrayInit() "+e);
      }
    }
  public static void ShortArrayInit()
    {
      try {
	short val = 42;
	short [] x = { 42 };
	if (x[0] == val)
	  passed("ShortArrayInit() short[] x = {"+val+"}");
	else
	  failed("ShortArrayInit() short[] x = {"+val+"}");
      } catch (Exception e) {
	failed("ShortArrayInit() "+e);
      }
    }
  public static void IntArrayInit()
    {
      try {
	int val = 42;
	int [] x = { 42 };
	if (x[0] == val)
	  passed("IntArrayInit() int[] x = {"+val+"}");
	else
	  failed("IntArrayInit() int[] x = {"+val+"}");
      } catch (Exception e) {
	failed("IntArrayInit() "+e);
      }
    }
  public static void failed(String s)
    {
      if (s != null)
	System.out.println("FAILED: " + s);
      else
	System.out.println("FAILED: ");
    }
  public static void passed(String s)
    {
      if (s != null)
	System.out.println("PASSED: " + s);
      else
	System.out.println("PASSED: ");
    }
  public static void ArrayName(String args[])
    {
      try {
	String name = args.getClass().getName();
	passed("ArrayName() name="+name);
      } catch (Exception e) {
	failed("ArrayName() "+e);
      }
    }
}

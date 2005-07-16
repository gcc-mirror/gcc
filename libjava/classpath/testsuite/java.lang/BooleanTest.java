/**
 * Test the Boolean object wrapper class.
 *
 * @author Brian Jones (brian.jones@oryxsoft.com)
 */
public class BooleanTest 
{
  Boolean j;
  String x;

  public static void main (String[] argv)
    {
      BooleanTest test = new BooleanTest();
      
      test.constructorsTest();
      test.booleanValueTest();
      test.equalsTest();
      test.getBooleanTest();
      test.hashCodeTest();
      test.toStringTest();
      test.valueOfTest();
      test.variablesTest();
    }

  public void constructorsTest()
    {
      j = new Boolean(true);    // is true
      if (j.booleanValue() != true)
	failed("Boolean(true)");
      else
	passed("Boolean(true)");

      j = new Boolean(false);   // is false
      if (j.booleanValue() != false)
	failed("Boolean(false)");
      else
	passed("Boolean(false)");

      j = new Boolean("tRuE");  // is true
      if (j.booleanValue() != true)
	failed("Boolean(\"tRuE\")");
      else
	passed("Boolean(String)");

      j = new Boolean("brian"); // is false
      if (j.booleanValue() != false)
	failed("Boolean(\"brian\")");
      else
	passed("Boolean(String)");

      j = new Boolean(null);    // is false
      if (j.booleanValue() != false)
	failed("Boolean(null)");
      else
	passed("Boolean(String)");
    }

  public void booleanValueTest()
    {
      if (Boolean.TRUE.booleanValue() != true)
	failed("Boolean.booleanValue()");
      else
	passed("Boolean.booleanValue()");
    }

  public void equalsTest()
    {
      j = new Boolean("false");
      if (j.equals(Boolean.FALSE) != true)
	failed("Boolean.equals(Object)");
      else
	passed("Boolean.equals(Object)");
    }

  public void getBooleanTest()
    {
      if (Boolean.getBoolean("BIG_DAWG_TEST"))
	failed("Boolean.getBoolean(String)");
      else
	passed("Boolean.getBoolean(String)");
    }

  public void hashCodeTest()
    {
      j = new Boolean(null);    // is false
      boolean caught = false;
      try 
	{
	  int i = j.hashCode();
	}
      catch (Exception e)
	{
	  caught = true;
	  failed("Boolean.hashCode()");
	}
      if (!caught)
	passed("Boolean.hashCode()");
    }

  public void toStringTest()
    {
      j = Boolean.TRUE;
      String x = j.toString();
      if (x.equals("true") != true)
	failed("j.toString() where j is Boolean.TRUE");
      else
	passed("Boolean.toString()");

      j = Boolean.FALSE;
      x = j.toString();
      if (x.equals("false") != true)
	failed("j.toString() where j is Boolean.FALSE");
      else
	passed("Boolean.toString()");
    }

  public void valueOfTest()
    {
      j = Boolean.valueOf("tRUe"); // true
      if (j.booleanValue() != true)
	failed("Boolean.valueOf(String)");
      else
	passed("Boolean.valueOf(String)");

      j = Boolean.valueOf(null);   // false
      if (j.booleanValue() != false)
	failed("Boolean.valueOf(null)");
      else
	passed("Boolean.valueOf(null)");

      j = Boolean.valueOf("lc");   // false
      if (j.booleanValue() != false)
	failed("Boolean.valueOf(String)");
      else
	passed("Boolean.valueOf(String)");
    }

  public void variablesTest()
    {
      if (Boolean.TRUE.booleanValue() != true)
	failed("Boolean.TRUE");
      else
	passed("Boolean.TRUE");

      if (Boolean.FALSE.booleanValue() != false)
	failed("Boolean.FALSE");
      else
	passed("Boolean.FALSE");

      x = Boolean.TYPE.getName();
      if (x.equals("boolean") != true)
	failed("Boolean.TYPE.getName() is " + x + " != boolean");
      else
	passed("Boolean.TYPE.getName() is boolean");
    }

  public void failed(String s)
    {
      if (s != null)
	System.out.println("FAILED: " + s);
      else
	System.out.println("FAILED: ");
    }

  public void passed(String s)
    {
      if (s != null)
	System.out.println("PASSED: " + s);
      else
	System.out.println("PASSED: ");
    }
}






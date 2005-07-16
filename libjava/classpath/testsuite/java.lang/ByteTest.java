import gnu.test.*;

/**
 * Test the Byte object wrapper class.
 *
 * @author Brian Jones (cbj@gnu.org)
 */
public class ByteTest
{
  public static class constructorTest1 implements Test
  {
    byte b = 1;
    
    public String getName() {
      return "Byte(byte)";
    }
    
    public Result test() {
      try {
	Byte byteObject = new Byte(b);
      } catch (Exception e) {
	return new Fail(e.getMessage());
      } catch (Error err) {
	return new Fail(err.getMessage());
      }
      return new Pass();
    }
  }
  
  public static class constructorTest2 implements Test
  {
    Byte byteObject = null;
    
    public String getName() {
      return "Byte(String)";
    }
    
    public Result test() {
      try {
	byteObject = new Byte("1");
      } catch (Exception e) {
	return new Fail(e.getMessage());
      } catch (Error err) {
	return new Fail(err.getMessage());
      }
      return new Pass();
    }
  }
  
  public static class byteValueTest implements Test
  {
    public String getName() {
      return "Byte.byteValue()";
    }
    
    public Result test() {
      byte b = 1;
      Byte byteObject = new Byte(b);
      if (byteObject.byteValue() == b)
	return new Pass();
      else
	return new Fail();
    }
  }

  public static class decodeTest implements Test
  {
    public String getName() {
      return "Byte.decode(String)";
    }
    
    public Result test() {
      Byte obj = Byte.decode("1");
      if (obj.byteValue() == 1)
	return new Pass();
      else
	return new Fail();
    }
  }
    
  public static class doubleValueTest implements Test
  {
    public String getName() {
      return "Byte.doubleValue()";
    }
    
    public Result test() {
      byte b = 4;
      double d = b;
      Byte obj = new Byte(b);
      if (obj.doubleValue() == d)
	return new Pass();
      else
	return new Fail();
    }
  }
  
  public static class equalsTest1 implements Test
  {
    public String getName() {
      return "Byte.equals(Object)";
    }

    public Result test() {
      Byte obj1 = null, obj2 = null;
      obj1 = new Byte((byte)1);
      obj2 = new Byte((byte)2);
      if (obj1.equals(obj2))
	return new Fail("1 != 2");
      else
	return new Pass("1 != 2");
    }
  }	

  public static class equalsTest2 implements Test
  {
    public String getName() {
      return "Byte.equals(Object)";
    }

    public Result test() {
      Byte obj1 = null, obj2 = null;
      obj1 = new Byte((byte)1);
      obj2 = new Byte((byte)2);
      obj2 = obj1;
      if (obj1.equals(obj2))
	return new Pass("1 == 1");
      else
	return new Fail("1 == 1");
    }
  }
  
  public static class floatValueTest implements Test
  {
    public String getName() {
      return "Byte.floatValue()";
    }

    public Result test() {
      byte b = 4;
      float f = b;
      Byte obj = new Byte(b);
      if (obj.floatValue() == f)
	return new Pass();
      else
	return new Fail();
    }
  }

  public static class hashCodeTest implements Test
  {
    public String getName() {
      return "Byte.hashCode()";
    }

    public Result test() {
      boolean caught = false;
      Byte obj = new Byte((byte)1);
      int i = obj.hashCode();
      if (i == 1)
	return new Pass();
      else
	return new Fail("hash is " + i + ".  It should be 1.");
    }
  }
      
  public static class intValueTest implements Test
  {
    public String getName() {
      return "Byte.intValue()";
    }

    public Result test() {
      byte b = 4;
      int i = b;
      Byte obj = new Byte(b);
      if (obj.intValue() == i)
	return new Pass();
      else
	return new Fail();
    }
  }
  
  public static class longValueTest implements Test
  {
    public String getName() {
      return "Byte.longValue()";
    }

    public Result test() {
      byte b = 4;
      long l = b;
      Byte obj = new Byte(b);
      if (obj.longValue() == l)
	return new Pass();
      else
	return new Fail();
    }
  }    

  public static class parseByteTest1 implements Test
  {
    public String getName() {
      return "Byte.parseByte(String)";
    }

    public Result test() {
      byte b = Byte.parseByte("1");
      if (b == (byte)1)
	return new Pass();
      else
	return new Fail();
    }
  }      

  public static class parseByteTest2 implements Test
  {
    public String getName() {
      return "Byte.parseByte(String, int)";
    }

    public Result test() {
      byte b = Byte.parseByte("-4", 10);
      if (b == (byte)-4)
	return new Pass();
      else
	return new Fail();
    }
  }

  public static class shortValueTest implements Test
  {
    public String getName() {
      return "Byte.shortValue()";
    }

    public Result test() {
      byte b = 4;
      short s = b;
      Byte obj = new Byte(b);
      if (obj.shortValue() == s)
	return new Pass();
      else
	return new Fail();
    }
  }

  public static class toStringTest1 implements Test
  {
    public String getName() {
      return "Byte.toString()";
    }

    public Result test() {
      Byte obj = new Byte((byte)-2);
      String x = obj.toString();
      if (x.equals("-2"))
	return new Pass();
      else
	return new Fail();
    }
  }
	
  public static class toStringTest2 implements Test
  {
    public String getName() {
      return "Byte.toString(byte)";
    }

    public Result test() {
      String x = Byte.toString((byte)-2);
      if (x.equals("-2"))
	return new Pass();
      else
	return new Fail();
    }
  }

  public static class valueOfTest1 implements Test
  {
    public String getName() {
      return "Byte.valueOf(String, int)";
    }

    public Result test() {
      Byte obj1 = Byte.valueOf("2",10);
      Byte obj2 = new Byte((byte)2);
      if (obj1.intValue() == obj2.intValue())
	return new Pass();
      else
	return new Fail();
    }
  }

  public static class valueOfTest2 implements Test
  {
    public String getName() {
      return "Byte.valueOf(String)";
    }

    public Result test() {
      Byte obj1 = Byte.valueOf("2");
      if (obj1.intValue() == 2)
	  return new Pass();
	else
	  return new Fail();
    }
  }
  
  public static class variablesTest1 implements Test
  {
    public String getName() {
      return "Byte.MIN_VALUE";
    }

    public Result test() {
      byte min = Byte.MIN_VALUE;
      byte max = Byte.MAX_VALUE;
      
      if (min == (byte)-128)
	return new Pass("Byte.MIN_VALUE is -128");
      else
	return new Fail("Byte.MIN_VALUE is " + min + " != -128");
    }
  }

  public static class variablesTest2 implements Test
  {
    public String getName() {
      return "Byte.MAX_VALUE";
    }

    public Result test() {
      byte min = Byte.MIN_VALUE;
      byte max = Byte.MAX_VALUE;
      
      if (max == (byte)127)
	return new Pass("Byte.MAX_VALUE is 127");
      else
	return new Fail("Byte.MAX_VALUE is " + max + " != 127");
    }
  }

  public static class variablesTest3 implements Test
  {
    public String getName() {
      return "Byte.TYPE.getName()";
    }

    public Result test() {
      String x = Byte.TYPE.getName();
      if (x.equals("byte") != true)
	return new Fail("Byte.TYPE.getName() is " + x + " != byte");
      else
	return new Pass("Byte.TYPE.getName() is byte");
    }
  }

  public static class typeInstance implements Test
  {
    public String getName() {
      return "Byte.TYPE.newInstance()";
    }

    public Result test() {
      try {
        Object b = Byte.TYPE.newInstance();
        return new Fail("Byte.TYPE.newInstance succeeded.");
      }
      catch (InstantiationException e) {
	return new Pass("Byte.TYPE.newInstance failed with exception '" + 
			e.toString() + "'");
      }
      catch (Exception ex) {
        return new Fail("Byte.TYPE.newInstance threw incorrect exception '" 
			+ ex.toString() + "'");
      }
    }
  }
}

public class CastTest
{
  public static void main(String args[])
    {
      d2d();
      l2d2l();
      d2l2d();
      f2d2f();
      d2f2d();
      i2f2i();
      l2f2l();
      f2l2f();
    }

  static void d2d()
    {
      String msg = "double -> double ";

      try {
	double dvalue1 = 4.2;
	double dvalue2 = (double)dvalue1;
	if (dvalue1 != dvalue2)
	  failed(msg + dvalue1 + " != " + dvalue2);
	else
	  passed(msg + dvalue1 + " == " + dvalue2);
      }
      catch (Exception e)
	{
	  failed(msg + " : exception " + e.toString());
	}
    }

  static void l2f2l()
    {
      String msg = "long -> float -> long ";
      
      try {
	long lvalue = 123;
	float fvalue = (float)lvalue;
	long lvalue2 = (long)fvalue;
	if (lvalue != lvalue2)
	  failed(msg + lvalue + " != " + lvalue2 + " (float)" + fvalue);
	else
	  passed(msg + lvalue + " == " + lvalue2 + " (float)" + fvalue);
      }
      catch (Exception e)
	{
	  failed(msg + " : exception " + e.toString());
	}
    }
  static void i2f2i()
    {
      String msg = "int -> float -> int ";
      
      try {
	int ivalue = 123;
	float fvalue = (float)ivalue;
	int ivalue2 = (int)fvalue;
	if (ivalue != ivalue2)
	  failed(msg + ivalue + " != " + ivalue2 + " (float)" + fvalue);
	else
	  passed(msg + ivalue + " == " + ivalue2 + " (float)" + fvalue);
      }
      catch (Exception e)
	{
	  failed(msg + " : exception " + e.toString());
	}
    }
  static void f2d2f()
    {
      String msg = "float -> double -> float ";

      try {
	float fvalue = 123.0f;
	double dvalue = (double)fvalue;
	float fvalue2 = (float)dvalue;
	
	if (fvalue != fvalue2)
	  failed(msg + fvalue + " != " + fvalue2 + " (double)" + dvalue);
	else
	  passed(msg + fvalue + " == " + fvalue2 + " (double)" + dvalue);
      }
      catch (Exception e)
	{
	  failed(msg + " : exception " + e.toString());
	}
    }
  static void f2l2f()
    {
      String msg = "float -> long -> float ";

      try {
	float fvalue = 123.0f;
	long lvalue = (long)fvalue;
	float fvalue2 = (float)lvalue;
	
	if (fvalue != fvalue2)
	  failed(msg + fvalue + " != " + fvalue2 + " (long)" + lvalue);
	else
	  passed(msg + fvalue + " == " + fvalue2 + " (long)" + lvalue);
      }
      catch (Exception e)
	{
	  failed(msg + " : exception " + e.toString());
	}
    }
  static void d2f2d()
    {
      String msg = "double -> float -> double ";

      try {
	double dvalue = 123.0;
	float fvalue = (float)dvalue;
	double dvalue2 = (double)fvalue;
	if (dvalue != dvalue2)
	  failed(msg + dvalue + " != " + dvalue2 + " (float)" + fvalue);
	else
	  passed(msg + dvalue + " == " + dvalue2 + " (float)" + fvalue);
      }
      catch (Exception e)
	{
	  failed(msg + " : exception " + e.toString());
	}
    }
  static void l2d2l()
    {
      String msg = "long -> double -> long ";

      try {
	long lvalue = 1023;
	double dvalue = (double)lvalue;
	long lvalue2 = (long)dvalue;
	
	if (lvalue != lvalue2)
	  failed(msg + lvalue + " != " + lvalue2 + " (double)" + dvalue);
	else
	  passed(msg + lvalue + " == " + lvalue2 + " (double)" + dvalue);
      }
      catch (Exception e)
	{
	  failed(msg + " : exception " + e.toString());
	}
    }
  static void d2l2d()
    {
      String msg = "double -> long -> double ";

      try {
	double dvalue = 123.0;
	long lvalue = (long)dvalue;
	double dvalue2 = (double)lvalue;
	if (dvalue != dvalue2)
	  failed(msg + dvalue + " != " + dvalue2 + " (long)" + lvalue);
	else
	  passed(msg + dvalue + " == " + dvalue2 + " (long)" + lvalue);
      }
      catch (Exception e)
	{
	  failed(msg + " : exception " + e.toString());
	}
    }
  static void passed(String msg)
    {
      System.out.println("PASSED: "+msg);
    }
  static void failed(String msg)
    {
      System.out.println("FAILED: "+msg);
    }
}

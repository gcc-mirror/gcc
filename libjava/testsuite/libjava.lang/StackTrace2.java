// Check that stack trace's work, and stack trace line numbers, if available,
// are correct.

public class StackTrace2
{
  public static void main(String[] args) 
  { 
    try
    {
      new StackTrace2().a();
    }
    catch (Exception x)
    {
      StackTraceElement[] trace = x.getStackTrace();
      checkTrace(trace);
    }
  }

  void a() 
  { 
    new Inner().foo();
  }
  
  class Inner
  {
    public void foo()
    {
      doCrash(null);
    }  

    public void doCrash(Object o)
    {
      o.toString();
    }
  }  
  
  static void checkTrace(StackTraceElement[] trace)
  {
    System.out.println("Trace length = " + trace.length);
    checkLine(trace[0], "StackTrace2$Inner", "doCrash", 33);
    checkLine(trace[1], "StackTrace2$Inner", "foo", 28);
    checkLine(trace[2], "StackTrace2", "a", 21);
    checkLine(trace[3], "StackTrace2", "main", 10);
  }
  
  static void checkLine(StackTraceElement frame, String expected_cl, 
                	String expected_method, int expected_line)
  {
    if (frame.getClassName().equals(expected_cl))
      System.out.print(expected_cl);
    else
      System.out.print("FAIL - expected " + expected_cl + ", got: " + 
		       frame.getClassName());
    
    System.out.print(".");

    if (frame.getMethodName().equals(expected_method))
      System.out.print(expected_method);
    else
      System.out.print("FAIL - expected " + expected_method + ", got: " +
		       frame.getMethodName());

    System.out.print(":");
    
    // Permit either the correct line number or no line number. This is so
    // we don't fail on platforms that don't yet support reading debug info 
    // for stack traces, or when no debug info is available.
    if (frame.getLineNumber() < 0
        || (frame.getLineNumber() == expected_line
            && frame.getFileName().equals("StackTrace2.java")))
      System.out.println("OK");
    else
      System.out.println("FAIL - expected " + expected_line + ", got: "
			 + frame.getLineNumber() + ", in file "
			 + frame.getFileName());
  }
}

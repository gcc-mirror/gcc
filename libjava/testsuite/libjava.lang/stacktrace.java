/* This test should test the stacktrace functionality.
   We only print ClassName and MethName since the other information
   like FileName and LineNumber are not consistent while building
   native or interpreted and we want to test the output inside the dejagnu
   test environment.
   Also, we have to make the methods public since they might be optimized away
   with inline's and then the -O3/-O2 execution might fail.
*/
public class stacktrace {
  public static void main(String args[]) {
    try {
      new stacktrace().a();
    } catch (TopException e) {
    }
  }

  public void a() throws TopException {
    try {
      b();
    } catch (MiddleException e) {
      throw new TopException(e);
    }
  }

  public void b() throws MiddleException {
    c();
  }

  public void c() throws MiddleException {
    try {
      d();
    } catch (BottomException e) {
      throw new MiddleException(e);
    }
  }

  public void d() throws BottomException {
    e();
  }

  public void e() throws BottomException {
    throw new BottomException();
  }
}

class TopException extends Exception {
  TopException(Throwable cause) {
    super(cause);
  }
}

class MiddleException extends Exception {
  MiddleException(Throwable cause) {
    super(cause);
  }
}

class BottomException extends Exception {
  BottomException() {
    StackTraceElement stack[] = this.getStackTrace();
    for (int i = 0; i < stack.length; i++) {
      String className = stack[i].getClassName();
      String methodName = stack[i].getMethodName();
      System.out.println(className + "." + methodName);
    }
  }
}

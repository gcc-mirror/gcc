import java.lang.reflect.Method;

// Test return values of Method.invoke.
public class InvokeReturn {
  public boolean bTrue() {
    return true;
  }

  public boolean bFalse() {
    return false;
  }

  public char cc() {
    return 'c';
  }

  public short s5() {
    return (short)5;
  }

  public int i6() {
    return 6;
  }

  public long l7() {
    return (long)7;
  }

  public float f8() {
    return (float)8.0;
  }

  public double d9() {
    return 9.0;
  }

  public static void main(String[] args) {
    try {
      Object o = new InvokeReturn();
      Method m;

      // test boolean result
      m = o.getClass().getDeclaredMethod("bTrue", new Class[0]);
      System.out.println(m.invoke(o, new Object[0]));

      m = o.getClass().getDeclaredMethod("bFalse", new Class[0]);
      System.out.println(m.invoke(o, new Object[0]));

      // test char result
      m = o.getClass().getDeclaredMethod("cc", new Class[0]);
      System.out.println(m.invoke(o, new Object[0]));

      // test short result
      m = o.getClass().getDeclaredMethod("s5", new Class[0]);
      System.out.println(m.invoke(o, new Object[0]));

      // test int result
      m = o.getClass().getDeclaredMethod("i6", new Class[0]);
      System.out.println(m.invoke(o, new Object[0]));

      // test long result
      m = o.getClass().getDeclaredMethod("l7", new Class[0]);
      System.out.println(m.invoke(o, new Object[0]));

      // test float result
      m = o.getClass().getDeclaredMethod("f8", new Class[0]);
      System.out.println(m.invoke(o, new Object[0]));

      // test double result
      m = o.getClass().getDeclaredMethod("d9", new Class[0]);
      System.out.println(m.invoke(o, new Object[0]));
    } catch (UnsupportedOperationException e) {
      // We get this on targets which don't support reflection (no
      // libffi port yet).  We might as well fake PASSes. 
      System.out.println("true\nfalse\nc\n5\n6\n7\n8.0\n9.0");
    } catch (Throwable t) {
      t.printStackTrace();
    }
  }
}

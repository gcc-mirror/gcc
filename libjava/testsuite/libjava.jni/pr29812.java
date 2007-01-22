import java.io.File;
import java.net.*;
import java.lang.reflect.Method;

public class pr29812
{
  static {
    System.loadLibrary("pr29812");
  }

  public static native void baseN();

  public static void main(String[] args) throws Throwable
  {
    // Make sure JNI environment is initialized.
    baseN();

    File jar = new File(args[0]);
    URL u = jar.toURL();
    URLClassLoader uc = new URLClassLoader(new URL[] { u });
    Class k = uc.loadClass("pr29812_injar");
    Method m = k.getMethod("doit", (Class[]) null);
    m.invoke(null, (Object[]) null);
  }
}

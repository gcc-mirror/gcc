public class jniutf
{
  native void printString (String str);

  static
  {
    System.loadLibrary ("jniutf");
  }

  public static void main (String[] args)
  {

    String s1 = new String("\u3040\u3041\u3042\u3043\u3044\u3045\u3046\u3047\u3048\u3049\u304A\u304B\u304C\u304D\u304E\u304F\u3050\u3051\u3052\u3053\u3054\u3055\u3056\u3057\u3058\u3059\u305A\u305B");
    new jniutf().printString (s1);
  }
}

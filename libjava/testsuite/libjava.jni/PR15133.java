/* Test case for PR java/15133 */
public class PR15133
{
  public void printIt (String dummy) { }

  public native void printIt (int num);

  public static void main (String[] args)
  {
    System.loadLibrary ("PR15133");

    new PR15133( ).printIt( 1729);
  }
}

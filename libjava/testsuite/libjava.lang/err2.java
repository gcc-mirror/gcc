/*--------------------------------------------------------------------------*/
/* File name  : err2.java                                              */
/*            :                                                             */
/* Cause      : Operator "+=" error in char,byte,short type                 */
/*            :                                                             */
/* Message    : err2.java: In class `err2':                       */
/*            : err2.java: In method `main(java.lang.String[])':       */
/*            : err2.java:22: Incompatible type for `='. Explicit cast */
/*            :  needed to convert `int' to `byte'.                         */
/*            :                 a %= b;                                     */
/*            :                   ^                                         */
/*            : 1 error                                                     */
/*--------------------------------------------------------------------------*/

public class err2 {
  public static void main(String[] args) {
    byte a = -16;
    byte b = 10;

    a %= b;

    if ( a == -6 ) {
      System.out.println("OK");
    } else {
      System.out.println("a = " +a);
    }
  }
}

/*--------------------------------------------------------------------------*/
/* File name  : err10.java                                                  */
/*            :                                                             */
/* Cause      : Operator >>> doesn't work correctly when value is negative. */
/*            :                                                             */
/* Message    : NG : a = -2                                                 */
/*--------------------------------------------------------------------------*/

public class err10 {
  public static void main(String[] args) {
    int a = -3;

    a = a>>>1;

    if ( a == 2147483646 ) {
      System.out.println("OK"); 
    } else {
      System.out.println("NG:[2147483646]-->[" +a+ "]"); 
    }

  }
}


/*--------------------------------------------------------------------------*/
/* File name  : err6.java                                              */
/*            :                                                             */
/* Cause      : Array evaluation order                                      */
/*            :                                                             */
/* Message    : NG:[1]-->[4]                                                */
/*            :                                                             */
/* Note       : JLS 15.9 Array Creation Expressions (p315--)                */
/*            :  p318 line3                                                 */
/*            :[Each dimension expression is fully evaluated                */
/*            : before any part of any dimension expression to its right.]  */
/*--------------------------------------------------------------------------*/

public class err6 {
  public static void main(String[] args) {
    int[] x = { 10, 11, 12, 1, 14 };
    int[] y = { 1, 2, 3, 4, 5, 6 };

    if ( x[(x=y)[2]] == 1 ) {
      System.out.println("OK");
    } else {
      System.out.println("NG:[1]-->[" +x[(x=y)[2]]+ "]");
    }
  }
}


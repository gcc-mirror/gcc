/*--------------------------------------------------------------------------*/
/* File name  : err3.java                                              */
/*            :                                                             */
/* Cause      : Evaluation sequence of the formula which used               */
/*            : the substitution operator is not performed correctly.       */
/*            :                                                             */
/* Message    : NG1:[27]-->[9]                                              */
/*            : NG2:[27]-->[9]                                              */
/*            :                                                             */
/* Note       : JLS 15.6 Evaluation Order (p305)                            */
/*                 S15.6.1 Evaluate Left-Hand Operand First                 */
/*            : A formula should be evaluated to 9*3 instead of 3*3.        */
/*--------------------------------------------------------------------------*/

public class err3 {
  public static void main(String[] args) {
    int x = 9;
    x *= (x = 3);
    if ( x == 27 ) {
      System.out.println("OK1");
    } else {
      System.out.println("NG1:[27]-->["+x+"]");
    }

    int y = 9;
    y = y * (y = 3);
    if ( y == 27 ) {
      System.out.println("OK2");
    } else {
      System.out.println("NG2:[27]-->["+y+"]");
    }
  }
}


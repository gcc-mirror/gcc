/*--------------------------------------------------------------------------*/
/* file_name  : err4.java                                              */
/*            :                                                             */
/* Cause      : Evaluation of the array which used the substitution         */
/*            : operator is not performed correctly.                        */
/*            :                                                             */
/* Message    : NG1:[27}-->[9.0]                                            */
/*            : NG1:[27}-->[9.0]                                            */
/*--------------------------------------------------------------------------*/

public class err4 {
  public static void main(String[] args) {

    // TEST1
    float []a = {9f};
    a[0] *= (a[0] = 3f);

    if ( a[0] == 27 ) {
      System.out.println("OK1");
    } else {
      System.out.println("NG1:[27}-->["+a[0]+"]");
    }

    //TEST2
    float [] b = {9f};
    b[0] = (float)(b[0] * (b[0] = 3f));
    if ( b[0] == 27 ) {
      System.out.println("OK1");
    } else {
      System.out.println("NG1:[27}-->["+b[0]+"]");
    }
  }
}


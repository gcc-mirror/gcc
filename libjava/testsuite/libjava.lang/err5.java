/*--------------------------------------------------------------------------*/
/* file_name  : err5.java                                              */
/*            :                                                             */
/* Cause      : Evaluation order of method argument is not correct.         */
/*            :                                                             */
/* Message    : NG                                                          */
/*            : a:[1]-->[2]                                                 */
/*            : b:[3]-->[3]                                                 */
/*            : c:[2]-->[2]                                                 */
/*            :                                                             */
/* Note       : JLS 15.6 Evaluation Order                                   */
/*                 S15.6.4 Argument Lists are Evaluated Left-to-Right(p309) */
/*             [Each argument expression appears to be fully evaluated      */
/*              before any part of any argument expression to its right.]   */
/*--------------------------------------------------------------------------*/

public class err5 {
  public static void main(String[] args) {
    int x = 1;

    err5 obj = new err5();
    obj.print(x, x = 3, x = 2);
  }

  void print(int a, int b, int c) {
    if ( a == 1 && b == 3 && c == 2 ) {
      System.out.println("OK");
    } else {
      System.out.println("NG");
      System.out.println("a:[1]-->["+a+"]");
      System.out.println("b:[3]-->["+b+"]");
      System.out.println("c:[2]-->["+c+"]");
    }
  }
}

/*--------------------------------------------------------------------------*/
/* File name  : err8.java                                              */
/*            :                                                             */
/* Cause      : When "do while" statement has only "break", error.          */
/*            :                                                             */
/* Message    : err8.java: In class `err8': xxxxxx                */
/*            : err8.java: In method `main(java.lang.String[])':       */
/*            : err8.java:20: Unreachable statement.                   */
/*            :                 } while (true) ;                            */
/*            :                         ^                                   */
/*            : 1 error                                                     */
/*--------------------------------------------------------------------------*/

public class err8 {
  public static void main(String[] args) {
    do {
      break;
    } while (true) ;

    System.out.println("OK");
  }
}


/*--------------------------------------------------------------------------*/
/* File name  : err9.java                                              */
/*            :                                                             */
/* Cause      : When I use "labeled continue" in "for"statement, error      */
/*            :                                                             */
/* Message    :  In class `err9':                                      */
/*            :  In method `main(java.lang.String[])':                      */
/*            : 22: `continue' must be in loop.                             */
/*            :                 continue  movehere;                         */
/*            :                 ^                                           */
/*            : 1 error                                                     */
/*--------------------------------------------------------------------------*/

public class err9 {
  public static void main(String[] args) {
    int y = 0;

  movehere: for ( int x = 0; x < 10; x++ ) {
    if ( x > 2 ) {
      continue  movehere;
    }
    y++;
  }

  if ( y == 3 ) {
    System.out.println("OK");
  } else {
    System.out.println("NG:[3]-->[" +y+ "]");
  }
  }
}


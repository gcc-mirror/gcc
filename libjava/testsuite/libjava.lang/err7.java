/*--------------------------------------------------------------------------*/
/* File name  : err7.java                                              */
/*            :                                                             */
/* Cause      : When exists both array (more than 10 elements) and          */ 
/*              "for" or "while" or "do while" statement , error.           */
/*            :                                                             */
/* Message    : /var/tmp/cc1oQM8i.s: Assembler messages:                    */
/*            : /var/tmp/cc1oQM8i.s:243: Fatal error: Symbol $L2 already def*/
/*            : fined.                                                      */
/*--------------------------------------------------------------------------*/

public class err7 {
  public static void main(String[] args) {
    int[] ary = {0,1,2,3,4,5,6,7,8,9};

    while ( true ) {
      break;
    }

    System.out.println("OK");
  }
}


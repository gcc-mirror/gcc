/*--------------------------------------------------------------------------*/
/* Name       : N19990310_02                                                */
/*            :                                                             */
/* Cause      : Array value calculation in for,while,do while loop is not   */
/*            : correct                                                     */
/*            :                                                             */
/* Message    : NG:[3]-->[2]                                                */
/*--------------------------------------------------------------------------*/

public class N19990310_02 {
	public static void main(String[] args) {
		int y[] = {2};
		for ( int i = 0; i < 1; i++ ) {
			y[i] += 1;
			if ( y[i] != 3 ) {
				System.out.println("NG:[3]-->[" +y[i]+ "]");
			}
			else
			  System.out.println("OK");
		}
	}
}

/*--------------------------------------------------------------------------*/
/* Name       : N19990310_3                                                 */
/*            :                                                             */
/* Cause      : Evaluation order miss, when make integer array.             */
/*            :                                                             */
/* Message    : NG:[4]-->[1]                                                */
/*--------------------------------------------------------------------------*/

public class N19990310_3 {
	public static void main(String[] args) {

		int x = 4; 

		int ary[][] = new int[x][x=1];

		if ( ary.length == 4 ) {
			System.out.println("OK");
		} else {
			System.out.println("NG:[4]-->[" +ary.length+ "]");
		}
	}
}



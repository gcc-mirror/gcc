/*--------------------------------------------------------------------------*/
/* Name       : N19990310_4.java                                            */
/*            :                                                             */
/* Cause      : assignment operator makes error in char,byte,short variable */
/*            :                                                             */
/* Message    : In class `N19990310_4':                                     */
/*            : In method `main(java.lang.String[])':                       */
/*            : Incompatible type for `='. Explicit cast needed to convert `*/
/*            : `int' to `char'.                                            */
/*            :                 x += (x = 3);                               */
/*            :                   ^                                         */
/*            : 1 error                                                     */
/*--------------------------------------------------------------------------*/

public class N19990310_4 {
	public static void main(String[] args) {
		char x = 9;

		x += (x = 3);
		if ( x == 12 ) {
			System.out.println("OK");
		} else {
			System.out.println("NG");
		}
	}
}


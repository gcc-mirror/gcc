/*--------------------------------------------------------------------------*/
/* Name       : N19990310_5.java                                            */
/*            :                                                             */
/* Cause      : A assignment operator makes error in char,byte,short array  */
/*            : element                                                     */
/*            :                                                             */
/* Message    : In class `N19990310_5':                                     */
/*            : In method `main(java.lang.String[])':                       */
/*            : Incompatible type for `='. Explicit cast needed to convert `*/
/*            : `int' to `char'.                                            */
/*            :                 a[0] += (a[0] = 3);                         */
/*            :                      ^                                      */
/*            : 1 error                                                     */
/*--------------------------------------------------------------------------*/

public class N19990310_5 {
	public static void main(String[] args) {
		char[] a = {9};
		a[0] += (a[0] = 3);

		if ( a[0] == 12 ) {
			System.out.println("OK");
		} else {
			System.out.println("NG");
		}
	}
}



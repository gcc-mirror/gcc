/*--------------------------------------------------------------------------*/
/* File name  : err13.java                                             */
/*            :                                                             */
/* Cause      : Conversion from zero to String type is not correct.         */
/*            :                                                             */
/* Message    : NG : test                                                   */
/*--------------------------------------------------------------------------*/

public class err13 {
	public static void main(String[] args) {
		String s = "test";
		s += 0;

		if ( s.equals("test0") ) {
			System.out.println("OK");
		} else {
			System.out.println("NG : " +s);
		}
	}
}


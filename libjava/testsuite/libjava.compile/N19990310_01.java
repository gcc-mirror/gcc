/*--------------------------------------------------------------------------*/
/* name       : N19990310_01                                                */
/*            :                                                             */
/* cause      :  When compare string with connected strings, error.         */
/*            :                                                             */
/* Message    : Internal compiler error: program jc1 got                    */
/*            : fatal signal 11                                             */
/*--------------------------------------------------------------------------*/

public class N19990310_01 {
	public static void main(String[] args) {

		if ( "Hello" == ("Hel"+"lo") ) {
		}

		System.out.println("OK");
	}
}

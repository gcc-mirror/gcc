/*--------------------------------------------------------------------------*/
/* Name       : N19990317.java                                              */
/*            :                                                             */
/* Cause      : When initialize valiable whose name is equal to method      */
/*            : return value, error.                                        */
/*            :                                                             */
/* Message    : Internal compiler error: program jc1 got                    */
/*            :  fatal signal 11                                            */
/*--------------------------------------------------------------------------*/

class Point {}

public class N19990317 {

	Point  func() {
		return null;
	}

	public static void main(String[] args) {
		int Point = 2; 
		System.out.println("OK");
	}
}

/*--------------------------------------------------------------------------*/
/* File name : err12.java                                              */
/*            :                                                             */
/* Cause      : Cast negative floating point to char makes error            */
/*            :                                                             */
/* Message    : Internal compiler error in functi on convert_move           */
/*--------------------------------------------------------------------------*/

public class err12 {
	public static void main(String[] args){
		char x1, x2;

		float y = -10000f;

		x1 = (char)y;		// err
		x2 = (char)-10000f;	// ok

		if ( x1 == x2 ) {
			System.out.println("OK");
		} else {
			System.out.println("NG");
			System.out.println("x1:[65520]-->[" +(x1-0)+"]");
			System.out.println("x2:[65520]-->[" +(x2-0)+"]");
		}
	}
}


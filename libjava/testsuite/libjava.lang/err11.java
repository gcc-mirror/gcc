/*--------------------------------------------------------------------------*/
/* File name : err11.java                                                   */
/*            :                                                             */
/* Cause      : If I declare both array which have more than 10 element     */
/*            : and switch statement, make error.                           */
/*            :                                                             */
/* Message    : err11.java:1: Missing class name.                      */
/*            : public class err11                                     */
/*            :              ^                                              */
/*            : err11.java:1: Class or interface declaration expected. */
/*            : public class err11                                     */
/*            :              ^                                              */
/*            : 2 errors                                                    */
/*--------------------------------------------------------------------------*/

public class err11
{
	public static void main(String args[])
	{
		int i;
		short ary1[] = {12,23,34,45,56,67,78,89,90,111};

		for(i=0; i<10; i++) {
			switch(ary1[i]) {
				case 111 : System.out.println("OK");
				default : break;
			}
		}
	}
}

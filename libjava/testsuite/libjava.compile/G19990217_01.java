/*-------------------------------------------------------------------------*/
/* File name : G19990217_01                                                */
/*           :                                                             */
/* Cause     : Compiler seems to run endlessly.                            */
/*           :                                                             */
/* Message   : No message. It's not stop to compile.                       */
/*-------------------------------------------------------------------------*/

public class G19990217_01 {
  int foo() {
	try {
	  return 0;
	} 
	finally { ; }
  }
}

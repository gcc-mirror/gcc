/*-------------------------------------------------------------------------*/
/* File name : G19990210_3                                                 */
/*           :                                                             */
/* Cause     :                                                             */
/*           :                                                             */
/* Message   : G19990210_3.java: In class `G19990210_3':                   */
/*           : G19990210_3.java: In method `foo()':                        */
/*           : G19990210_3.java:23: Invalid argument to `++'.              */
/*           :                         o.i++;                              */
/*           :                            ^                                */
/*           : 1 error                                                     */
/*-------------------------------------------------------------------------*/
public class G19990210_3  {
  static void foo() {
	bar o = new bar();
	synchronized(o) {
	  o.i++;
	}
  }
}
class bar {
  static int i;
}

/*-------------------------------------------------------------------------*/
/* File name : err1.java                                                   */
/*           :                                                             */
/* Cause     : When I make class-type Object.                              */
/*           :                                                             */
/* Message   : Internal compiler error: program jc1 got                    */
/*           :  fatal signal 11                                            */
/*-------------------------------------------------------------------------*/

class A {
  int counter = 100;
}

public class err1 {
  public static void main(String[] args) {
    A array[] = new A[10]; //err
    array[0] = new A();

    if ( array[0].counter == 100 ) {
      System.out.println("OK");
    } else {
      System.out.println("NG:[100]-->[" +array[0].counter+ "]");
    }
  }
}

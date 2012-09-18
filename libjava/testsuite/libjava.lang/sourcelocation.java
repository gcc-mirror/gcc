/* This test should test the source location attribution.
   We print the line number of different parts of the program to make sure
   that the source code attribution is correct.
   To make this test pass, one need to have up-to-date addr2line installed
   to parse the dwarf4 data format.
*/
public class sourcelocation {
  public static void main(String args[]) {
    try {
      System.out.println(new Exception().getStackTrace()[0].getLineNumber());
      throw new Exception();
    } catch (Exception e) {
      System.out.println(new Exception().getStackTrace()[0].getLineNumber());
    } finally {
      System.out.println(new Exception().getStackTrace()[0].getLineNumber());
    }
  }
}

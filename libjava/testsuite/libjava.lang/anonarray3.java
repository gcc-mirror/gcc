// Class anonarray3
// Generated on Tue Feb  8 19:18:10 PST 2000
//

class anonarray3 {
  private static final int[] foo (int x) {
      return new int[] { x+1 };
  }
  public static void main (String[] arg)
  {
    System.out.println (foo (34)[0]);
  }
}

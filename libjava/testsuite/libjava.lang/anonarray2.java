// Class anonarray2
// Generated on Tue Feb  1 21:14:06 PST 2000
// Anonymous array, with a non primitive type.

class anonarray2 {

  static void foo (String [][] x) {
      for (int i = 0; i < x.length; i++)
          {
              for (int j = 0; j < x[i].length; j++)
                  System.out.print (x[i][j]);
              System.out.println();
          }
  }

  public static void main (String[] arg)
  {
      foo (new String[][] {{"2","3"},{"5","7"}});
      System.out.println ((new String [][] {{"11","13"},{"17","19"}}).length);
      System.out.println ((new String [][] {{"23","29"},{"31","37"}})[0][1]);
  }
}

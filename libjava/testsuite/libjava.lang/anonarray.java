// Class anonarray
// Generated on Tue Feb  1 16:11:29 PST 2000
// Simple anonymous array, of primitive types.

class anonarray {

  static void foo (int [][] x) {
      for (int i = 0; i < x.length; i++)
          {
              for (int j = 0; j < x[i].length; j++)
                  System.out.print (x[i][j]);
              System.out.println();
          }
  }
  
  public static void main (String[] arg)
  {  
      foo (new int[][] {{2,3},{5,7}});
      System.out.println ((new int [][] {{11,13},{17,19}}).length);
      System.out.println ((new int [][] {{23,29},{31,37}})[0][1]);
  }
}

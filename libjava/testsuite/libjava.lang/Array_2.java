// Test to make sure multidimensional arrays work.
// From Bryce McKinlay

public class Array_2
{
  static final int a = 10, b = 15;

  public static void main(String args[])
  {
    int[][] foo = new int [a][b];
    System.out.println(foo.length);
    System.out.println(foo[a-1].length);
  }
}

public class G19990303_02
{
  public static void main (String[] args)
  {
    int i = -1;
    try
      {
	System.out.println ("Pass 1");
	int[][][] arrayInt = new int[i][1][1];
	int ii = arrayInt[i - 1][0][0];
	System.out.println ("Pass 2");
      }
    catch (NegativeArraySizeException e)
      {
	System.out.println ("Pass NegativeArraySizeException");
      }
    System.out.println ("Pass 3");
  }
}

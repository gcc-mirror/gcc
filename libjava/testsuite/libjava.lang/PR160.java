public class PR160
{
  static final int len = 100;
  
  public static void main(String args[])
  {
    
    double[] a = new double[len];
    double[] b = new double[len];
    
    for (int i = 0; i < len ; i++)
      {
	a[i] = 0.65;
      }
    
    System.arraycopy(a, 0, b, 0, len);
    
    boolean errors = false;
    
    for (int i = 0; i < len ; i++)
      {
	if (a[i] != b[i])
	  {
	    System.out.println("ERROR! " + a[i] + " != " 
			       + b[i] + " at index " + i);
	    errors = true;
	  }
      }
    
    if (!errors)
      System.out.println("ok");
    
  }
}

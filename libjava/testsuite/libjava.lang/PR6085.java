public class PR6085
{
  public static void main(String[] args)
  {
    F1 f1 = new F1();
  }
  
    static class F1
    {
      F11 f11;
      F12 f12;
      
      F1()
      {
        f12 = new F12();
	System.out.println (f12.i);
	System.out.println (f12.k);
      }
      
      class F11
      { 
	int k = 90;
	F11() {}
      }

      class F12 extends F11 
      {
	int i;
	F12()
	{
	  i = 17;
	}
      }
    }
}

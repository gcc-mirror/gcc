public class Divide_2
{
  static void poo()
  {
    int n = 4/0;
  }
  public static void main(String[] argv)
  {
    try
      {
	poo();
      }
    catch (ArithmeticException _)
      {
	return;
      }
    
    throw new RuntimeException();
  }
}

public class uesc
{
  public foo (Object[] v[])
  {
    char z = '\uuu00a0';
    char y = '\u00au0';		// At one point we erroneously
				// accepted this.
  }
}

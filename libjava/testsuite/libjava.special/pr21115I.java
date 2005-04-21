public class pr21115I extends pr21115
{
  public boolean test(boolean a, boolean b,
		      boolean c, boolean d,
		      boolean e, boolean f,
		      boolean g, boolean h,
		      boolean i, boolean j)
  {
    return a && b && c && d && e && f && g && h && i;
  }
  
  public boolean test(int a, int b,
		      int c, int d,
		      int e, int f,
		      int g, int h,
		      int i, int j)
  {
    return true;
  }
}

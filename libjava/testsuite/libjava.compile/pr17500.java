// gcj had a problem compiling code where two anonymous classes had
// captured constructor arguments of the same type but with different
// names.

public class pr17500
{
  public Object m1 (final Object one)
  {
    return new Comparable()
      {
	public int compareTo(Object other)
	{
	  return one == other ? 0 : 1;
	}
      };
  }

  public Object m2 (final Object two)
  {
    return new Comparable()
      {
	public int compareTo(Object other)
	{
	  return two == other ? 0 : 1;
	}
      };
  }
}

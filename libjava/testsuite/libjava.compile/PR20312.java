/* Can't throw what the overridden method doesn't. */
public class PR20312
{
  public String toString( ) throws java.io.IOException
  {
    return "SNAFU";
  }
}

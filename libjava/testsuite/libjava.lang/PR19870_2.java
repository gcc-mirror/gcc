// PR19870: Test synthetic accessor generation for private static methods
// accessed across nested class boundaries.
public class PR19870_2
{
  static class A
  {
    private static void foo( )
    {
      System.out.println( "1");
    }

    private static void bar( int x)
    {
      System.out.println( x);
      snafu( );
      PR19870_2.snafu( );
    }
  }

  static class B
  {
    private static void foo( )
    {
      A.foo( );
    }
  }

  private static void snafu( )
  {
    System.out.println( "3");
  }

  public static void main( String[] args)
  {
    A.foo( );
    A.bar( 2);
    B.foo( );
  }
}

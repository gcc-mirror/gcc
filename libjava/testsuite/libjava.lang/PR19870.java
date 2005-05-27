// PR19870: Test static field access across nested class boundaries.
//
public class PR19870
{
  private static int x = 123;

  static class Foo
  {
    private static int junk = 1000;

    static void snafu( )
    {
      System.out.println( x);
      x = 456;
      System.out.println( PR19870.x);
      PR19870.x = 789;
      System.out.println( PR19870.x);

      System.out.println( Bar.junk);
    }
  }

  static class Bar
  {
    private static int junk = 1984;

    static void snafu( )
    {
      System.out.println( Foo.junk);
      Foo.junk = 2000;
      System.out.println( Foo.junk);
    }
  }

  public static void main( String[] args)
  {
    Foo.snafu( );
    Bar.snafu( );

    System.out.println( Foo.junk);
    Foo.junk = 3000;
    System.out.println( Foo.junk);
  }
}

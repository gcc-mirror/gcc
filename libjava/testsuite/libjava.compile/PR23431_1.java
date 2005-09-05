abstract class Foo
{
  public abstract void bar( );
}

class PR23431_1 extends Foo
{
  private void bar( ) { }
}

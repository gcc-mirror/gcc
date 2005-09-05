interface Foo
{
  public void bar( );
}

class PR23431_2 implements Foo
{
  private void bar( ) { }
}

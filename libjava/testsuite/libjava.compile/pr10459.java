public class pr10459
{
  pr10459 x;

  public void aMethod() throws Throwable
  {
    for (; ;x.clone().clone())
      ;
  }
}

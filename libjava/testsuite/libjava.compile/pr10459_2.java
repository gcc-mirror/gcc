public class pr10459_2
{
  pr10459_2 x;

  public void aMethod() throws Throwable
  {
    for (; ;x.clone().clone())
      break;
  }
}

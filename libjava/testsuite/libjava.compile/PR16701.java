class Cl
{
  private static final int CONSTANT1 = 0x001;
  public static final int CONSTANT2 = 0x002 >> CONSTANT1;
}

public class PR16701
{
  public static final int VALUE = Cl.CONSTANT2;
}

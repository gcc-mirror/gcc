public class klass
{
  public static void main (String[] args)
  {
    Class ic1 = Integer.TYPE;
    int[] foo = new int[3];
    Class ic2 = foo.getClass().getComponentType();
    System.out.println(ic1.equals(ic2));
  }
}

// Test StringBuffer.replace(), reverse(), insert(String), append(String), 
// and delete().

public class StringBuffer_1
{
  public static void main(String args[])
  {
    StringBuffer sb = new StringBuffer("45");
    sb.insert(0, "123");
    sb.append("89");
    sb.insert(5, "6");
    sb.insert(6, '7');
    System.out.println (sb);
    
    sb.delete (3, 99);
    
    String foo = sb.toString();
    
    System.out.println (foo);
    sb.reverse();
    System.out.println (foo);

    System.out.println (sb);
    sb = new StringBuffer("1234");    
    System.out.println(sb.reverse());
    
    sb = new StringBuffer("123456789");
    sb.append ("0");
    System.out.println(sb);

    sb.replace (2, 99, "foo");
    System.out.println (sb);

    sb = new StringBuffer("123456789");
    sb.replace (1, 1, "XX");
    System.out.println (sb);

    sb = new StringBuffer("123456789");
    sb.replace (0, 2, "XX");
    System.out.println (sb);

    sb = new StringBuffer("123456789");
    sb.replace (5, 9, "54321");
    System.out.println (sb);

    sb = new StringBuffer("123456789");
    
    sb.delete (1,4);
    System.out.println (sb);    

    // Test bounds checks
    try
    {
      sb.insert (-2, "x");
    }
    catch (StringIndexOutOfBoundsException x)
    {
      System.out.println (x.getClass());
    }

    try
    {
      sb.insert (96, "x");
    }
    catch (StringIndexOutOfBoundsException x)
    {
      System.out.println (x.getClass());
    }

    try
    {
      sb.delete (-2, 2);
    }
    catch (StringIndexOutOfBoundsException x)
    {
      System.out.println (x.getClass());
    }

    try
    {
      sb.delete (96, 418);
    }
    catch (StringIndexOutOfBoundsException x)
    {
      System.out.println (x.getClass());
    }

    try
    {
      sb.delete (4, 2);
    }
    catch (StringIndexOutOfBoundsException x)
    {
      System.out.println (x.getClass());
    }

    try
    {
      sb.replace (-2, 2, "54321");
    }
    catch (StringIndexOutOfBoundsException x)
    {
      System.out.println (x.getClass());
    }

    try
    {
      sb.replace (4, 2, "54321");
    }
    catch (StringIndexOutOfBoundsException x)
    {
      System.out.println (x.getClass());
    }

    try
    {
      sb.replace (12, 18, "54321");
    }
    catch (StringIndexOutOfBoundsException x)
    {
      System.out.println (x.getClass());
    }
  }
}

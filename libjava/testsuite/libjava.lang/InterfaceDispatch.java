/* Test interface dispatch, type checking (instanceof), and casting. */

interface IA
{
  String a();
}

interface IB extends IA
{
  String b();
}

interface IC extends IB
{
  void c();
  int d();
  IB e(int i);
}

interface ID
{
  String z();
  String a();  
}

class CA
{
  String a()
  {
    return "CA a()";
  }
}

class CB implements IB
{
  public String a()
  {
    return "CB a()";
  }
  
  public String b()
  {
    return "CB b()";  
  }
}

class CC extends CB
{
  public int d()
  {
    return 99;
  }
}

class CD extends CC implements IC
{
  public String a()
  {
    return "CD a()";
  }
  
  public void c()
  {
    System.out.println("CD c()");
  }
  
  public int d()
  {
    return 6;
  }
  
  public IB e(int i)
  {
    if (i == 1)
      return new CB();
    else
      return new CD();
  }
}

class CE extends CB implements IB, ID
{
  public String a()
  {
    return ("CE a()");
  }
  
  public String b()
  {
    return ("CE b()");
  }
    
  public String z()
  {
    return("CE z()");  
  }
}


public class InterfaceDispatch
{
  public static void main(String args[])
  {
    new InterfaceDispatch();
  }
  
  public InterfaceDispatch()
  {
    /* _Jv_InstanceOf */
    
    /* Object instanceof CLASS */
    Object obj = new CA();
    
    if (obj instanceof CA)
      {
        System.out.println ("ok 1");
      }
    else
      {
        System.out.println ("FAIL 1");
      }

    obj = new CD();
    
    if (!(obj instanceof CA))
      {
        System.out.println ("ok 2a");
      }
    else
      {
        System.out.println ("FAIL 2a");
      }

    if (obj instanceof CB)
      {
        System.out.println ("ok 2b");
      }
    else
      {
        System.out.println ("FAIL 2b");
      }

    
    /* Object instanceof INTERFACE */
    obj = new CB();
    
    if (!(obj instanceof IC))
      {
        System.out.println("ok 3");
      }
    else
      {
        System.out.println ("FAIL 3");
      }
    
    if (obj instanceof IB)
      {
        System.out.println("ok 4");
      }
    else
      {
        System.out.println ("FAIL 4");
      }
    
    /* InterfaceRef instanceof INTERFACE */
    
    IA ia = new CB();
    
    if (ia instanceof IB)
      {
        System.out.println("ok 5");
      }
    else
      {
        System.out.println ("FAIL 5");
      }
    
    
    if (!(ia instanceof IC))
      {
        System.out.println("ok 6");
      }
    else
      {
        System.out.println ("FAIL 6");
      }
      
    /* InterfaceRef instanceof CLASS */
    
    if (ia instanceof CB)
      {
        System.out.println("ok 7");
      }
    else
      {
        System.out.println ("FAIL 7");
      }
      
    
    if (!(ia instanceof CD))
      {
        System.out.println("ok 8");
      }
    else
      {
        System.out.println ("FAIL 8");
      }    
    
    
    /* _Jv_CheckCast */
    Object obj_ca = new CA();
    Object obj_cc = new CC();    
    
    IA ia2;
    
    try
      {
        ia2 = (IA) obj_cc;
        System.out.println("ok 9");
      }
    catch (ClassCastException x)
      {
        System.out.println("FAIL 9");
      }
    
    CD cd;
    
    try
      {
        cd = (CD) obj_ca;
        System.out.println("FAIL 10");
      }
    catch (ClassCastException x)
      {
        System.out.println("ok 10");
      }
    
    IA ia3;
    
    try
      {
        ia3 = (IB) obj_ca;
        System.out.println("FAIL 11");
      }
    catch (ClassCastException x)
      {
        System.out.println("ok 11");
      }
      
    /* _Jv_LookupInterfaceMethod */
    Object obj_cb = new CB();
    
    IB ib = (IB) obj_cb;
    ib.b();
    if (ib.a().equalsIgnoreCase("CB a()"))
      System.out.println("ok 12");
    else
      System.out.println("FAIL 12");
      
    IC ic = new CD();
    if (ic.a().equalsIgnoreCase("CD a()"))
      System.out.println("ok 13");
    else
      System.out.println("FAIL 13");
          
    if (ic.d() == 6)
      System.out.println("ok 14");
    else
      System.out.println("FAIL 14");
      
    Object ce = new CE();
    
    ib = (IB) ce;
    ID id = (ID) ce;
    
    if (ib.b().equals("CE b()") && id.a().equals("CE a()"))
      System.out.println("ok 15");
    else
      System.out.println("FAIL 15");
    
    String t = ((ID)ce).z();
    
    if (t.equalsIgnoreCase("CE z()"))
      System.out.println("ok 16");
    else
      System.out.println("FAIL 16");
      
    /* Array types */
    
    Object[] obj_a = new CC[10];
    try
      {
        CB[] ca_a = (CB[]) obj_a;
        System.out.println("ok 17");
      }
    catch (ClassCastException x)
      {
        System.out.println("FAIL 17");
      }
    
    if (obj_a instanceof IB[])
      {
        System.out.println("ok 18");      
      }
    else
      {
        System.out.println("FAIL 18");      
      }
    
    IB[] ib_a = new CD[5];
    try 
      {
        CD[] cd_a = (CD[]) ib_a;
        System.out.println("ok 19");
      }
    catch (ClassCastException x)
      {
        System.out.println("FAIL 19");
      }
      
    CA[] ca_a;

    try 
      {
        ca_a = (CA[]) ib_a;
        System.out.println("FAIL 20");
      }
    catch (ClassCastException x)
      {
        System.out.println("ok 20");
      }

    
    /* Primitive types */
    
    short[] short_a = new short[100];
    
    try
      {
        obj = short_a;
        System.out.println("ok 21");
      }
    catch (ClassCastException x)
      {
        System.out.println("FAIL 21");      
      }

    try
      {
        short[] short_b = (short[]) obj;
        System.out.println("ok 22");
      }
    catch (ClassCastException x)
      {
        System.out.println("FAIL 22");      
      }

    int[] short_b;

    try
      {
        short_b = (int[]) obj;
        System.out.println("FAIL 23");
      }
    catch (ClassCastException x)
      {
        System.out.println("ok 23");      
      }

    Object obj1 = new int[25];
    
    if (obj1 instanceof short[])
      {
        System.out.println("FAIL 24");      
      }
    else
      {
        System.out.println("ok 24");
      }
    
    if (obj1 instanceof int[])
      {
        System.out.println("ok 25");
      }
    else
      {
        System.out.println("FAIL 25");
      }
      
    /* null assignment */
    
    CA obj_ca2 = null;
    
    if (obj_ca2 instanceof CA)
      {
        System.out.println("FAIL 26");        
      }
    else
      {
        System.out.println("ok 26");
      }
  }  
}

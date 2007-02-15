public class getlocalvartable
{
  public boolean done = false;

  // num_frames is the number of frames > the original run () call so if
  // num_frames = 1, the thread will have 2 frames, the original Thread.run
  // call, plus one additional
  public int num_frames, thread_num;

  public static int num_threads = 1;

  static
    {
      System.loadLibrary("natgetlocalvartable");
    }

  public double aMethod (float pone, float ptwo)
  {
    float fone, ftwo;
    double done, dtwo;
    
    fone = pone;
    ftwo = 2 * ptwo;
    
    done = 5 * fone;
    dtwo = 6 * ftwo;
    
    return done + dtwo;
  }
  
  public long bMethod (int ipone, int iptwo)
  {
    int ione, itwo;
    long lone, ltwo;
    
    ione = ipone;
    itwo = 5 * iptwo;
    
    lone = ione;
    ltwo = 8 * itwo;
    
    return lone + ltwo;
  }
  
  public Object cMethod (Object op)
  {
    Object oone, otwo;
    oone = op;
    otwo = oone;
    oone = null;
    
    return otwo;
  }

  public static native int do_getlocalvartable_tests ();

  public static void main (String[] args)
  {
    System.out.println ("JVMTI getlocalvartable Interpreted Test");

    do_getlocalvartable_tests ();
  }
}

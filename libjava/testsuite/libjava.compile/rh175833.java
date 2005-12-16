// Follow-on to PR 25429
public class rh175833
{
  private static final Object CONST  = new Object();
    class I {
        public Object f () {
	  // We need an accessor here.
	  return CONST;
        }
    }

  public static void main(String[] args) { }
}

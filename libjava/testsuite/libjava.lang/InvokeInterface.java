import java.lang.reflect.*;

interface one
{
  int n(int N);
}

interface two
{
  int nn(int N);
}

interface three
{
  int nnn(int N);
}

class arse implements one, two
{
  public int n(int N) { return N; }
  public int nn(int N) { return N*2; }
}

class arsey implements two, one, three
{
  public int n(int N) { return N*4; }
  public int nn(int N) { return N*8; }
  public int nnn(int N) { return N*16; }
}

public class InvokeInterface extends arse
{
  int f ()
  {
	return flunk.nn(1);
  }
  static two flunk = new arse();
  static three flunkey = new arsey();
  public static void main(String[] s) throws Throwable
  {
	Class[] argtypes = {Integer.TYPE};
	Method m = two.class.getMethod("nn", argtypes);
	Object[] args = {new Integer(1)};
	System.out.println(flunk.nn(1));
	System.out.println(m.invoke(new arse(), args));
	m = arse.class.getMethod("nn", argtypes);
	System.out.println(m.invoke(new arse(), args));
	m = two.class.getMethod("nn", argtypes);
	System.out.println(m.invoke(new arsey(), args));
	m = three.class.getMethod("nnn", argtypes);
	System.out.println(m.invoke(new arsey(), args));	
	m = arsey.class.getMethod("nnn", argtypes);
	System.out.println(m.invoke(new arsey(), args));	
  }
}

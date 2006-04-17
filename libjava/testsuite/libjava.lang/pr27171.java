public class pr27171 {

	public static void main(String[] args) throws Throwable {
	  // Isolated low surrogate.
	  char x = 56478;  // 0xdc9e
	  String xs = new String(new char[] { x });
	  // Note that we fix a result for our implementation; but
	  // the JDK does something else.
	  System.out.println(xs.getBytes("UTF-8").length);

	  // isolated high surrogate -- at end of input stream
	  char y = 0xdaee;
	  String ys = new String(new char[] { y });
	  // Note that we fix a result for our implementation; but
	  // the JDK does something else.
	  System.out.println(ys.getBytes("UTF-8").length);
	}
}


import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;

public class PR141 {
  public static void test(String str) throws IOException {
    StringReader reader = new StringReader(str);
    StreamTokenizer st = new StreamTokenizer(reader);
    int t = 0;
    
    do {
      t = st.nextToken();
      
      switch (t) {
      case StreamTokenizer.TT_WORD:
	System.out.println("[TT_WORD]: " + st.sval);
	break;
      case StreamTokenizer.TT_NUMBER:
	System.out.println("[TT_NUMBER]: " + st.nval);
	break;
      case StreamTokenizer.TT_EOL:
	System.out.println("[TT_EOL]");
	break;
      case StreamTokenizer.TT_EOF:
	System.out.println("[TT_EOF]");
	break;
      default:
	System.out.println((char)t);
	break;
      }
    } while (t != StreamTokenizer.TT_EOF);
  }
  
  public static void main(String[] args) {
    try {
      test("(a).(b)");
    } catch (Throwable t) {
      t.printStackTrace();
    }
  }
}

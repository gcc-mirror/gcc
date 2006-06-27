// Test case for http://bugzilla.redhat.com/bugzilla/show_bug.cgi?id=194522

import java.io.*;
import java.nio.charset.Charset;

public class RH194522
{
  public static void main(String[] args) throws Exception
  {
    Charset c = Charset.forName("UTF-8");
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    PrintWriter pw = new PrintWriter(new OutputStreamWriter(baos, c));
    pw.println("hi");
    pw.println("bob");
    pw.flush();
    pw.close();
  }
}

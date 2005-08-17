// This tests that a C++ ABI class can derive from a BC ABI class.
// This can't always work, but if the base class does not change then
// it will work fine.

import org.xml.sax.*;

public class bclink extends SAXParseException {
  public bclink() { super ("hi", null); }

  public static void main(String[] args) throws Throwable {
  }
}

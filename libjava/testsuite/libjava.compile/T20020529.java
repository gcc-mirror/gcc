public class T20020529
{
  public void checkXMLLangAttributeValue(String lang)
  {
      int offset = -1;
      if (lang.length() >= 2) {
          char ch0 = lang.charAt(0);
      }
      if (offset > 0) {
          char ch = lang.charAt(offset++);
          if (ch != '-') {
              offset = -1;
          } else {
              while (true)
	      {
                if (ch == '-')
                  ch = lang.charAt(offset++);
                ch = lang.charAt(offset++);
              }
          }
      }
  }
}

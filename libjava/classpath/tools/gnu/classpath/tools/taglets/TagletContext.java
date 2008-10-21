package gnu.classpath.tools.taglets;

import com.sun.javadoc.Doc;

public class TagletContext
{
   protected Doc doc;

   public TagletContext(Doc doc)
   {
      this.doc = doc;
   }

   public Doc getDoc()
   {
      return this.doc;
   }

   public String toString()
   {
      return "TagletContext{doc=" + doc + "}";
   }
}

/* gnu.classpath.tools.doclets.htmldoclet.HtmlPage
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package gnu.classpath.tools.doclets.htmldoclet;

import gnu.classpath.tools.IOToolkit;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.sun.javadoc.Tag;

/**
 *  Allows outputting an HTML document without having to build the
 *  document tree in-memory.
 */
public class HtmlPage
{
   private File file;
   private PrintWriter out;
   private String pathToRoot;
   private String docType;
   private String baseUrl;
   private File rootDir;

   public static final String DOCTYPE_FRAMESET = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">";

   public HtmlPage(File file, String pathToRoot, String encoding, String baseUrl, File rootDir)
      throws IOException
   {
      this(file, pathToRoot, encoding, baseUrl, rootDir, "<!DOCTYPE html PUBLIC \"-//gnu.org///DTD XHTML 1.1 plus Target 1.0//EN\" \"" + pathToRoot + "/resources/xhtml11-target10.dtd\">");
   }

   public HtmlPage(File file, String pathToRoot, String encoding, String baseUrl, File rootDir, String docType)
      throws IOException
   {
      this.file = file;
      OutputStream fileOut = new FileOutputStream(file);
      Writer writer;
      if (null != encoding) {
         writer = new OutputStreamWriter(fileOut,
                                         encoding);
      }
      else {
         writer = new OutputStreamWriter(fileOut);
      }
      this.out = new PrintWriter(new BufferedWriter(writer));
      this.pathToRoot = pathToRoot;
      this.docType = docType;
      this.baseUrl = baseUrl;
      this.rootDir = rootDir;
   }

   public void beginElement(String elementName)
   {
      print('<');
      print(elementName);
      print('>');
   }

   public void beginElement(String elementName, String attributeName, String attributeValue)
   {
      print('<');
      print(elementName);
      print(' ');
      print(attributeName);
      print('=');
      print('\"');
      print(attributeValue);
      print('\"');
      print('>');
   }

   public void beginElement(String elementName, String[] attributeNames, String[] attributeValues)
   {
      print('<');
      print(elementName);
      for (int i=0; i<attributeNames.length; ++i) {
         if (null != attributeValues[i]) {
            print(' ');
            print(attributeNames[i]);
            print('=');
            print('\"');
            print(attributeValues[i]);
            print('\"');
         }
      }
      print('>');
   }

   public void beginElement(String elementName, String attributeName, String attributeValue, String[] attributeNames, String[] attributeValues)
   {
      print('<');
      print(elementName);
      print(' ');
      print(attributeName);
      print('=');
      print('\"');
      print(attributeValue);
      print('\"');
      if (null != attributeNames) {
         for (int i=0; i<attributeNames.length; ++i) {
            if (null != attributeValues[i]) {
               print(' ');
               print(attributeNames[i]);
               print('=');
               print('\"');
               print(attributeValues[i]);
               print('\"');
            }
         }
      }
      print('>');
   }

   public void atomicElement(String elementName)
   {
      print('<');
      print(elementName);
      print("/>");
   }

   public void atomicElement(String elementName, String attributeName, String attributeValue)
   {
      print('<');
      print(elementName);
      print(' ');
      print(attributeName);
      print('=');
      print('\"');
      print(attributeValue);
      print('\"');
      print("/>");
   }

   public void atomicElement(String elementName, String[] attributeNames, String[] attributeValues)
   {
      print('<');
      print(elementName);
      for (int i=0; i<attributeNames.length; ++i) {
         if (null != attributeValues[i]) {
            print(' ');
            print(attributeNames[i]);
            print('=');
            print('\"');
            print(attributeValues[i]);
            print('\"');
         }
      }
      print("/>");
   }


   public void endElement(String elementName)
   {
      print("</");
      print(elementName);
      print('>');
   }


   public void beginDiv(CssClass cssClass)
   {
      String[] divAttributeNames = cssClass.getAttributeNames();
      String[] divAttributeValues = cssClass.getAttributeValues();
      if (null == divAttributeNames) {
         divAttributeNames = new String[0];
      }
      if (null == divAttributeValues) {
         divAttributeValues = new String[0];
      }

      String[] attributeNames = new String[1 + divAttributeNames.length];
      String[] attributeValues = new String[1 + divAttributeValues.length];

      System.arraycopy(divAttributeNames, 0, attributeNames, 1, divAttributeNames.length);
      System.arraycopy(divAttributeValues, 0, attributeValues, 1, divAttributeNames.length);

      attributeNames[0] = "class";
      attributeValues[0] = cssClass.getName();

      beginElement(cssClass.getDivElementName(), attributeNames, attributeValues);
      if (null != cssClass.getInnerElementName()) {
         beginElement(cssClass.getInnerElementName());
      }
   }

   public void endDiv(CssClass cssClass)
   {
      if (null != cssClass.getInnerElementName()) {
         endElement(cssClass.getInnerElementName());
      }
      endElement(cssClass.getDivElementName());
   }

   public void beginSpan(CssClass cssClass)
   {
      beginElement(cssClass.getSpanElementName(), "class", cssClass.getName());
   }

   public void endSpan(CssClass cssClass)
   {
      endElement(cssClass.getSpanElementName());
   }

   public void hr()
   {
      atomicElement("hr");
   }

   public void br()
   {
      atomicElement("br");
   }

   public void print(String text)
   {
      out.print(text);
   }

   public void print(char c)
   {
      out.print(c);
   }

   public void div(CssClass cssClass, String contents)
   {
      beginDiv(cssClass);
      print(contents);
      endDiv(cssClass);
   }

   public void span(CssClass cssClass, String contents)
   {
      beginSpan(cssClass);
      print(contents);
      endSpan(cssClass);
   }

   public void beginPage(String title, String charset, Map stylesheets)
      throws IOException
   {
      beginPage(title, charset, Collections.EMPTY_SET, stylesheets);
   }

   public void beginPage(String title, String charset,
                         Collection keywords, Map stylesheets)
      throws IOException
   {
      print("<?xml version=\"1.0\" encoding=\"" + charset + "\"?>\n");
      print(docType);
      print("<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">");
      beginElement("head");
      beginElement("title");
      print(title);
      endElement("title");
      if (null != baseUrl && baseUrl.length() > 0) {
         StringBuffer url = new StringBuffer();
         url.append(baseUrl);
         if ('/' == url.charAt(url.length() - 1)) {
            url.delete(url.length() - 1, url.length());
         }
         url.append(file.getCanonicalPath().substring(rootDir.getCanonicalPath().length()));
         atomicElement("base",
                       new String[] { "href" },
                       new String[] { url.toString() });
      }
      beginElement("script",
                    new String[] { "src", "type" },
                    new String[] { pathToRoot + "/resources/gjdoc.js", "text/javascript" });
      print("<!-- this comment required for konqueror 3.2.2 -->");
      endElement("script");
      atomicElement("meta",
                    new String[] { "http-equiv", "content" },
                    new String[] { "Content-Type", "text/html; charset=" + charset });
      atomicElement("meta",
                    new String[] { "name", "content" },
                    new String[] { "generator", "GNU Gjdoc Standard Doclet" });
      Iterator keywordIt = keywords.iterator();
      while (keywordIt.hasNext()) {
         String keyword = (String)keywordIt.next();
         atomicElement("meta",
                       new String[] { "name", "content" },
                       new String[] { "keywords", keyword });
      }

      Iterator cssIt = stylesheets.keySet().iterator();
      while (cssIt.hasNext()) {
         String sheetName = (String)cssIt.next();
         String[] sheetFiles = (String[])stylesheets.get(sheetName);

         for (int i=0; i<sheetFiles.length; ++i) {
            String sheetFile = sheetFiles[i];
            atomicElement("link",
                          new String[] { "rel", "type", "href", "title" },
                          new String[] { "stylesheet", "text/css",
                                         pathToRoot + "/" + sheetFile, sheetName });
         }
      }

      endElement("head");
   }

   public void endPage()
   {
      endElement("html");
   }

   public void close()
   {
      out.close();
   }

   public void beginTable(CssClass cssClass)
   {
      beginElement("table", "class", cssClass.getName());
   }

   public void beginTable(CssClass cssClass, String[] attributeNames, String[] attributeValues)
   {
      beginElement("table", "class", cssClass.getName(), attributeNames, attributeValues);
   }

   public void beginRow()
   {
      beginElement("tr");
   }

   public void beginRow(CssClass cssClass)
   {
      beginElement("tr", "class", cssClass.getName(), cssClass.getAttributeNames(), cssClass.getAttributeValues());
   }

   public void beginRow(String attribute, String value)
   {
      beginElement("tr", attribute, value);
   }

   public void beginCell()
   {
      beginElement("td");
   }

   public void beginCell(String attribute, String value)
   {
      beginElement("td", attribute, value);
   }

   public void beginCell(CssClass cssClass)
   {
      beginElement("td", "class", cssClass.getName(), cssClass.getAttributeNames(), cssClass.getAttributeValues());
   }

   public void endCell()
   {
      endElement("td");
   }

   public void cell(CssClass cssClass, String contents)
   {
      beginCell(cssClass);
      print(contents);
      endCell();
   }

   public void endRow()
   {
      endElement("tr");
   }

   public void rowDiv(CssClass cssClass, String contents)
   {
      beginRow(cssClass);
      beginCell("colspan", "2");
      beginDiv(cssClass);
      print(contents);
      endDiv(cssClass);
      endCell();
      endRow();
   }

   public void endTable()
   {
      endElement("table");
   }

   public void beginAnchor(String href)
   {
      beginElement("a", "href", href);
   }

   public void beginAnchor(String href, String title)
   {
      beginElement("a",
                   new String[] { "href", "title" },
                   new String[] { href, title });
   }

   public void beginAnchor(String href, String title, String target)
   {
      beginElement("a",
                   new String[] { "href", "title", "target" },
                   new String[] { href, title, target });
   }

   public void endAnchor()
   {
      endElement("a");
   }

   public void anchor(String href, String label)
   {
      beginAnchor(href);
      print(label);
      endAnchor();
   }

   public void anchorName(String name)
   {
      atomicElement("a", new String[] { "name", "id" }, new String[] { name, name });
   }

   public String getPathToRoot()
   {
      return pathToRoot;
   }

   public void beginBody(CssClass cssClass)
   {
      beginBody(cssClass, true);
   }

   public void beginBody(CssClass cssClass, boolean setTitle)
   {
      if (setTitle) {
         beginElement("body",
                      new String[] { "class", "onload" },
                      new String[] { cssClass.getName(), "if(parent.contentPageLoaded)parent.contentPageLoaded(document.title)" }
                      );
      }
      else {
         beginElement("body",
                      new String[] { "class", "onload" },
                      new String[] { cssClass.getName(), "if(parent.contentPageLoaded)parent.contentPageLoaded()" }
                      );
      }
   }

   public void endBody()
   {
      endElement("body");
   }

   public void insert(Reader in)
      throws IOException
   {
      IOToolkit.copyStream(in, out);
   }

   public String createHrefString(String url, String content)
   {
      return createHrefString(url, content, null);
   }

   public String createHrefString(String url, String content, String title)
   {
      StringBuffer result = new StringBuffer();
      result.append("<a href=\"");
      result.append(url);
      result.append("\"");
      if (null != title) {
         result.append(" title=\"");
         result.append(title);
         result.append("\"");
      }
      result.append(">");
      result.append(content);
      result.append("</a>");
      return result.toString();
   }

   public File getFile()
   {
      return this.file;
   }
}

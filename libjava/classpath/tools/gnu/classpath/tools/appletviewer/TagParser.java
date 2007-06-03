/* TagParser.java -- a parser for applet tags
   Copyright (C) 2006  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

package gnu.classpath.tools.appletviewer;

import gnu.javax.swing.text.html.parser.HTML_401F;

import gnu.xml.dom.DomNode;
import gnu.xml.dom.html2.DomHTMLAppletElement;
import gnu.xml.dom.html2.DomHTMLDocument;
import gnu.xml.dom.html2.DomHTMLEmbedElement;
import gnu.xml.dom.html2.DomHTMLObjectElement;
import gnu.xml.dom.html2.DomHTMLParamElement;
import gnu.xml.dom.html2.DomHTMLParser;

import java.io.File;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;

import java.net.MalformedURLException;
import java.net.URL;

import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.Vector;

import org.w3c.dom.NodeList;


public class TagParser
{
    
  /**
   * Parsed document.
   */
  DomHTMLDocument document;
  
  /**
   * The document base of this applet.
   */
  URL documentbase;
  
  /**
   * The document base of all the applets.
   */
  static URL db;
  
  /** 
   * The tags in the document.
   */
  Vector tags = new Vector();
  
  /**
   * Default constructor.
   */
  TagParser()
  {
    // Do nothing.
  }

  /**
   * Constructs and parses document using the given location.
   * 
   * @param location - location of applet
   */
  TagParser(String location) throws IOException
  {
    documentbase = getLocationToURL(location);
    db = documentbase;
    InputStreamReader in = new InputStreamReader(documentbase.openStream());
    document = (DomHTMLDocument) (new DomHTMLParser(HTML_401F.getInstance()).parseDocument(in));
  }

  /**
   * Constructs and parses document.
   * 
   * @param in - Reader to parse document from.
   * @param documentBase - the URL of the applet
   * @throws IOException - is thrown if any IO error occurs.
   */
  TagParser(Reader in, URL documentBase) throws IOException
  {
    documentbase = documentBase;
    db = documentbase;
    document = (DomHTMLDocument) (new DomHTMLParser(HTML_401F.getInstance()).parseDocument(in));
  }
  
  /**
   * Parses all applet tags in document.
   * 
   * @return a list of AppletTag objects representing the applet tags
   * in document
   */
  ArrayList parseAppletTags()
  {    
    ArrayList allTags = new ArrayList();
    if (document == null)
      return null;
    
    recurseDocument(document.getChildNodes());

    int sz = tags.size();
    for (int j = 0; j < sz; j++)
      {
        Object curr = tags.get(j);
        // Order of checking is important here.
        // Must check embed element before applet element
        // because DomHTMLEmbedElement extends DomHTMLAppletElement
        AppletTag a = null;
        if (curr instanceof DomHTMLEmbedElement)
          a = new AppletTag((DomHTMLEmbedElement) curr);
        else if (curr instanceof DomHTMLAppletElement)
          a = new AppletTag((DomHTMLAppletElement) curr);
        else if (curr instanceof DomHTMLObjectElement)
          a = new AppletTag((DomHTMLObjectElement) curr);
        a.documentbase = documentbase;
        allTags.add(a);
      }
    
    return allTags;
  }
  
  /**
   * Recurses the document in search for the appropriate tags.
   * 
   * @param list - the Node list.
   */
  private void recurseDocument(NodeList list)
  {
    // Recurse and store all APPLET, OBJECT and EMBED tags.
    int length = list.getLength();
    for (int i = 0; i < length; i++)
      {
        DomNode curr = (DomNode) list.item(i);
        if ((curr instanceof DomHTMLEmbedElement) || 
            (curr instanceof DomHTMLAppletElement) ||
            (curr instanceof DomHTMLObjectElement))
          tags.add(curr);
        recurseDocument(curr.getChildNodes());
      }
  }
  
  /**
   * Parses the param elements for a given node.
   * 
   * @param node - the node element to parse.
   */
  static void parseParams(DomNode node, AppletTag t)
  {
    boolean ja = false;
    boolean jb = false;
    boolean jc = false;
    NodeList l = node.getChildNodes();
    int size = l.getLength();
    
    if (size != 0)
      for (int i = 0; i < size; i++)
        {
          Object c = l.item(i);
          if (! (c instanceof DomHTMLParamElement))
            continue;
          DomHTMLParamElement curr = (DomHTMLParamElement) c;
          String key = curr.getName();
          String val = curr.getValue();
          
          if (key.equals("java_code"))
            {
              jc = true;
              t.code = val;
            }
          else if (key.equals("java_codebase"))
            {
              jb = true;
              t.codebase = val;
            }
          else if (!jc && key.equals("code"))
            t.code = val;
          else if (!jc && key.equals("classid"))
            {
              int x = val.indexOf(":");
              if (x != -1)
                val = val.substring(x + 1);
              t.code = val;
            }
          else if (!jb && key.equals("codebase"))
            t.codebase = val;
          else if (key.equals("java_archive"))
            {
              ja = true;
              t.archives = parseArchives(val, t);
              val = t.archives.toString();
            }
          else if (!ja && key.equals("archive"))
            {
              t.archives = parseArchives(val, t);
              val = t.archives.toString();
            }
          val = unescapeString(val);
          t.parameters.put(key.toLowerCase(), val);
        }
  }
  
  /**
   * This method does the same thing as the g_strcompress function in glib.
   * 
   * @param value
   * @return value in its original one-byte equivalence.
   */
  private static String unescapeString(String value)
  {
    String unescVal = "";
    for (int i = 0; i < value.length(); i++)
      {
        if (i == value.length() - 1)
          {
            unescVal = unescVal.concat(value.substring(i));
            break;
          }
        if (value.charAt(i) == '\\')
          {
            switch (value.charAt(i + 1))
              {
              case 'b':
                unescVal = unescVal.concat("\b");
                break;
              case 'f':
                unescVal = unescVal.concat("\f");
                break;
              case 'n':
                unescVal = unescVal.concat("\n");
                break;
              case 'r':
                unescVal = unescVal.concat("\r");
                break;
              case 't':
                unescVal = unescVal.concat("\t");
                break;
              case '\\':
                unescVal = unescVal.concat("\\");
                break;
              case '\"':
                unescVal = unescVal.concat("\"");
                break;
              default:
                unescVal = unescVal.concat("\\");
                unescVal = unescVal.concat(value.substring(i + 1, i + 2));
                break;
              }
            i++;
          }
        else
          unescVal = unescVal.concat(value.substring(i, i + 1));
      }
    return unescVal;
  }
  
  /**
   * Parses the archive string and returns a list.
   * 
   * @param the list of archives (comma-separated) in a String.
   */
  static ArrayList parseArchives(String arcs, AppletTag t)
  {
    try
      {
        ArrayList list = new ArrayList();

        StringTokenizer tagTokenizer = new StringTokenizer(arcs, ",");
        while (tagTokenizer.hasMoreTokens())
          list.add(t.prependCodeBase(tagTokenizer.nextToken().trim()));

        return list;
      }
    catch (MalformedURLException e)
      {
      }
    return null;
  }
  
  /**
   * Gets the location to the URL, given a location.
   * 
   * @param location - the given location.
   * @return the URL.
   */
  static URL getLocationToURL(String location) throws IOException
  {
    URL tmpDocumentBase = null;

    try
      {        
        // Try parsing location as a URL.
        tmpDocumentBase = new URL(location);
        
        // If no file was specified in the URL the assume the user
        // meant the root page.
        String f = tmpDocumentBase.getFile();
        if (f.indexOf(".") == -1 && !f.endsWith(File.separator))
          if (new File(tmpDocumentBase.getFile()).isDirectory())
            tmpDocumentBase = new URL(location.concat(File.separator));
      }
    catch (MalformedURLException e)
      {
        // location is not a URL.  See if it is an HTML file.
        String path;

        if (location.startsWith(File.separator))
          path = new File(location).getCanonicalPath();
        else
          path = new File(System.getProperty("user.dir") + File.separator
                          + location).getCanonicalPath();

        tmpDocumentBase = new URL("file", "", path);
        
        if (new File(tmpDocumentBase.getFile()).isDirectory())
          tmpDocumentBase = new URL("file", "", path + File.separator);
      }
    
    return tmpDocumentBase;
  }
}

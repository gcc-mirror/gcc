/* AppletTag.java -- a representation of an HTML APPLET tag
   Copyright (C) 2003, 2004, 2005, 2006  Free Software Foundation, Inc.

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

import gnu.xml.dom.html2.DomHTMLAppletElement;
import gnu.xml.dom.html2.DomHTMLEmbedElement;
import gnu.xml.dom.html2.DomHTMLObjectElement;

import java.awt.Dimension;
import java.awt.Toolkit;

import java.io.File;

import java.net.MalformedURLException;
import java.net.URL;

import java.text.NumberFormat;
import java.text.ParseException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Locale;

/**
 * @author Lillian Angel (langel@redhat.com)
 * @author Thomas Fitzsimmons (fitzsim@redhat.com)
 */
class AppletTag
{

  /**
   * The document base of this applet.
   */
  URL documentbase;

  /**
   * name of applet tag.
   */
  String name = "";

  /**
   * code of applet tag.
   */
  String code = "";

  /**
   * codebase of applet tag.
   */
  String codebase = "";

  /**
   * The archives.
   */
  ArrayList archives = new ArrayList();

  /**
   * The parameters.
   */
  HashMap parameters = new HashMap();

  /**
   * The screen size.
   */
  Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

  /**
   * Default constructor.
   */
  AppletTag()
  {
    // Do nothing.
  }

  /**
   * Constructs an AppletTag and parses the given applet element.
   *
   * @param appElement - the Applet element to parse.
   */
  AppletTag(DomHTMLAppletElement appElement)
  {
    name = appElement.getName();
    parameters.put("name", name);

    parameters.put("object", appElement.getObject());
    parameters.put("align", appElement.getAlign());
    parameters.put("alt", appElement.getAlt());
    parameters.put("height", appElement.getHeight());
    parameters.put("hspace", Integer.toString(appElement.getHspace()));
    parameters.put("vspace", Integer.toString(appElement.getVspace()));
    parameters.put("width", appElement.getWidth());

    TagParser.parseParams(appElement, this);

    if (code.equals(""))
      {
        code = appElement.getCode();
        if (code.equals(""))
          code = appElement.getCls();
      }

    // Must initialize codebase before archives
    if (codebase.equals(""))
      {
        codebase = appElement.getCodeBase();
        if (codebase.equals(""))
          codebase = appElement.getSrc();
      }

    if (archives.size() == 0)
      {
        String arcs = "";
        String arch = appElement.getArchive();

        if (code.indexOf(".") < 0)
          arcs = code + ".jar";

        if (!arch.equals(""))
          arcs += "," + arch;

        if (!arcs.equals(""))
          archives = TagParser.parseArchives(arcs, this);
      }
  }

  /**
   * Constructs an AppletTag and parses the given embed element.
   *
   * @param embElement - the Embed element to parse.
   */
  AppletTag(DomHTMLEmbedElement embElement)
  {
    // In an EMBED tag, a parameter is any non-standard attribute. This
    // is a problem for applets that take parameters named "code",
    // "codebase", "archive", "object", or "type". The solution is to
    // allow the same attributes, prefixed by "java_". The presence of
    // a "java_" attribute indicates that the non-prefixed attribute
    // should be interpreted as a parameter. For example if "java_code"
    // and "code" attributes are present in the EMBED tag then the
    // "code" attribute is interpreted as a parameter.

    name = embElement.getName();
    parameters.put("name", name);

    String jobj = embElement.getJavaObject();
    if (!jobj.equals(""))
      parameters.put("java_object", jobj);
    else
      parameters.put("object", embElement.getObject());

    parameters.put("width", embElement.getWidth());
    parameters.put("height", embElement.getHeight());
    parameters.put("align", embElement.getAlign());
    parameters.put("alt", embElement.getAlt());
    parameters.put("hspace", Integer.toString(embElement.getHspace()));
    parameters.put("mayscript", embElement.getMayscript());
    parameters.put("pluginspage", embElement.getPluginsPage());
    parameters.put("title", embElement.getTitle());
    parameters.put("type", embElement.getType());
    parameters.put("java_type", embElement.getJavaType());
    parameters.put("vspace", Integer.toString(embElement.getVspace()));

    TagParser.parseParams(embElement, this);

    // Must initialize codebase before archives
    if (codebase.equals(""))
      {
        String javacb = embElement.getJavaCodeBase();
        if (!javacb.equals(""))
          codebase = javacb;
        else
          codebase = embElement.getCodeBase();
      }

    if (code.equals(""))
      {
        String jcode = embElement.getJavaCode();
        if (!jcode.equals(""))
          code = jcode;
        else
          code = embElement.getCode();
      }

    if (archives.size() == 0)
      {
        String arcs = "";
        String jarch = embElement.getJavaArchive();
        String arch = embElement.getArchive();

        if (code.indexOf(".") < 0)
          arcs = code + ".jar";

        if (!jarch.equals(""))
          arcs += "," + jarch;
        else if (!arch.equals(""))
          arcs += "," + arch;

        if (!arcs.equals(""))
          archives = TagParser.parseArchives(arcs, this);
      }
  }

  /**
   * Constructs an AppletTag and parses the given object element.
   *
   * @param objElement - the Object element to parse.
   */
  AppletTag(DomHTMLObjectElement objElement)
  {
    // In an OBJECT tag, a parameter is any non-standard attribute. This
    // is a problem for applets that take parameters named "code",
    // "codebase", "archive", "object", or "type". The solution is to
    // allow the same attributes, prefixed by "java_". The presence of
    // a "java_" attribute indicates that the non-prefixed attribute
    // should be interpreted as a parameter. For example if "java_code"
    // and "code" attributes are present in the OBJECT tag then the
    // "code" attribute is interpreted as a parameter.

    name = objElement.getName();
    parameters.put("name", name);

    String jobj = objElement.getJavaObject();
    if (!jobj.equals(""))
      parameters.put("java_object", jobj);
    else
      parameters.put("object", objElement.getObject());

    parameters.put("type", objElement.getType());
    parameters.put("java_type", objElement.getJavaType());
    parameters.put("align", objElement.getAlign());
    parameters.put("codetype", objElement.getCodeType());
    parameters.put("data", objElement.getData());
    parameters.put("declare", Boolean.toString(objElement.getDeclare()));
    parameters.put("height", objElement.getHeight());
    parameters.put("hspace", Integer.toString(objElement.getHspace()));
    parameters.put("border", objElement.getBorder());
    parameters.put("standby", objElement.getStandby());
    parameters.put("tabindex", Integer.toString(objElement.getTabIndex()));
    parameters.put("usemap", objElement.getUseMap());
    parameters.put("vspace", Integer.toString(objElement.getVspace()));
    parameters.put("width", objElement.getWidth());
    parameters.put("mayscript", objElement.getMayscript());
    parameters.put("scriptable", objElement.getScriptable());

    TagParser.parseParams(objElement, this);

    // Must initialize codebase before archives
    if (codebase.equals(""))
      {
        String javacb = objElement.getJavaCodeBase();
        if (! javacb.equals(""))
          codebase = javacb;
        else
          codebase = objElement.getCodeBase();
      }

    if (code.equals(""))
      {
        String jcode = objElement.getJavaCode();
        if (!jcode.equals(""))
          code = jcode;
        else
          code = objElement.getCode();
      }

    if (archives.size() == 0)
      {
        String arcs = "";
        String jarch = objElement.getJavaArchive();
        String arch = objElement.getArchive();

        if (code.indexOf(".") < 0)
          arcs = code + ".jar";

        if (!jarch.equals(""))
          arcs += "," + jarch;
        else if (!arch.equals(""))
          arcs += "," + arch;

        if (!arcs.equals(""))
          archives = TagParser.parseArchives(arcs, this);
      }
  }

  /**
   * String representation of the tag.
   *
   * @return the string representation.
   */
  public String toString()
  {
    return ("  name=" + name + "\n" + "  code=" + code + "\n" + "  codebase="
            + codebase + "\n" + "  archive=" + archives + "\n" + "  parameters="
            + parameters + "\n" + "  documentbase=" + documentbase + "\n");
  }

  /**
   * Returns the size of the applet.
   *
   * @return the size.
   */
  Dimension getSize()
  {
    Dimension size = new Dimension(320, 200);

    try
      {
        String widthStr = (String) parameters.get("width");

        if (widthStr != null && ! widthStr.equals(""))
          {
            if (widthStr.charAt(widthStr.length() - 1) == '%')
              {
                double p = NumberFormat.getPercentInstance(Locale.US).parse(widthStr).intValue() / 100.0;
                size.width = (int)(p * screenSize.width);
              }
            else
              size.width = NumberFormat.getInstance(Locale.US).parse(widthStr).intValue();
          }
      }
    catch (ParseException e)
      {
        // Use default.
      }

    try
      {
        String heightStr = (String) parameters.get("height");

        if (heightStr != null && !heightStr.equals(""))
          {
            if (heightStr.charAt(heightStr.length() - 1) == '%')
              {
                double p = NumberFormat.getPercentInstance(Locale.US).parse(heightStr).intValue() / 100.0;
                size.height = (int) (p * screenSize.height);
              }
            else
              size.height = NumberFormat.getInstance(Locale.US).parse(heightStr).intValue();
          }
      }
    catch (ParseException e)
      {
        // Use default.
      }

    return size;
  }

  /**
   * Gets the code base.
   *
   * @return the codebase.
   */
  String getCodeBase()
  {
    return codebase;
  }

  /**
   * Gets the archive list.
   *
   * @return the archive list.
   */
  ArrayList getArchives()
  {
    return archives;
  }

  /**
   * Gets the code.
   *
   * @return the code.
   */
  String getCode()
  {
    return code;
  }

  /**
   * Gets the document base.
   *
   * @return the document base.
   */
  URL getDocumentBase()
  {
    return documentbase;
  }

  /**
   * Gets the specified parameter.
   *
   * @param name - the specified parameter.
   * @return the parameter.
   */
  String getParameter(String name)
  {
    return (String) parameters.get(name.toLowerCase());
  }

  /**
   * Prepends the base to the codebase.
   *
   * @return the new URL.
   */
  URL prependCodeBase(String base) throws MalformedURLException
  {
    if (documentbase == null)
      documentbase = TagParser.db;

    URL fullcodebase;

    //If no codebase was specified, default to documentbase.
    if (codebase.equals(""))
      {
        if (documentbase.getFile().endsWith(File.separator))
          fullcodebase = documentbase;
        else
          {
            String dirname = documentbase.getFile();
            if (dirname.indexOf(".") < 0)
              fullcodebase = new URL(documentbase + File.separator);
            else
              {
                // Determine dirname for file by stripping everything
                // past the last file separator.
                dirname = dirname.substring(0,
                                            dirname.lastIndexOf(File.separatorChar) + 1);

                fullcodebase = new URL(documentbase.getProtocol(),
                                       documentbase.getHost(),
                                       documentbase.getPort(), dirname);
              }
          }
      }
    else
      {
        // codebase was specified.
        URL codebaseURL = new URL(documentbase, codebase);

        if ("file".equals(codebaseURL.getProtocol()))
          {
            if (new File(codebaseURL.getFile()).isDirectory() && !codebase.endsWith(File.separator))
              fullcodebase = new URL(documentbase, codebase + File.separator);
            else
              fullcodebase = new URL(documentbase, codebase);
          }
        else if (codebase.endsWith(File.separator))
          fullcodebase = new URL(documentbase, codebase);
        else
          fullcodebase = new URL(documentbase, codebase + File.separator);
      }

    return new URL(fullcodebase, base);
  }
}

/* DomHTMLObjectElement.java -- 
   Copyright (C) 2005 Free Software Foundation, Inc.

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

package gnu.xml.dom.html2;

import org.w3c.dom.Document;
import org.w3c.dom.html2.HTMLFormElement;
import org.w3c.dom.html2.HTMLObjectElement;

/**
 * An HTML 'OBJECT' element node.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomHTMLObjectElement
  extends DomHTMLElement
  implements HTMLObjectElement
{

  protected DomHTMLObjectElement(DomHTMLDocument owner, String namespaceURI,
                                 String name)
  {
    super(owner, namespaceURI, name);
  }

  public HTMLFormElement getForm()
  {
    return (HTMLFormElement) getParentElement("form");
  }

  public String getCode()
  {
    return getHTMLAttribute("code");
  }

  public void setCode(String code)
  {
    setHTMLAttribute("code", code);
  }
  
  public String getJavaCode()
  {
    return getHTMLAttribute("java_code");
  }

  public void setJavaCode(String code)
  {
    setHTMLAttribute("java_code", code);
  }
  
  public String getObject()
  {
    return getHTMLAttribute("object");
  }

  public void setObject(String obj)
  {
    setHTMLAttribute("object", obj);
  }
  
  public String getJavaObject()
  {
    return getHTMLAttribute("java_object");
  }

  public void setJavaObject(String obj)
  {
    setHTMLAttribute("java_object", obj);
  }
  
  public String getAlign()
  {
    return getHTMLAttribute("align");
  }

  public void setAlign(String align)
  {
    setHTMLAttribute("align", align);
  }
  
  public String getArchive()
  {
    return getHTMLAttribute("archive");
  }

  public void setArchive(String archive)
  {
    setHTMLAttribute("archive", archive);
  }
  
  public String getJavaArchive()
  {
    return getHTMLAttribute("java_archive");
  }

  public void setJavaArchive(String archive)
  {
    setHTMLAttribute("java_archive", archive);
  }
  
  public String getBorder()
  {
    return getHTMLAttribute("border");
  }

  public void setBorder(String border)
  {
    setHTMLAttribute("border", border);
  }
  
  public String getCodeBase()
  {
    return getHTMLAttribute("codebase");
  }

  public void setCodeBase(String codeBase)
  {
    setHTMLAttribute("codebase", codeBase);
  }
  
  public String getJavaCodeBase()
  {
    return getHTMLAttribute("java_codebase");
  }

  public void setJavaCodeBase(String codeBase)
  {
    setHTMLAttribute("java_codebase", codeBase);
  }
  
  public String getCodeType()
  {
    return getHTMLAttribute("codetype");
  }

  public void setCodeType(String codeType)
  {
    setHTMLAttribute("codetype", codeType);
  }
  
  public String getData()
  {
    return getHTMLAttribute("data");
  }

  public void setData(String data)
  {
    setHTMLAttribute("data", data);
  }
  
  public boolean getDeclare()
  {
    return getBooleanHTMLAttribute("declare");
  }

  public void setDeclare(boolean declare)
  {
    setBooleanHTMLAttribute("declare", declare);
  }
  
  public String getHeight()
  {
    return getHTMLAttribute("height");
  }

  public void setHeight(String height)
  {
    setHTMLAttribute("height", height);
  }
  
  public int getHspace()
  {
    return getIntHTMLAttribute("hspace");
  }

  public void setHspace(int hspace)
  {
    setIntHTMLAttribute("hspace", hspace);
  }
  
  public String getName()
  {
    return getHTMLAttribute("name");
  }

  public void setName(String name)
  {
    setHTMLAttribute("name", name);
  }
  
  public String getStandby()
  {
    return getHTMLAttribute("standby");
  }

  public void setStandby(String standby)
  {
    setHTMLAttribute("standby", standby);
  }
  
  public int getTabIndex()
  {
    return getIntHTMLAttribute("tabindex");
  }

  public void setTabIndex(int tabIndex)
  {
    setIntHTMLAttribute("tabindex", tabIndex);
  }
  
  public String getType()
  {
    return getHTMLAttribute("type");
  }

  public void setType(String type)
  {
    setHTMLAttribute("type", type);
  }
  
  public String getJavaType()
  {
    return getHTMLAttribute("java_type");
  }

  public void setJavaType(String type)
  {
    setHTMLAttribute("java_type", type);
  }
  
  public String getUseMap()
  {
    return getHTMLAttribute("usemap");
  }

  public void setUseMap(String useMap)
  {
    setHTMLAttribute("usemap", useMap);
  }
  
  public int getVspace()
  {
    return getIntHTMLAttribute("vspace");
  }

  public void setVspace(int vspace)
  {
    setIntHTMLAttribute("vspace", vspace);
  }
  
  public String getWidth()
  {
    return getHTMLAttribute("width");
  }

  public void setWidth(String width)
  {
    setHTMLAttribute("width", width);
  }

  public Document getContentDocument()
  {
    // TODO getContentDocument
    return null;
  }
  
  public void setMayscript(String may)
  {
    setHTMLAttribute("mayscript", may);
  }
  
  public String getMayscript()
  {
    return getHTMLAttribute("mayscript");
  }
  
  public void setScriptable(String scr)
  {
    setHTMLAttribute("scriptable", scr);
  }
  
  public String getScriptable()
  {
    return getHTMLAttribute("scriptable");
  }
}


/* DomLSInput.java -- 
   Copyright (C) 1999,2000,2001 Free Software Foundation, Inc.

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

package gnu.xml.dom.ls;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.w3c.dom.ls.LSInput;

/**
 * Specification of XML input to parse.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomLSInput
  implements LSInput
{

  private InputStream in;
  private String systemId;
  private String publicId;
  private String baseURI;
  private String encoding;
  private boolean certifiedText;

  public Reader getCharacterStream()
  {
    return new InputStreamReader(in);
  }

  public void setCharacterStream(Reader characterStream)
  {
    in = new ReaderInputStream(characterStream);
  }

  public InputStream getByteStream()
  {
    return in;
  }

  public void setByteStream(InputStream byteStream)
  {
    in = byteStream;
  }

  public String getStringData()
  {
    StringBuffer acc = new StringBuffer();
    Reader reader = getCharacterStream();
    try
      {
        char[] buf = new char[4096];
        for (int len = reader.read(buf); len != -1; len = reader.read(buf))
          {
            acc.append(buf, 0, len);
          }
      }
    catch (IOException e)
      {
        return null; // ?
      }
    return acc.toString();
  }

  public void setStringData(String stringData)
  {
    in = new ReaderInputStream(new StringReader(stringData));
  }

  public String getSystemId()
  {
    return systemId;
  }

  public void setSystemId(String systemId)
  {
    this.systemId = systemId;
  }

  public String getPublicId()
  {
    return publicId;
  }

  public void setPublicId(String publicId)
  {
    this.publicId = publicId;
  }

  public String getBaseURI()
  {
    return baseURI;
  }

  public void setBaseURI(String baseURI)
  {
    this.baseURI = baseURI;
  }

  public String getEncoding()
  {
    return encoding;
  }

  public void setEncoding(String encoding)
  {
    this.encoding = encoding;
  }

  public boolean getCertifiedText()
  {
    return certifiedText;
  }

  public void setCertifiedText(boolean certifiedText)
  {
    this.certifiedText = certifiedText;
  }
  
}


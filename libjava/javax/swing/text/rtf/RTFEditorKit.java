/* RTFEditorKit.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package javax.swing.text.rtf;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.StyledEditorKit;

/**
 * Provides support for RTF data for use in
 * {@link javax.swing.JEditorPane}s.
 *
 * @author Roman Kennke (roman@ontographics.com)
 */
public class RTFEditorKit
  extends StyledEditorKit
{

  /**
   * Constructs a new RTFEditorKit.
   */
  public RTFEditorKit()
  {
    super();
  }

  /**
   * Returns the MIME content type. In the case of RTFEditorKit this is
   * &apos;text/rtf&apos;
   *
   * @return the MIME content type for RTFEditorKit
   */
  public String getContentType()
  {
    return "text/rtf";
  }

  /**
   * Reads RTF data from <code>stream</code> into <code>doc</code> at the
   * specified position <code>pos</code>.
   *
   * @param stream the {@link InputStream} from where we read RTF data
   * @param doc the {@link Document} into which we read the RTF data
   * @param pos the position where to start
   *
   * @throws IOException if an IO error occurs
   * @throws BadLocationException if the position is not valid
   */
  public void read(InputStream stream, Document doc, int pos)
    throws IOException, BadLocationException
  {
    RTFParser parser = new RTFParser(stream, doc, pos);
    parser.parse();
  }


  /**
   * Reads RTF data from <code>reader</code> into <code>doc</code> at the
   * specified position <code>pos</code>.
   *
   * @param reader the {@link Reader} from where we read RTF data
   * @param doc the {@link Document} into which we read the RTF data
   * @param pos the position where to start
   *
   * @throws IOException if an IO error occurs
   * @throws BadLocationException if the position is not valid
   */
  public void read(Reader reader, Document doc, int pos)
    throws IOException, BadLocationException
  {
    RTFParser parser = new RTFParser(reader, doc, pos);
    parser.parse();
  }
}

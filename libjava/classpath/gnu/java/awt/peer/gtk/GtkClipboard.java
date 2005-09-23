/* GtkClipboard.java
   Copyright (C) 1999, 2005  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Image;
import java.awt.datatransfer.*;

import java.io.*;

import java.util.List;
import java.util.Iterator;

public class GtkClipboard extends Clipboard
{

  // Given to the native side so it can signal special targets that
  // can be converted to one of the special predefined DataFlavors.
  static final String stringMimeType;
  static final String imageMimeType;
  static final String filesMimeType;

  // Indicates whether the results of the clipboard selection can be
  // cached by GtkSelection. True if
  // gdk_display_supports_selection_notification.
  static final boolean canCache;

  static
  {
    stringMimeType = DataFlavor.stringFlavor.getMimeType();
    imageMimeType = DataFlavor.imageFlavor.getMimeType();
    filesMimeType = DataFlavor.javaFileListFlavor.getMimeType();

    canCache = initNativeState(stringMimeType, imageMimeType, filesMimeType);
  }

  /**
   * The one and only gtk+ clipboard instance.
   */
  private static GtkClipboard instance = new GtkClipboard();

  /**
   * Creates the clipboard and sets the initial contents to the
   * current gtk+ selection.
   */
  private GtkClipboard()
  {
    super("System Clipboard");
    setContents(new GtkSelection(), null);
  }

  /**
   * Returns the one and only GtkClipboard instance.
   */

  static GtkClipboard getInstance()
  {
    return instance;
  }

  /**
   * Sets the GtkSelection facade as new contents of the clipboard.
   * Called from gtk+ when another application grabs the clipboard and
   * we loose ownership.
   */
  private static void setSystemContents()
  {
    GtkClipboardNotifier.announce();
  }

  /**
   * Sets the new contents and advertises the available flavors to the
   * gtk+ clipboard.
   */
  public synchronized void setContents(Transferable contents,
				       ClipboardOwner owner)
  {
    super.setContents(contents, owner);

    if (contents == null)
      {
	advertiseContent(null, false, false, false);
	return;
      }

    // We don't need to do anything for a GtkSelection facade.
    if (contents instanceof GtkSelection)
      return;

    boolean text = false;
    boolean images = false;
    boolean files = false;

    if (contents instanceof StringSelection
	|| contents.isDataFlavorSupported(DataFlavor.stringFlavor)
	|| contents.isDataFlavorSupported(DataFlavor.plainTextFlavor)
	|| contents.isDataFlavorSupported(DataFlavor
					  .getTextPlainUnicodeFlavor()))
      text = true;

    DataFlavor[] flavors = contents.getTransferDataFlavors();
    String[] mimeTargets = new String[flavors.length];
    for (int i = 0; i < flavors.length; i++)
      {
	DataFlavor flavor = flavors[i];
	String mimeType = flavor.getMimeType();
	mimeTargets[i] = mimeType;

	if (! text)
	  if ("text".equals(flavor.getPrimaryType())
	      || flavor.isRepresentationClassReader())
	    text = true;

	// XXX - We only support automatic image conversion for
	// GtkImages at the moment. So explicitly check that we have
	// one.
	if (! images && flavors[i].equals(DataFlavor.imageFlavor))
	  {
	    try
	      {
		Object o = contents.getTransferData(DataFlavor.imageFlavor);
		if (o instanceof GtkImage)
		  images = true;
	      }
	    catch (UnsupportedFlavorException ufe)
	      {
	      }
	    catch (IOException ioe)
	      {
	      }
	    catch (ClassCastException cce)
	      {
	      }
	  }

	if (flavors[i].equals(DataFlavor.javaFileListFlavor))
	  files = true;
      }

    advertiseContent(mimeTargets, text, images, files);
  }

  /**
   * Advertises new contents to the gtk+ clipboard given a string
   * array of (mime-type) targets. When the boolean flags text, images
   * and/or files are set then gtk+ is asked to also advertise the
   * availability of any text, image or uri/file content types it
   * supports. If targets is null (and all flags false) then the
   * selection has explicitly been erased.
   */
  private native void advertiseContent(String[] targets,
				       boolean text,
				       boolean images,
				       boolean files);
  
  /**
   * Called by the gtk+ clipboard when an application has requested
   * text.  Return a string representing the current clipboard
   * contents or null when no text can be provided.
   */
  private String provideText()
  {
    Transferable contents = this.contents;
    if (contents == null || contents instanceof GtkSelection)
      return null;

    // Handle StringSelection special since that is just pure text.
    if (contents instanceof StringSelection)
      {
        try
          {
            return (String) contents.getTransferData(DataFlavor.stringFlavor);
	  }
        catch (UnsupportedFlavorException ufe)
          {
          }
        catch (IOException ioe)
          {
          }
        catch (ClassCastException cce)
          {
          }
      }

    // Try to get a plain text reader for the current contents and
    // turn the result into a string.
    try
      {
	DataFlavor plainText = DataFlavor.getTextPlainUnicodeFlavor();
	Reader r = plainText.getReaderForText(contents);
	if (r != null)
	  {
	    StringBuffer sb = new StringBuffer();
	    char[] cs = new char[1024];
	    int l = r.read(cs);
	    while (l != -1)
	      {
		sb.append(cs, 0, l);
		l = r.read(cs);
	      }
	    return sb.toString();
	  }
      }
    catch (IllegalArgumentException iae)
      {
      }
    catch (UnsupportedEncodingException iee)
      {
      }
    catch (UnsupportedFlavorException ufe)
      {
      }
    catch (IOException ioe)
      {
      }

    return null;
  }

  /**
   * Called by the gtk+ clipboard when an application has requested an
   * image.  Returns a GtkImage representing the current clipboard
   * contents or null when no image can be provided.
   */
  private GtkImage provideImage()
  {
    Transferable contents = this.contents;
    if (contents == null || contents instanceof GtkSelection)
      return null;

    try
      {
	return (GtkImage) contents.getTransferData(DataFlavor.imageFlavor);
      }
    catch (UnsupportedFlavorException ufe)
      {
      }
    catch (IOException ioe)
      {
      }
    catch (ClassCastException cce)
      {
      }

    return null;
  }

  /**
   * Called by the gtk+ clipboard when an application has requested a
   * uri-list.  Return a string array containing the URIs representing
   * the current clipboard contents or null when no URIs can be
   * provided.
   */
  private String[] provideURIs()
  {
    Transferable contents = this.contents;
    if (contents == null || contents instanceof GtkSelection)
      return null;

    try
      {
	List list = (List) contents.getTransferData
	  (DataFlavor.javaFileListFlavor);
	String[] uris = new String[list.size()];
	int u = 0;
	Iterator it = list.iterator();
	while (it.hasNext())
	  uris[u++] = ((File) it.next()).toURI().toString();
	return uris;
      }
    catch (UnsupportedFlavorException ufe)
      {
      }
    catch (IOException ioe)
      {
      }
    catch (ClassCastException cce)
      {
      }

    return null;
  }

  /**
   * Called by gtk+ clipboard when an application requests the given
   * target mime-type. Returns a byte array containing the requested
   * data, or null when the contents cannot be provided in the
   * requested target mime-type. Only called after any explicit text,
   * image or file/uri requests have been handled earlier and failed.
   */
  private byte[] provideContent(String target)
  {
    // Sanity check. The callback could be triggered just after we
    // changed the clipboard.
    Transferable contents = this.contents;
    if (contents == null || contents instanceof GtkSelection)
      return null;

    // XXX - We are being called from a gtk+ callback. Which means we
    // should return as soon as possible and not call arbitrary code
    // that could deadlock or go bonkers. But we don't really know
    // what DataTransfer contents object we are dealing with. Same for
    // the other provideXXX() methods.
    try
      {
	DataFlavor flavor = new DataFlavor(target);
	Object o = contents.getTransferData(flavor);

	if (o instanceof byte[])
	  return (byte[]) o;

	if (o instanceof InputStream)
	  {
	    InputStream is = (InputStream) o;
	    ByteArrayOutputStream baos = new ByteArrayOutputStream();
	    byte[] bs = new byte[1024];
	    int l = is.read(bs);
	    while (l != -1)
	      {
		baos.write(bs, 0, l);
		l = is.read(bs);
	      }
	    return baos.toByteArray();
	  }

	if (o instanceof Serializable)
	  {
	    ByteArrayOutputStream baos = new ByteArrayOutputStream();
	    ObjectOutputStream oos = new ObjectOutputStream(baos);
	    oos.writeObject(o);
	    oos.close();
	    return baos.toByteArray();
	  }
      }
    catch (ClassNotFoundException cnfe)
      {
      }
    catch (UnsupportedFlavorException ufe)
      {
      }
    catch (IOException ioe)
      {
      }
    catch (ClassCastException cce)
      {
      }

    return null;
  }

  /**
   * Initializes the gtk+ clipboard and caches any native side
   * structures needed. Returns whether or not the contents of the
   * Clipboard can be cached (gdk_display_supports_selection_notification).
   */
  private static native boolean initNativeState(String stringTarget,
						String imageTarget,
						String filesTarget);
}

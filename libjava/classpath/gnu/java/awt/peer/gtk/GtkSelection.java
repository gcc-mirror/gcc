/* GtkClipboard.java - Class representing gtk+ clipboard selection.
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


package gnu.java.awt.peer.gtk;

import gnu.classpath.Pointer;

import java.awt.datatransfer.*;

import java.io.*;
import java.net.*;
import java.util.*;

import java.awt.Image;

/**
 * Class representing the gtk+ clipboard selection. This is used when
 * another program owns the clipboard. Whenever the system clipboard
 * selection changes we create a new instance to notify the program
 * that the available flavors might have changed. When requested it
 * (lazily) caches the targets, and (text, image, or files/uris)
 * clipboard contents.
 */
public class GtkSelection implements Transferable
{
  /**
   * Static lock used for requests of mimetypes and contents retrieval.
   */
  static private Object requestLock = new Object();

  /**
   * Whether we belong to the Clipboard (true) or to the Primary selection.
   */
  private final boolean clipboard;

  /**
   * Whether a request for mimetypes, text, images, uris or byte[] is
   * currently in progress. Should only be tested or set with
   * requestLock held. When true no other requests should be made till
   * it is false again.
   */
  private boolean requestInProgress;

  /**
   * Indicates a requestMimeTypes() call was made and the
   * corresponding mimeTypesAvailable() callback was triggered.
   */
  private boolean mimeTypesDelivered;

  /**
   * Set and returned by getTransferDataFlavors. Only valid when
   * mimeTypesDelivered is true.
   */
  private DataFlavor[] dataFlavors;
  
  /**
   * Indicates a requestText() call was made and the corresponding
   * textAvailable() callback was triggered.
   */
  private boolean textDelivered;

  /**
   * Set as response to a requestText() call and possibly returned by
   * getTransferData() for text targets. Only valid when textDelivered
   * is true.
   */
  private String text;
  
  /**
   * Indicates a requestImage() call was made and the corresponding
   * imageAvailable() callback was triggered.
   */
  private boolean imageDelivered;

  /**
   * Set as response to a requestImage() call and possibly returned by
   * getTransferData() for image targets. Only valid when
   * imageDelivered is true and image is null.
   */
  private Pointer imagePointer;

  /**
   * Cached image value. Only valid when imageDelivered is
   * true. Created from imagePointer.
   */
  private Image image;

  /**
   * Indicates a requestUris() call was made and the corresponding
   * urisAvailable() callback was triggered.
   */
  private boolean urisDelivered;

  /**
   * Set as response to a requestURIs() call. Only valid when
   * urisDelivered is true
   */
  private List<File> uris;

  /**
   * Indicates a requestBytes(String) call was made and the
   * corresponding bytesAvailable() callback was triggered.
   */
  private boolean bytesDelivered;

  /**
   * Set as response to a requestBytes(String) call. Only valid when
   * bytesDelivered is true.
   */
  private byte[] bytes;

  /**
   * Should only be created by the GtkClipboard class. The clipboard
   * should be either GtkClipboard.clipboard or
   * GtkClipboard.selection.
   */
  GtkSelection(GtkClipboard clipboard)
  {
    this.clipboard = (clipboard == GtkClipboard.clipboard);
  }

  /**
   * Gets an array of mime-type strings from the gtk+ clipboard and
   * transforms them into an array of DataFlavors.
   */
  public DataFlavor[] getTransferDataFlavors()
  {
    DataFlavor[] result;
    synchronized (requestLock)
      {
        // Did we request already and cache the result?
        if (mimeTypesDelivered)
          result = (DataFlavor[]) dataFlavors.clone();
        else
          {
            // Wait till there are no pending requests.
            while (requestInProgress)
              {
                try
                  {
                    requestLock.wait();
                  }
                catch (InterruptedException ie)
                  {
                    // ignored
                  }
              }

            // If nobody else beat us and cached the result we try
            // ourselves to get it.
            if (! mimeTypesDelivered)
              {
                requestInProgress = true;
                requestMimeTypes(clipboard);
                while (! mimeTypesDelivered)
                  {
                    try
                      {
                        requestLock.wait();
                      }
                    catch (InterruptedException ie)
                      {
                        // ignored
                      }
                  }
                requestInProgress = false;
              }
            result = dataFlavors;
            if (! GtkClipboard.canCache)
              {
                dataFlavors = null;
                mimeTypesDelivered = false;
              }
            requestLock.notifyAll();
          }
      }
    return result;
  }

  /**
   * Callback that sets the available DataFlavors[]. Note that this
   * should not call any code that could need the main gdk lock.
   */
  private void mimeTypesAvailable(String[] mimeTypes)
  {
    synchronized (requestLock)
      {
        if (mimeTypes == null)
          dataFlavors = new DataFlavor[0];
        else
          {
            // Most likely the mimeTypes include text in which case we add an
            // extra element.
            ArrayList<DataFlavor> flavorsList =
              new ArrayList<DataFlavor>(mimeTypes.length + 1);
            
            for (int i = 0; i < mimeTypes.length; i++)
              {
                try
                  {
                    if (mimeTypes[i] == GtkClipboard.stringMimeType)
                      {
                        // XXX - Fix DataFlavor.getTextPlainUnicodeFlavor()
                        // and also add it to the list.
                        flavorsList.add(DataFlavor.stringFlavor);
                        flavorsList.add(DataFlavor.plainTextFlavor);
                      }
                    else if (mimeTypes[i] == GtkClipboard.imageMimeType)
                      flavorsList.add(DataFlavor.imageFlavor);
                    else if (mimeTypes[i] == GtkClipboard.filesMimeType)
                      flavorsList.add(DataFlavor.javaFileListFlavor);
                    else
                      {
                        // We check the target to prevent duplicates
                        // of the "magic" targets above.
                        DataFlavor target = new DataFlavor(mimeTypes[i]);
                        if (! flavorsList.contains(target))
                          flavorsList.add(target);
                      }
                  }
                catch (ClassNotFoundException cnfe)
                  {
                    cnfe.printStackTrace();
                  }
                catch (NullPointerException npe)
                  {
                    npe.printStackTrace();
                  }
              }
	    
            dataFlavors = new DataFlavor[flavorsList.size()];
            flavorsList.toArray(dataFlavors);
          }

        mimeTypesDelivered = true;
        requestLock.notifyAll();
      }
  }

  /**
   * Gets the available data flavors for this selection and checks
   * that at least one of them is equal to the given DataFlavor.
   */
  public boolean isDataFlavorSupported(DataFlavor flavor)
  {
    DataFlavor[] dfs = getTransferDataFlavors();
    for (int i = 0; i < dfs.length; i++)
      if (flavor.equals(dfs[i]))
        return true;

    return false;
  }

  /**
   * Helper method that tests whether we already have the text for the
   * current gtk+ selection on the clipboard and if not requests it
   * and waits till it is available.
   */
  private String getText()
  {
    String result;
    synchronized (requestLock)
      {
        // Did we request already and cache the result?
        if (textDelivered)
          result = text;
        else
          {
            // Wait till there are no pending requests.
            while (requestInProgress)
              {
                try
                  {
                    requestLock.wait();
                  }
                catch (InterruptedException ie)
                  {
                    // ignored
                  }
              }

            // If nobody else beat us we try ourselves to get and
            // caching the result.
            if (! textDelivered)
              {
                requestInProgress = true;
                requestText(clipboard);
                while (! textDelivered)
                  {
                    try
                      {
                        requestLock.wait();
                      }
                    catch (InterruptedException ie)
                      {
                        // ignored
                      }
                  }
                requestInProgress = false;
              }
            result = text;
            if (! GtkClipboard.canCache)
              {
                text = null;
                textDelivered = false;
              }
            requestLock.notifyAll();
          }
      }
    return result;
  }

  /**
   * Callback that sets the available text on the clipboard. Note that
   * this should not call any code that could need the main gdk lock.
   */
  private void textAvailable(String text)
  {
    synchronized (requestLock)
      {
        this.text = text;
        textDelivered = true;
        requestLock.notifyAll();
      }
  }

  /**
   * Helper method that tests whether we already have an image for the
   * current gtk+ selection on the clipboard and if not requests it
   * and waits till it is available.
   */
  private Image getImage()
  {
    Image result;
    synchronized (requestLock)
      {
        // Did we request already and cache the result?
        if (imageDelivered)
          result = image;
        else
          {
            // Wait till there are no pending requests.
            while (requestInProgress)
              {
                try
                  {
                    requestLock.wait();
                  }
                catch (InterruptedException ie)
                  {
                    // ignored
                  }
              }

            // If nobody else beat us we try ourselves to get and
            // caching the result.
            if (! imageDelivered)
              {
                requestInProgress = true;
                requestImage(clipboard);
                while (! imageDelivered)
                  {
                    try
                      {
                        requestLock.wait();
                      }
                    catch (InterruptedException ie)
                      {
                        // ignored
                      }
                  }
                requestInProgress = false;
              }
            
            if (imagePointer != null)
              image = new GtkImage(imagePointer);
            
            imagePointer = null;
            result = image;
            if (! GtkClipboard.canCache)
              {
                image = null;
                imageDelivered = false;
              }
            requestLock.notifyAll();
          }
      }
    return result;
  }

  /**
   * Callback that sets the available image on the clipboard. Note
   * that this should not call any code that could need the main gdk
   * lock. Note that we get a Pointer to a GdkPixbuf which we cannot
   * turn into a real GtkImage at this point. That will be done on the
   * "user thread" in getImage().
   */
  private void imageAvailable(Pointer pointer)
  {
    synchronized (requestLock)
      {
        this.imagePointer = pointer;
        imageDelivered = true;
        requestLock.notifyAll();
      }
  }

  /**
   * Helper method that test whether we already have a list of
   * URIs/Files and if not requests them and waits till they are
   * available.
   */
  private List<File> getURIs()
  {
    List<File> result;
    synchronized (requestLock)
      {
        // Did we request already and cache the result?
        if (urisDelivered)
          result = uris;
        else
          {
            // Wait till there are no pending requests.
            while (requestInProgress)
              {
                try
                  {
                    requestLock.wait();
                  }
                catch (InterruptedException ie)
                  {
                    // ignored
                  }
              }

            // If nobody else beat us we try ourselves to get and
            // caching the result.
            if (! urisDelivered)
              {
                requestInProgress = true;
                requestURIs(clipboard);
                while (! urisDelivered)
                  {
                    try
                      {
                        requestLock.wait();
                      }
                    catch (InterruptedException ie)
                      {
                        // ignored
                      }
                  }
                requestInProgress = false;
              }
            result = uris;
            if (! GtkClipboard.canCache)
              {
                uris = null;
                urisDelivered = false;
              }
            requestLock.notifyAll();
          }
      }
    return result;
  }

  /**
   * Callback that sets the available File list. Note that this should
   * not call any code that could need the main gdk lock.
   */
  private void urisAvailable(String[] uris)
  {
    synchronized (requestLock)
      {
        if (uris != null && uris.length != 0)
          {
            ArrayList<File> list = new ArrayList<File>(uris.length);
            for (int i = 0; i < uris.length; i++)
              {
                try
                  {
                    URI uri = new URI(uris[i]);
                    if (uri.getScheme().equals("file"))
                      list.add(new File(uri));
                  }
                catch (URISyntaxException use)
                  {
                  }
              }
            this.uris = list;
          }

        urisDelivered = true;
        requestLock.notifyAll();
      }
  }

  /**
   * Helper method that requests a byte[] for the given target
   * mime-type flavor and waits till it is available. Note that unlike
   * the other get methods this one doesn't cache the result since
   * there are possibly many targets.
   */
  private byte[] getBytes(String target)
  {
    byte[] result;
    synchronized (requestLock)
      {
        // Wait till there are no pending requests.
        while (requestInProgress)
          {
            try
              {
                requestLock.wait();
              }
            catch (InterruptedException ie)
              {
                // ignored
              }
          }

        // Request bytes and wait till they are available.
        requestInProgress = true;
        requestBytes(clipboard, target);
        while (! bytesDelivered)
          {
            try
              {
                requestLock.wait();
              }
            catch (InterruptedException ie)
              {
                // ignored
              }
          }
        result = bytes;
        bytes = null;
        bytesDelivered = false;
        requestInProgress = false;
	
        requestLock.notifyAll();
      }
    return result;
  }

  /**
   * Callback that sets the available byte array on the
   * clipboard. Note that this should not call any code that could
   * need the main gdk lock.
   */
  private void bytesAvailable(byte[] bytes)
  {
    synchronized (requestLock)
      {
        this.bytes = bytes;
        bytesDelivered = true;
        requestLock.notifyAll();
      }
  }

  public Object getTransferData(DataFlavor flavor)
    throws UnsupportedFlavorException
  {
    // Note the fall throughs for the "magic targets" if they fail we
    // try one more time through getBytes().
    if (flavor.equals(DataFlavor.stringFlavor))
      {
        String text = getText();
        if (text != null)
          return text;
      }

    if (flavor.equals(DataFlavor.plainTextFlavor))
      {
        String text = getText();
        if (text != null)
          return new StringBufferInputStream(text);
      }

    if (flavor.equals(DataFlavor.imageFlavor))
      {
        Image image = getImage();
        if (image != null)
          return image;
      }

    if (flavor.equals(DataFlavor.javaFileListFlavor))
      {
        List<File> uris = getURIs();
        if (uris != null)
          return uris;
      }

    byte[] bytes = getBytes(flavor.getMimeType());
    if (bytes == null)
      throw new UnsupportedFlavorException(flavor);

    if (flavor.isMimeTypeSerializedObject())
      {
        try
          {
            ByteArrayInputStream bais = new ByteArrayInputStream(bytes);
            ObjectInputStream ois = new ObjectInputStream(bais);
            return ois.readObject();
          }
        catch (IOException ioe)
          {
            ioe.printStackTrace();
          }
        catch (ClassNotFoundException cnfe)
          {
            cnfe.printStackTrace();
          }
      }

    if (flavor.isRepresentationClassInputStream())
      return new ByteArrayInputStream(bytes);

    // XXX, need some more conversions?

    throw new UnsupportedFlavorException(flavor);
  }

  /*
   * Requests text, Image or an byte[] for a particular target from the
   * other application. These methods return immediately. When the
   * content is available the contentLock will be notified through
   * textAvailable, imageAvailable, urisAvailable or bytesAvailable and the
   * appropriate field is set.
   * The clipboard argument is true if we want the Clipboard, and false
   * if we want the (primary) selection.
   */
  private native void requestText(boolean clipboard);
  private native void requestImage(boolean clipboard);
  private native void requestURIs(boolean clipboard);
  private native void requestBytes(boolean clipboard, String target);

  /* Similar to the above but for requesting the supported targets. */
  private native void requestMimeTypes(boolean clipboard);
}

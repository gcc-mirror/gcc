/* DataHandler.java -- Handler for data available in multiple formats.
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

package javax.activation;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.net.URL;

/**
 * Handler for data available in multiple sources and formats.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.1
 */
public class DataHandler
  implements Transferable
{

  private static final DataFlavor[] NO_FLAVORS = new DataFlavor[0];
  private static DataContentHandlerFactory factory = null;

  private final DataSource dataSource;
  private DataSource objDataSource;
  private Object object;
  private String objectMimeType;
  private CommandMap currentCommandMap;
  private DataFlavor[] transferFlavors = NO_FLAVORS;
  private DataContentHandler dataContentHandler;
  private DataContentHandler factoryDCH;
  private DataContentHandlerFactory oldFactory;
  private String shortType;

  /**
   * Constructor in which the data is read from a data source.
   * @param ds the data source
   */
  public DataHandler(DataSource ds)
  {
    dataSource = ds;
    oldFactory = factory;
  }

  /**
   * Constructor using a reified object representation.
   * @param obj the object representation of the data
   * @param mimeType the MIME type of the object
   */
  public DataHandler(Object obj, String mimeType)
  {
    dataSource = null;
    object = obj;
    objectMimeType = mimeType;
    oldFactory = factory;
  }

  /**
   * Constructor in which the data is read from a URL.
   * @param url the URL
   */
  public DataHandler(URL url)
  {
    dataSource = new URLDataSource(url);
    oldFactory = factory;
  }

  /**
   * Returns the data source from which data is read.
   */
  public DataSource getDataSource()
  {
    if (dataSource != null)
      {
        return dataSource;
      }
    if (objDataSource == null)
      {
        objDataSource = new DataHandlerDataSource(this);
      }
    return objDataSource;
  }

  /**
   * Returns the name of the data object if created with a DataSource.
   */
  public String getName()
  {
    if (dataSource != null)
      {
        return dataSource.getName();
      }
    return null;
  }

  /**
   * Returns the MIME type of the data (with parameters).
   */
  public String getContentType()
  {
    if (dataSource != null)
      {
        return dataSource.getContentType();
      }
    return objectMimeType;
  }

  /**
   * Returns an input stream from which the data can be read.
   */
  public InputStream getInputStream()
    throws IOException
  {
    if (dataSource != null)
      {
        return dataSource.getInputStream();
      }
    DataContentHandler dch = getDataContentHandler();
    if (dch == null)
      {
        throw new UnsupportedDataTypeException("no DCH for MIME type " +
                                               getShortType());
      }
    if ((dch instanceof ObjectDataContentHandler) &&
        ((ObjectDataContentHandler)dch).getDCH() == null)
      {
        throw new UnsupportedDataTypeException("no object DCH " +
                                               "for MIME type " +
                                               getShortType());
      }
    PipedOutputStream pos = new PipedOutputStream();
    DataContentHandlerWriter dchw =
      new DataContentHandlerWriter(dch, object, objectMimeType, pos);
    Thread thread = new Thread(dchw, "DataHandler.getInputStream");
    thread.start();
    return new PipedInputStream(pos);
  }

  static class DataContentHandlerWriter
    implements Runnable
  {

    DataContentHandler dch;
    Object object;
    String mimeType;
    OutputStream out;

    DataContentHandlerWriter(DataContentHandler dch, Object object,
                             String mimeType, OutputStream out)
    {
      this.dch = dch;
      this.object = object;
      this.mimeType = mimeType;
      this.out = out;
    }

    public void run()
    {
      try
        {
          dch.writeTo(object, mimeType, out);
        }
      catch(IOException e)
        {
        }
      finally
        {
          try
            {
              out.close();
            }
          catch(IOException e)
            {
            }
        }
    }
  }

  /**
   * Writes the data as a byte stream.
   * @param os the stream to write to
   */
  public void writeTo(OutputStream os)
    throws IOException
  {
    if (dataSource != null)
      {
        InputStream in = dataSource.getInputStream();
        byte[] buf = new byte[8192];
        for (int len = in.read(buf); len != -1; len = in.read(buf))
          {
            os.write(buf, 0, len);
          }
        in.close();
      }
    else
      {
        DataContentHandler dch = getDataContentHandler();
        dch.writeTo(object, objectMimeType, os);
      }
  }

  /**
   * Returns an output stream that can be used to overwrite the underlying
   * data, if the DataSource constructor was used.
   */
  public OutputStream getOutputStream()
    throws IOException
  {
    if (dataSource != null)
      {
        return dataSource.getOutputStream();
      }
    return null;
  }

  /**
   * Returns the data flavors in which this data is available.
   */
  public synchronized DataFlavor[] getTransferDataFlavors()
  {
    if (factory != oldFactory || transferFlavors == NO_FLAVORS)
      {
        DataContentHandler dch = getDataContentHandler();
        transferFlavors = dch.getTransferDataFlavors();
      }
    return transferFlavors;
  }

  /**
   * Indicates whether the specified data flavor is supported for this
   * data.
   */
  public boolean isDataFlavorSupported(DataFlavor flavor)
  {
    DataFlavor[] flavors = getTransferDataFlavors();
    for (int i = 0; i < flavors.length; i++)
      {
        if (flavors[i].equals(flavor))
          {
            return true;
          }
      }
    return false;
  }

  /**
   * Returns an object representing the data to be transferred.
   * @param flavor the requested data flavor
   */
  public Object getTransferData(DataFlavor flavor)
    throws UnsupportedFlavorException, IOException
  {
    DataContentHandler dch = getDataContentHandler();
    return dch.getTransferData(flavor, dataSource);
  }

  /**
   * Sets the command map to be used by this data handler.
   * Setting to null uses the default command map.
   * @param commandMap the command map to use
   */
  public synchronized void setCommandMap(CommandMap commandMap)
  {
    if (commandMap != currentCommandMap || commandMap == null)
      {
        transferFlavors = NO_FLAVORS;
        dataContentHandler = null;
        currentCommandMap = commandMap;
      }
  }

  /**
   * Returns the preferred commands for this type of data.
   */
  public CommandInfo[] getPreferredCommands()
  {
    CommandMap commandMap = getCommandMap();
    return commandMap.getPreferredCommands(getShortType());
  }

  /**
   * Returns the complete list of commands for this type of data.
   */
  public CommandInfo[] getAllCommands()
  {
    CommandMap commandMap = getCommandMap();
    return commandMap.getAllCommands(getShortType());
  }

  /**
   * Returns the specified command.
   * @param cmdName the command name
   */
  public CommandInfo getCommand(String cmdName)
  {
    CommandMap commandMap = getCommandMap();
    return commandMap.getCommand(getShortType(), cmdName);
  }

  /**
   * Returns the data as a reified object.
   */
  public Object getContent()
    throws IOException
  {
    DataContentHandler dch = getDataContentHandler();
    return dch.getContent(getDataSource());
  }

  /**
   * Returns the instantiated bean using the specified command.
   * @param cmdInfo the command to instantiate the bean with
   */
  public Object getBean(CommandInfo cmdInfo)
  {
    try
      {
        return cmdInfo.getCommandObject(this, getClass().getClassLoader());
      }
    catch (IOException e)
      {
        e.printStackTrace(System.err);
        return null;
      }
    catch (ClassNotFoundException e)
      {
        e.printStackTrace(System.err);
        return null;
      }
  }

  /**
   * Sets the data content handler factory.
   * If the factory has already been set, throws an Error.
   * @param newFactory the factory to set
   */
  public static synchronized void
    setDataContentHandlerFactory(DataContentHandlerFactory newFactory)
  {
    if (factory != null)
      {
        throw new Error("DataContentHandlerFactory already defined");
      }
    SecurityManager security = System.getSecurityManager();
    if (security != null)
      {
        try
          {
            security.checkSetFactory();
          }
        catch (SecurityException e)
          {
            if (newFactory != null && DataHandler.class.getClassLoader()
                != newFactory.getClass().getClassLoader())
              {
                throw e;
              }
          }
      }
    factory = newFactory;
  }

  /*
   * Returns just the base part of the data's content-type, with no
   * parameters.
   */
  private synchronized String getShortType()
  {
    if (shortType == null)
      {
        String contentType = getContentType();
        try
          {
            MimeType mimeType = new MimeType(contentType);
            shortType = mimeType.getBaseType();
          }
        catch (MimeTypeParseException e)
          {
            shortType = contentType;
          }
      }
    return shortType;
  }

  /*
   * Returns the command map for this handler.
   */
  private synchronized CommandMap getCommandMap()
  {
    if (currentCommandMap != null)
      {
        return currentCommandMap;
      }
    return CommandMap.getDefaultCommandMap();
  }

  /*
   * Returns the DCH for this handler.
   */
  private synchronized DataContentHandler getDataContentHandler()
  {
    if (factory != oldFactory)
      {
        oldFactory = factory;
        factoryDCH = null;
        dataContentHandler = null;
        transferFlavors = NO_FLAVORS;
      }
    if (dataContentHandler != null)
      {
        return dataContentHandler;
      }
    String mimeType = getShortType();
    if (factoryDCH == null && factory != null)
      {
        factoryDCH = factory.createDataContentHandler(mimeType);
      }
    if (factoryDCH != null)
      {
        dataContentHandler = factoryDCH;
      }
    if (dataContentHandler == null)
      {
        CommandMap commandMap = getCommandMap();
        dataContentHandler = commandMap.createDataContentHandler(mimeType);
      }
    if (dataSource != null)
      {
        dataContentHandler =
          new DataSourceDataContentHandler(dataContentHandler, dataSource);
      }
    else
      {
        dataContentHandler =
          new ObjectDataContentHandler(dataContentHandler, object,
                                       objectMimeType);
      }
    return dataContentHandler;
  }

}

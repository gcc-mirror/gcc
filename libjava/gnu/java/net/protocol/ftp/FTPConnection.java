/* FTPConnection.java --
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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


package gnu.java.net.protocol.ftp;

import gnu.java.net.CRLFInputStream;
import gnu.java.net.CRLFOutputStream;
import gnu.java.net.EmptyX509TrustManager;
import gnu.java.net.LineInputStream;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.BindException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ProtocolException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.List;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;

/**
 * An FTP client connection, or PI.
 * This implements RFC 959, with the following exceptions:
 * <ul>
 * <li>STAT, HELP, SITE, SMNT, and ACCT commands are not supported.</li>
 * <li>the TYPE command does not allow alternatives to the default bytesize
 * (Non-print), and local bytesize is not supported.</li>
 * </ul>
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class FTPConnection
{

  /**
   * The default FTP transmission control port.
   */
  public static final int FTP_PORT = 21;

  /**
   * The FTP data port.
   */
  public static final int FTP_DATA_PORT = 20;

  // -- FTP vocabulary --
  protected static final String USER = "USER";
  protected static final String PASS = "PASS";
  protected static final String ACCT = "ACCT";
  protected static final String CWD = "CWD";
  protected static final String CDUP = "CDUP";
  protected static final String SMNT = "SMNT";
  protected static final String REIN = "REIN";
  protected static final String QUIT = "QUIT";

  protected static final String PORT = "PORT";
  protected static final String PASV = "PASV";
  protected static final String TYPE = "TYPE";
  protected static final String STRU = "STRU";
  protected static final String MODE = "MODE";

  protected static final String RETR = "RETR";
  protected static final String STOR = "STOR";
  protected static final String STOU = "STOU";
  protected static final String APPE = "APPE";
  protected static final String ALLO = "ALLO";
  protected static final String REST = "REST";
  protected static final String RNFR = "RNFR";
  protected static final String RNTO = "RNTO";
  protected static final String ABOR = "ABOR";
  protected static final String DELE = "DELE";
  protected static final String RMD = "RMD";
  protected static final String MKD = "MKD";
  protected static final String PWD = "PWD";
  protected static final String LIST = "LIST";
  protected static final String NLST = "NLST";
  protected static final String SITE = "SITE";
  protected static final String SYST = "SYST";
  protected static final String STAT = "STAT";
  protected static final String HELP = "HELP";
  protected static final String NOOP = "NOOP";
  
  protected static final String AUTH = "AUTH";
  protected static final String PBSZ = "PBSZ";
  protected static final String PROT = "PROT";
  protected static final String CCC = "CCC";
  protected static final String TLS = "TLS";

  public static final int TYPE_ASCII = 1;
  public static final int TYPE_EBCDIC = 2;
  public static final int TYPE_BINARY = 3;

  public static final int STRUCTURE_FILE = 1;
  public static final int STRUCTURE_RECORD = 2;
  public static final int STRUCTURE_PAGE = 3;

  public static final int MODE_STREAM = 1;
  public static final int MODE_BLOCK = 2;
  public static final int MODE_COMPRESSED = 3;

  // -- Telnet constants --
  private static final String US_ASCII = "US-ASCII";

  /**
   * The socket used to communicate with the server.
   */
  protected Socket socket;

  /**
   * The socket input stream.
   */
  protected LineInputStream in;

  /**
   * The socket output stream.
   */
  protected CRLFOutputStream out;

  /**
   * The timeout when attempting to connect a socket.
   */
  protected int connectionTimeout;

  /**
   * The read timeout on sockets.
   */
  protected int timeout;

  /**
   * If true, print debugging information.
   */
  protected boolean debug;

  /**
   * The current data transfer process in use by this connection.
   */
  protected DTP dtp;

  /**
   * The current representation type.
   */
  protected int representationType = TYPE_ASCII;

  /**
   * The current file structure type.
   */
  protected int fileStructure = STRUCTURE_FILE;

  /**
   * The current transfer mode.
   */
  protected int transferMode = MODE_STREAM;

  /**
   * If true, use passive mode.
   */
  protected boolean passive = false;

  /**
   * Creates a new connection to the server using the default port.
   * @param hostname the hostname of the server to connect to
   */
  public FTPConnection(String hostname)
    throws UnknownHostException, IOException
  {
    this(hostname, -1, 0, 0, false);
  }
  
  /**
   * Creates a new connection to the server.
   * @param hostname the hostname of the server to connect to
   * @param port the port to connect to(if &lt;=0, use default port)
   */
  public FTPConnection(String hostname, int port)
    throws UnknownHostException, IOException
  {
    this(hostname, port, 0, 0, false);
  }

  /**
   * Creates a new connection to the server.
   * @param hostname the hostname of the server to connect to
   * @param port the port to connect to(if &lt;=0, use default port)
   * @param connectionTimeout the connection timeout, in milliseconds
   * @param timeout the I/O timeout, in milliseconds
   * @param debug print debugging information
   */
  public FTPConnection(String hostname, int port,
                        int connectionTimeout, int timeout, boolean debug)
    throws UnknownHostException, IOException
  {
    this.connectionTimeout = connectionTimeout;
    this.timeout = timeout;
    this.debug = debug;
    if (port <= 0)
      {
        port = FTP_PORT;
      }
    
    // Set up socket
    socket = new Socket();
    InetSocketAddress address = new InetSocketAddress(hostname, port);
    if (connectionTimeout > 0)
      {
        socket.connect(address, connectionTimeout);
      }
    else
      {
        socket.connect(address);
      }
    if (timeout > 0)
      {
        socket.setSoTimeout(timeout);
      }
    
    InputStream in = socket.getInputStream();
    in = new BufferedInputStream(in);
    in = new CRLFInputStream(in);
    this.in = new LineInputStream(in);
    OutputStream out = socket.getOutputStream();
    out = new BufferedOutputStream(out);
    this.out = new CRLFOutputStream(out);
    
    // Read greeting
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 220:                  // hello
        break;
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Authenticate using the specified username and password.
   * If the username suffices for the server, the password will not be used
   * and may be null.
   * @param username the username
   * @param password the optional password
   * @return true on success, false otherwise
   */
  public boolean authenticate(String username, String password)
    throws IOException
  {
    String cmd = USER + ' ' + username;
    send(cmd);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 230:                  // User logged in
        return true;
      case 331:                 // User name okay, need password
        break;
      case 332:                 // Need account for login
      case 530:                 // No such user
        return false;
      default:
        throw new FTPException(response);
      }
    cmd = PASS + ' ' + password;
    send(cmd);
    response = getResponse();
    switch (response.getCode())
      {
      case 230:                  // User logged in
      case 202:                  // Superfluous
        return true;
      case 332:                  // Need account for login
      case 530:                  // Bad password
        return false;
      default:
        throw new FTPException(response);
      }
  }

  /**
   * Negotiates TLS over the current connection.
   * See IETF draft-murray-auth-ftp-ssl-15.txt for details.
   * @param confidential whether to provide confidentiality for the
   * connection
   */
  public boolean starttls(boolean confidential)
    throws IOException
  {
    return starttls(confidential, new EmptyX509TrustManager());
  }
  
  /**
   * Negotiates TLS over the current connection.
   * See IETF draft-murray-auth-ftp-ssl-15.txt for details.
   * @param confidential whether to provide confidentiality for the
   * connection
   * @param tm the trust manager used to validate the server certificate.
   */
  public boolean starttls(boolean confidential, TrustManager tm)
    throws IOException
  {
    try
      {
        // Use SSLSocketFactory to negotiate a TLS session and wrap the
        // current socket.
        SSLContext context = SSLContext.getInstance("TLS");
        // We don't require strong validation of the server certificate
        TrustManager[] trust = new TrustManager[] { tm };
        context.init(null, trust, null);
        SSLSocketFactory factory = context.getSocketFactory();
        
        send(AUTH + ' ' + TLS);
        FTPResponse response = getResponse();
        switch (response.getCode())
          {
          case 500:
          case 502:
          case 504:
          case 534:
          case 431:
            return false;
          case 234:
            break;
          default:
            throw new FTPException(response);
          }
        
        String hostname = socket.getInetAddress().getHostName();
        int port = socket.getPort();
        SSLSocket ss =
         (SSLSocket) factory.createSocket(socket, hostname, port, true);
        String[] protocols = { "TLSv1", "SSLv3" };
        ss.setEnabledProtocols(protocols);
        ss.setUseClientMode(true);
        ss.startHandshake();

        // PBSZ:PROT sequence
        send(PBSZ + ' ' + Integer.MAX_VALUE);
        response = getResponse();
        switch (response.getCode())
          {
          case 501: // syntax error
          case 503: // not authenticated
            return false;
          case 200:
            break;
          default:
            throw new FTPException(response);
          }
        send(PROT + ' ' +(confidential ? 'P' : 'C'));
        response = getResponse();
        switch (response.getCode())
          {
          case 503: // not authenticated
          case 504: // invalid level
          case 536: // level not supported
            return false;
          case 200:
            break;
          default:
            throw new FTPException(response);
          }
        
        if (confidential)
          {
            // Set up streams
            InputStream in = ss.getInputStream();
            in = new BufferedInputStream(in);
            in = new CRLFInputStream(in);
            this.in = new LineInputStream(in);
            OutputStream out = ss.getOutputStream();
            out = new BufferedOutputStream(out);
            this.out = new CRLFOutputStream(out);
          }
        return true;
      }
    catch (GeneralSecurityException e)
      {
        return false;
      }
  }
  
  /**
   * Changes directory to the specified path.
   * @param path an absolute or relative pathname
   * @return true on success, false if the specified path does not exist
   */
  public boolean changeWorkingDirectory(String path)
    throws IOException
  {
    String cmd = CWD + ' ' + path;
    send(cmd);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 250:
        return true;
      case 550:
        return false;
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Changes directory to the parent of the current working directory.
   * @return true on success, false otherwise
   */
  public boolean changeToParentDirectory()
    throws IOException
  {
    send(CDUP);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 250:
        return true;
      case 550:
        return false;
      default:
        throw new FTPException(response);
      }
  }

  /**
   * Terminates an authenticated login.
   * If file transfer is in progress, it remains active for result response
   * only.
   */
  public void reinitialize()
    throws IOException
  {
    send(REIN);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 220:
        if (dtp != null)
          {
            dtp.complete();
            dtp = null;
          }
        break;
      default:
        throw new FTPException(response);
      }
  }

  /**
   * Terminates the control connection.
   * The file transfer connection remains open for result response only.
   * This connection is invalid and no further commands may be issued.
   */
  public void logout()
    throws IOException
  {
    send(QUIT);
    try
      {
        getResponse();            // not required
      }
    catch (IOException e)
      {
      }
    if (dtp != null)
      {
        dtp.complete();
        dtp = null;
      }
    try
      {
        socket.close();
      }
    catch (IOException e)
      {
      }
  }
  
  /**
   * Initialise the data transfer process.
   */
  protected void initialiseDTP()
    throws IOException
  {
    if (dtp != null)
      {
        dtp.complete();
        dtp = null;
      }
    
    InetAddress localhost = socket.getLocalAddress();
    if (passive)
      {
        send(PASV);
        FTPResponse response = getResponse();
        switch (response.getCode())
          {
          case 227:
            String message = response.getMessage();
            try
              {
                int start = message.indexOf(',');
                char c = message.charAt(start - 1);
                while (c >= 0x30 && c <= 0x39)
                  {
                    c = message.charAt((--start) - 1);
                  }
                int mid1 = start;
                for (int i = 0; i < 4; i++)
                  {
                    mid1 = message.indexOf(',', mid1 + 1);
                  }
                int mid2 = message.indexOf(',', mid1 + 1);
                if (mid1 == -1 || mid2 < mid1)
                  {
                    throw new ProtocolException("Malformed 227: " +
                                                 message);
                  }
                int end = mid2;
                c = message.charAt(end + 1);
                while (c >= 0x30 && c <= 0x39)
                  {
                    c = message.charAt((++end) + 1);
                  }
                
                String address =
                  message.substring(start, mid1).replace(',', '.');
                int port_hi =
                  Integer.parseInt(message.substring(mid1 + 1, mid2));
                int port_lo =
                  Integer.parseInt(message.substring(mid2 + 1, end + 1));
                int port = (port_hi << 8) | port_lo;
                
                /*System.out.println("Entering passive mode: " + address +
                  ":" + port);*/
                dtp = new PassiveModeDTP(address, port, localhost,
                                          connectionTimeout, timeout);
                break;
              }
            catch (ArrayIndexOutOfBoundsException e)
              {
                throw new ProtocolException(e.getMessage() + ": " +
                                             message);
              }
            catch (NumberFormatException e)
              {
                throw new ProtocolException(e.getMessage() + ": " +
                                             message);
              }
          default:
            throw new FTPException(response);
          }
      }
    else
      {
        // Get the local port
        int port = socket.getLocalPort() + 1;
        int tries = 0;
        // Bind the active mode DTP
        while (dtp == null)
          {
            try
              {
                dtp = new ActiveModeDTP(localhost, port,
                                         connectionTimeout, timeout);
                /*System.out.println("Listening on: " + port);*/
              }
            catch (BindException e)
              {
                port++;
                tries++;
                if (tries > 9)
                  {
                    throw e;
                  }
              }
          }
        
        // Send PORT command
        StringBuffer buf = new StringBuffer(PORT);
        buf.append(' ');
        // Construct the address/port string form
        byte[] address = localhost.getAddress();
        for (int i = 0; i < address.length; i++)
          {
            int a =(int) address[i];
            if (a < 0)
              {
                a += 0x100;
              }
            buf.append(a);
            buf.append(',');
          }
        int port_hi =(port & 0xff00) >> 8;
        int port_lo =(port & 0x00ff);
        buf.append(port_hi);
        buf.append(',');
        buf.append(port_lo);
        send(buf.toString());
        // Get response
        FTPResponse response = getResponse();
        switch (response.getCode())
          {
          case 200:                // OK
            break;
          default:
            dtp.abort();
            dtp = null;
            throw new FTPException(response);
          }
      }
    dtp.setTransferMode(transferMode);
  }
  
  /**
   * Set passive mode.
   * @param flag true if we should use passive mode, false otherwise
   */
  public void setPassive(boolean flag)
    throws IOException
  {
    if (passive != flag)
      {
        passive = flag;
        initialiseDTP();
      }
  }
  
  /**
   * Returns the current representation type of the transfer data.
   * @return TYPE_ASCII, TYPE_EBCDIC, or TYPE_BINARY
   */
  public int getRepresentationType()
  {
    return representationType;
  }

  /**
   * Sets the desired representation type of the transfer data.
   * @param type TYPE_ASCII, TYPE_EBCDIC, or TYPE_BINARY
   */
  public void setRepresentationType(int type)
    throws IOException
  {
    StringBuffer buf = new StringBuffer(TYPE);
    buf.append(' ');
    switch (type)
      {
      case TYPE_ASCII:
        buf.append('A');
        break;
      case TYPE_EBCDIC:
        buf.append('E');
        break;
      case TYPE_BINARY:
        buf.append('I');
        break;
      default:
        throw new IllegalArgumentException(Integer.toString(type));
      }
    //buf.append(' ');
    //buf.append('N');
    send(buf.toString());
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 200:
        representationType = type;
        break;
      default:
        throw new FTPException(response);
      }
  }

  /**
   * Returns the current file structure type.
   * @return STRUCTURE_FILE, STRUCTURE_RECORD, or STRUCTURE_PAGE
   */
  public int getFileStructure()
  {
    return fileStructure;
  }

  /**
   * Sets the desired file structure type.
   * @param structure STRUCTURE_FILE, STRUCTURE_RECORD, or STRUCTURE_PAGE
   */
  public void setFileStructure(int structure)
    throws IOException
  {
    StringBuffer buf = new StringBuffer(STRU);
    buf.append(' ');
    switch (structure)
      {
      case STRUCTURE_FILE:
        buf.append('F');
        break;
      case STRUCTURE_RECORD:
        buf.append('R');
        break;
      case STRUCTURE_PAGE:
        buf.append('P');
        break;
      default:
        throw new IllegalArgumentException(Integer.toString(structure));
      }
    send(buf.toString());
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 200:
        fileStructure = structure;
        break;
      default:
        throw new FTPException(response);
      }
  }

  /**
   * Returns the current transfer mode.
   * @return MODE_STREAM, MODE_BLOCK, or MODE_COMPRESSED
   */
  public int getTransferMode()
  {
    return transferMode;
  }

  /**
   * Sets the desired transfer mode.
   * @param mode MODE_STREAM, MODE_BLOCK, or MODE_COMPRESSED
   */
  public void setTransferMode(int mode)
    throws IOException
  {
    StringBuffer buf = new StringBuffer(MODE);
    buf.append(' ');
    switch (mode)
      {
      case MODE_STREAM:
        buf.append('S');
        break;
      case MODE_BLOCK:
        buf.append('B');
        break;
      case MODE_COMPRESSED:
        buf.append('C');
        break;
      default:
        throw new IllegalArgumentException(Integer.toString(mode));
      }
    send(buf.toString());
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 200:
        transferMode = mode;
        if (dtp != null)
          {
            dtp.setTransferMode(mode);
          }
        break;
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Retrieves the specified file.
   * @param filename the filename of the file to retrieve
   * @return an InputStream containing the file content
   */
  public InputStream retrieve(String filename)
    throws IOException
  {
    if (dtp == null || transferMode == MODE_STREAM)
      {
        initialiseDTP();
      }
    /*
       int size = -1;
       String cmd = SIZE + ' ' + filename;
       send(cmd);
       FTPResponse response = getResponse();
       switch (response.getCode())
       {
       case 213:
       size = Integer.parseInt(response.getMessage());
       break;
       case 550: // File not found
       default:
       throw new FTPException(response);
       }
     */
    String cmd = RETR + ' ' + filename;
    send(cmd);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 125:                  // Data connection already open; transfer starting
      case 150:                  // File status okay; about to open data connection
        return dtp.getInputStream();
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Returns a stream for uploading a file.
   * If a file with the same filename already exists on the server, it will
   * be overwritten.
   * @param filename the name of the file to save the content as
   * @return an OutputStream to write the file data to
   */
  public OutputStream store(String filename)
    throws IOException
  {
    if (dtp == null || transferMode == MODE_STREAM)
      {
        initialiseDTP();
      }
    String cmd = STOR + ' ' + filename;
    send(cmd);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 125:                  // Data connection already open; transfer starting
      case 150:                  // File status okay; about to open data connection
        return dtp.getOutputStream();
      default:
        throw new FTPException(response);
      }
  }

  /**
   * Returns a stream for uploading a file.
   * If a file with the same filename already exists on the server, the
   * content specified will be appended to the existing file.
   * @param filename the name of the file to save the content as
   * @return an OutputStream to write the file data to
   */
  public OutputStream append(String filename)
    throws IOException
  {
    if (dtp == null || transferMode == MODE_STREAM)
      {
        initialiseDTP();
      }
    String cmd = APPE + ' ' + filename;
    send(cmd);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 125:                  // Data connection already open; transfer starting
      case 150:                  // File status okay; about to open data connection
        return dtp.getOutputStream();
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * This command may be required by some servers to reserve sufficient
   * storage to accommodate the new file to be transferred.
   * It should be immediately followed by a <code>store</code> or
   * <code>append</code>.
   * @param size the number of bytes of storage to allocate
   */
  public void allocate(long size)
    throws IOException
  {
    String cmd = ALLO + ' ' + size;
    send(cmd);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 200:                  // OK
      case 202:                  // Superfluous
        break;
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Renames a file.
   * @param oldName the current name of the file
   * @param newName the new name
   * @return true if successful, false otherwise
   */
  public boolean rename(String oldName, String newName)
    throws IOException
  {
    String cmd = RNFR + ' ' + oldName;
    send(cmd);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 450:                  // File unavailable
      case 550:                  // File not found
        return false;
      case 350:                 // Pending
        break;
      default:
        throw new FTPException(response);
      }
    cmd = RNTO + ' ' + newName;
    send(cmd);
    response = getResponse();
    switch (response.getCode())
      {
      case 250:                  // OK
        return true;
      case 450:
      case 550:
        return false;
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Aborts the transfer in progress.
   * @return true if a transfer was in progress, false otherwise
   */
  public boolean abort()
    throws IOException
  {
    send(ABOR);
    FTPResponse response = getResponse();
    // Abort client DTP
    if (dtp != null)
      {
        dtp.abort();
      }
    switch (response.getCode())
      {
      case 226:                  // successful abort
        return false;
      case 426:                 // interrupted
        response = getResponse();
        if (response.getCode() == 226)
          {
            return true;
          }
        // Otherwise fall through to throw exception
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Causes the file specified to be deleted at the server site.
   * @param filename the file to delete
   */
  public boolean delete(String filename)
    throws IOException
  {
    String cmd = DELE + ' ' + filename;
    send(cmd);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 250:                  // OK
        return true;
      case 450:                 // File unavailable
      case 550:                 // File not found
        return false;
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Causes the directory specified to be deleted.
   * This may be an absolute or relative pathname.
   * @param pathname the directory to delete
   */
  public boolean removeDirectory(String pathname)
    throws IOException
  {
    String cmd = RMD + ' ' + pathname;
    send(cmd);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 250:                  // OK
        return true;
      case 550:                 // File not found
        return false;
      default:
        throw new FTPException(response);
      }
  }

  /**
   * Causes the directory specified to be created at the server site.
   * This may be an absolute or relative pathname.
   * @param pathname the directory to create
   */
  public boolean makeDirectory(String pathname)
    throws IOException
  {
    String cmd = MKD + ' ' + pathname;
    send(cmd);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 257:                  // Directory created
        return true;
      case 550:                 // File not found
        return false;
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Returns the current working directory.
   */
  public String getWorkingDirectory()
    throws IOException
  {
    send(PWD);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 257:
        String message = response.getMessage();
        if (message.charAt(0) == '"')
          {
            int end = message.indexOf('"', 1);
            if (end == -1)
              {
                throw new ProtocolException(message);
              }
            return message.substring(1, end);
          }
        else
          {
            int end = message.indexOf(' ');
            if (end == -1)
              {
                return message;
              }
            else
              {
                return message.substring(0, end);
              }
          }
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Returns a listing of information about the specified pathname.
   * If the pathname specifies a directory or other group of files, the
   * server should transfer a list of files in the specified directory.
   * If the pathname specifies a file then the server should send current
   * information on the file.  A null argument implies the user's
   * current working or default directory.
   * @param pathname the context pathname, or null
   */
  public InputStream list(String pathname)
    throws IOException
  {
    if (dtp == null || transferMode == MODE_STREAM)
      {
        initialiseDTP();
      }
    if (pathname == null)
      {
        send(LIST);
      }
    else
      {
        String cmd = LIST + ' ' + pathname;
        send(cmd);
      }
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 125:                  // Data connection already open; transfer starting
      case 150:                  // File status okay; about to open data connection
        return dtp.getInputStream();
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Returns a directory listing. The pathname should specify a
   * directory or other system-specific file group descriptor; a null
   * argument implies the user's current working or default directory.
   * @param pathname the directory pathname, or null
   * @return a list of filenames(strings)
   */
  public List nameList(String pathname)
    throws IOException
  {
    if (dtp == null || transferMode == MODE_STREAM)
      {
        initialiseDTP();
      }
    if (pathname == null)
      {
        send(NLST);
      }
    else
      {
        String cmd = NLST + ' ' + pathname;
        send(cmd);
      }
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 125:                  // Data connection already open; transfer starting
      case 150:                  // File status okay; about to open data connection
        InputStream in = dtp.getInputStream();
        in = new BufferedInputStream(in);
        in = new CRLFInputStream(in);     // TODO ensure that TYPE is correct
        LineInputStream li = new LineInputStream(in);
        List ret = new ArrayList();
        for (String line = li.readLine();
             line != null;
             line = li.readLine())
          {
            ret.add(line);
          }
        li.close();
        return ret;
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Returns the type of operating system at the server.
   */
  public String system()
    throws IOException
  {
    send(SYST);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 215:
        String message = response.getMessage();
        int end = message.indexOf(' ');
        if (end == -1)
          {
            return message;
          }
        else
          {
            return message.substring(0, end);
          }
      default:
        throw new FTPException(response);
      }
  }
  
  /**
   * Does nothing.
   * This method can be used to ensure that the connection does not time
   * out.
   */
  public void noop()
    throws IOException
  {
    send(NOOP);
    FTPResponse response = getResponse();
    switch (response.getCode())
      {
      case 200:
        break;
      default:
        throw new FTPException(response);
      }
  }

  // -- I/O --

  /**
   * Sends the specified command line to the server.
   * The CRLF sequence is automatically appended.
   * @param cmd the command line to send
   */
  protected void send(String cmd)
    throws IOException
  {
    byte[] data = cmd.getBytes(US_ASCII);
    out.write(data);
    out.writeln();
    out.flush();
  }

  /**
   * Reads the next response from the server.
   * If the server sends the "transfer complete" code, this is handled here,
   * and the next response is passed to the caller.
   */
  protected FTPResponse getResponse()
    throws IOException
  {
    FTPResponse response = readResponse();
    if (response.getCode() == 226)
      {
        if (dtp != null)
          {
            dtp.transferComplete();
          }
        response = readResponse();
      }
    return response;
  }

  /**
   * Reads and parses the next response from the server.
   */
  protected FTPResponse readResponse()
    throws IOException
  {
    String line = in.readLine();
    if (line == null)
      {
        throw new ProtocolException( "EOF");
      }
    if (line.length() < 4)
      {
        throw new ProtocolException(line);
      }
    int code = parseCode(line);
    if (code == -1)
      {
        throw new ProtocolException(line);
      }
    char c = line.charAt(3);
    if (c == ' ')
      {
        return new FTPResponse(code, line.substring(4));
      }
    else if (c == '-')
      {
        StringBuffer buf = new StringBuffer(line.substring(4));
        buf.append('\n');
        while(true)
          {
            line = in.readLine();
            if (line == null)
              {
                throw new ProtocolException("EOF");
              }
            if (line.length() >= 4 &&
                line.charAt(3) == ' ' &&
                parseCode(line) == code)
              {
                return new FTPResponse(code, line.substring(4),
                                        buf.toString());
              }
            else
              {
                buf.append(line);
                buf.append('\n');
              }
          }
      }
    else
      {
        throw new ProtocolException(line);
      }
  }
  
  /*
   * Parses the 3-digit numeric code at the beginning of the given line.
   * Returns -1 on failure.
   */
  static final int parseCode(String line)
  {
    char[] c = { line.charAt(0), line.charAt(1), line.charAt(2) };
    int ret = 0;
    for (int i = 0; i < 3; i++)
      {
        int digit =((int) c[i]) - 0x30;
        if (digit < 0 || digit > 9)
          {
            return -1;
          }
        // Computing integer powers is way too expensive in Java!
        switch (i)
          {
          case 0:
            ret +=(100 * digit);
            break;
          case 1:
            ret +=(10 * digit);
            break;
          case 2:
            ret += digit;
            break;
          }
      }
    return ret;
  }

}


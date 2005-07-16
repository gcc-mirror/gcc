/* bufferedResponseHandler.java --
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


package gnu.CORBA;

import gnu.CORBA.CDR.cdrBufOutput;
import gnu.CORBA.GIOP.MessageHeader;
import gnu.CORBA.GIOP.ReplyHeader;
import gnu.CORBA.GIOP.cxCodeSet;

import org.omg.CORBA.ORB;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;

/**
 * Provides the CDR output streams for writing the response to the given
 * buffer.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
class bufferedResponseHandler
  implements ResponseHandler
{
  /**
   * The message header.
   * This field is used to compute the size and alignments.
   * It is, however, never directly written to the buffer stream.
   */
  final MessageHeader message_header;

  /**
   * The associated orb.
   */
  final ORB orb;

  /**
   * The reply header. This field is used to compute the size and alignments.
   * It is, however, never directly written to the buffer stream.
   */
  final ReplyHeader reply_header;

  /**
   * True if the stream was obtained by invoking {@link #createExceptionReply()},
   * false otherwise.
   */
  private boolean exceptionReply;

  /**
   * The buffer to write into.
   */
  private cdrBufOutput buffer;

  /**
   * Create a new buffered response handler that uses the given message
   * headers. The headers are used to compute sizes and check the versions.
   * They are not written into a stream inside this class.
   *
   * @param m_header a message header.
   * @param r_header a reply header.
   */
  bufferedResponseHandler(ORB an_orb, MessageHeader m_header,
                          ReplyHeader r_header
                         )
  {
    message_header = m_header;
    reply_header = r_header;
    orb = an_orb;
    prepareStream();
  }

  /**
   * Get an output stream for providing details about the exception.
   * Before returning the stream, the handler automatically writes
   * the message header and the reply about exception header,
   * but not the message header.
   *
   * @return the stream to write exception details into.
   */
  public OutputStream createExceptionReply()
  {
    exceptionReply = true;
    prepareStream();
    return buffer;
  }

  /**
   * Get an output stream for writing a regular reply (not an exception).
   *
   * Before returning the stream, the handler automatically writes
   * the regular reply header, but not the message header.
   *
   * @return the output stream for writing a regular reply.
   */
  public OutputStream createReply()
  {
    exceptionReply = false;
    prepareStream();
    reply_header.reply_status = ReplyHeader.NO_EXCEPTION;
    return buffer;
  }

  /**
   * Get the buffer, normally containing the written reply.
   * The reply includes the reply header (or the exception header)
   * but does not include the message header.
   *
   * The stream buffer can also be empty if no data have been written
   * into streams, returned by {@link #createReply()} or
   * {@link #createExceptionReply()}.
   *
   * @return the CDR output stream, containing the written output.
   */
  cdrBufOutput getBuffer()
  {
    return buffer;
  }

  /**
   * True if the stream was obtained by invoking
   * {@link #createExceptionReply()}, false otherwise
   * (usually no-exception reply).
   */
  boolean isExceptionReply()
  {
    return exceptionReply;
  }

  /**
   * Compute the header offset, set the correct version number and codeset.
   */
  private void prepareStream()
  {
    buffer = new cdrBufOutput();
    buffer.setOrb(orb);
    buffer.setOffset(message_header.getHeaderSize());

    // Get the position after the reply header would be written.
    reply_header.write(buffer);

    int new_offset = message_header.getHeaderSize() + buffer.buffer.size();

    buffer.buffer.reset();
    buffer.setOffset(new_offset);

    if (message_header.version.since_inclusive(1, 2))
      buffer.align(8);

    buffer.setVersion(message_header.version);

    buffer.setCodeSet(cxCodeSet.find(reply_header.service_context));
  }
}

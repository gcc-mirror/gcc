// natMappedByteFileBuffer.cc

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <jvm.h>

#include <errno.h>
#include <string.h>
#include <sys/types.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <gnu/gcj/RawData.h>
#include <gnu/java/nio/MappedByteFileBuffer.h>
#include <java/lang/Error.h>

jbyte
gnu::java::nio::MappedByteFileBuffer::nio_read_Byte_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

jchar
gnu::java::nio::MappedByteFileBuffer::nio_read_Char_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

jdouble
gnu::java::nio::MappedByteFileBuffer::nio_read_Double_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

jfloat
gnu::java::nio::MappedByteFileBuffer::nio_read_Float_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

jint
gnu::java::nio::MappedByteFileBuffer::nio_read_Int_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

jlong
gnu::java::nio::MappedByteFileBuffer::nio_read_Long_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

jshort
gnu::java::nio::MappedByteFileBuffer::nio_read_Short_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

void
gnu::java::nio::MappedByteFileBuffer::nio_write_Byte_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, jbyte,
                                             gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

void
gnu::java::nio::MappedByteFileBuffer::nio_write_Char_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, jchar,
                                             gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

void
gnu::java::nio::MappedByteFileBuffer::nio_write_Double_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, jdouble,
                                             gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

void
gnu::java::nio::MappedByteFileBuffer::nio_write_Float_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, jfloat,
                                             gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

void
gnu::java::nio::MappedByteFileBuffer::nio_write_Int_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, jint,
                                             gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

void
gnu::java::nio::MappedByteFileBuffer::nio_write_Long_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, jlong,
                                             gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

void
gnu::java::nio::MappedByteFileBuffer::nio_write_Short_file_channel
                                            (gnu::java::nio::FileChannelImpl*,
                                             jint, jint, jshort,
                                             gnu::gcj::RawData*)
{
  throw new ::java::lang::Error (_Jv_NewStringUTF ("not implemented"));
}

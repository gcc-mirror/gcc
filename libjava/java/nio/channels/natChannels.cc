// natChannels.cc - Native part of Channels class.

/* Copyright (C) 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>

#include <java/nio/channels/Channels.h>
#include <java/io/FileInputStream.h>
#include <java/io/FileOutputStream.h>
#include <gnu/java/nio/channels/FileChannelImpl.h>

using java::nio::channels::Channels;
using java::io::FileInputStream;
using java::io::FileOutputStream;
using gnu::java::nio::channels::FileChannelImpl;

FileInputStream*
Channels::newInputStream(FileChannelImpl* ch)
{
  // Needs to be native to bypass Java access protection.
  return new FileInputStream (ch);
}

FileOutputStream*
Channels::newOutputStream(FileChannelImpl* ch)
{
  // Needs to be native to bypass Java access protection.
  return new FileOutputStream (ch);
}

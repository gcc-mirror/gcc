/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <java/net/NetworkInterface.h>
#include <java/net/SocketException.h>
#include <java/util/Vector.h>

::java::util::Vector*
java::net::NetworkInterface::getRealNetworkInterfaces ()
{
  throw new SocketException (
    JvNewStringLatin1 ("NetworkInterface.getrealNetworkInterfaces: unimplemented"));
}

/* Copyright (C) 2003, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <java/net/SocketException.h>
#include <java/net/VMNetworkInterface.h>
#include <java/util/Vector.h>

::java::util::Vector*
java::net::VMNetworkInterface::getInterfaces ()
{
  throw new SocketException (
    JvNewStringLatin1 ("VMNetworkInterface.getInterfaces: unimplemented"));
}

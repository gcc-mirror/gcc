..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _cuda-streams-usage:

CUDA Streams Usage
------------------

This applies to the ``nvptx`` plugin only.

The library provides elements that perform asynchronous movement of
data and asynchronous operation of computing constructs.  This
asynchronous functionality is implemented by making use of CUDA
streams [#f1]_.

The primary means by that the asynchronous functionality is accessed
is through the use of those OpenACC directives which make use of the
``async`` and ``wait`` clauses.  When the ``async`` clause is
first used with a directive, it creates a CUDA stream.  If an
``async-argument`` is used with the ``async`` clause, then the
stream is associated with the specified ``async-argument``.

Following the creation of an association between a CUDA stream and the
``async-argument`` of an ``async`` clause, both the ``wait``
clause and the ``wait`` directive can be used.  When either the
clause or directive is used after stream creation, it creates a
rendezvous point whereby execution waits until all operations
associated with the ``async-argument``, that is, stream, have
completed.

Normally, the management of the streams that are created as a result of
using the ``async`` clause, is done without any intervention by the
caller.  This implies the association between the ``async-argument``
and the CUDA stream will be maintained for the lifetime of the program.
However, this association can be changed through the use of the library
function ``acc_set_cuda_stream``.  When the function
``acc_set_cuda_stream`` is called, the CUDA stream that was
originally associated with the ``async`` clause will be destroyed.
Caution should be taken when changing the association as subsequent
references to the ``async-argument`` refer to a different
CUDA stream.

.. -
   OpenACC Library Interoperability
   -

.. [#f1] See "Stream Management" in "CUDA Driver API",
  TRM-06703-001, Version 5.5, for additional information

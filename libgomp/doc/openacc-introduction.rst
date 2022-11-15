..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

Introduction
************

The OpenACC library uses the CUDA Driver API, and may interact with
programs that use the Runtime library directly, or another library
based on the Runtime library, e.g., CUBLAS [#f1]_.

This chapter describes the use cases and what changes are
required in order to use both the OpenACC library and the CUBLAS and Runtime
libraries within a program.

.. [#f1] See section 2.26,
  "Interactions with the CUDA Driver API" in
  "CUDA Runtime API", Version 5.5, and section 2.27, "VDPAU
  Interoperability", in "CUDA Driver API", TRM-06703-001, Version 5.5,
  for additional information on library interoperability.

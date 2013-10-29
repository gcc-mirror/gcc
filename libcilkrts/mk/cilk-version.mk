#########################################################################
#
#  @copyright
#  Copyright (C) 2009-2013, Intel Corporation
#  All rights reserved.
#  
#  @copyright
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#  
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#    * Neither the name of Intel Corporation nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#  
#  @copyright
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
#  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
#  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
#  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
#  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#  POSSIBILITY OF SUCH DAMAGE.
###########################################################################
# cilk-version.mk
#
# The one place we look up information from the code management system
#
# Note that the build number is *only* valid on the build machines

ifeq ($(wildcard $(TOP)/../.hg),)
  # If this is the open source release, there is no Mercurial repository,
  # so set some reasonable defaults.
  CILK_VERSION_MAJOR := 2
  CILK_VERSION_MINOR := 0
  CILK_VERSION_BUILD := 1
  CILK_VERSION_REV   := 0

  CILK_VERSION_HASH  := 000000000000
  CILK_VERSION_BRANCH := oss
else
  CILK_VERSION_MAJOR := 2
  CILK_VERSION_MINOR := 0
  CILK_VERSION_BUILD := $(firstword $(subst +, ,$(shell hg id --num)))
  CILK_VERSION_REV   := 0

  CILK_VERSION_HASH := $(firstword $(subst +, ,$(shell hg id --id)))
  CILK_VERSION_BRANCH := $(shell hg id --branch)
endif


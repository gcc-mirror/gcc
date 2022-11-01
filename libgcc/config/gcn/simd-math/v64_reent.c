#include "amdgcnmach.h"

static struct v64_reent __reent;
struct v64_reent *_v64_reent = &__reent;

v64si*
__v64si_signgam (void)
{
  return &_REENT_V64SI_SIGNGAM(_V64_REENT);
}
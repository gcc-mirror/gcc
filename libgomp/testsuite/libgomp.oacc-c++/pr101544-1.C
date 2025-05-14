// { dg-additional-options -fno-inline } for stable results regarding OpenACC 'routine'.
// But actually, as none of the '#pragma acc routine' syntax is accepted, force inlining:
#define ALWAYS_INLINE __attribute__((always_inline))

#include "../libgomp.c++/pr101544-1.C"
//TODO { dg-prune-output {using 'vector_length \(32\)', ignoring 1} }

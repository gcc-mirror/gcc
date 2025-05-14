// See also '../libgomp.oacc-c++/pr101544-1.C'.
#ifndef ALWAYS_INLINE
# define ALWAYS_INLINE
#endif

//===--- declare_target_base_class.cpp --------------------------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
//
// This test was suggested by members of NERSC. This test defines a declare
// target region which includes only a base class and a 'concrete' device
// pointer. 
// 
// Test suggestion comes from Chris Daily and Rahulkumar Gayatri from NERSC
////===----------------------------------------------------------------------===//

#include <new>
#include <vector>
#include <iostream>

#pragma omp declare target
//#pragma acc routine //TODO error: '#pragma acc routine' not immediately followed by function declaration or definition
class S {
public:
  //#pragma acc routine //TODO error: '#pragma acc routine' must be at file scope
  ALWAYS_INLINE
  S() : _devPtr(nullptr) {}
  //#pragma acc routine //TODO error: '#pragma acc routine' must be at file scope
  ALWAYS_INLINE
  double sag(double x, double y) {
    return x + y;
  }
  S* cloneToDevice() {
    S* ptr;
#pragma omp target map(ptr)
#pragma acc serial copy(ptr)
    {
      ptr = new S();
    }
    _devPtr = ptr;
    return ptr;
  }
private:
  S* _devPtr;
};
//#pragma acc routine (S) //TODO error: 'class S' does not refer to a function
//#pragma acc routine (S::S) //TODO error: '#pragma acc routine' names a set of overloads
//#pragma acc routine (S::sag) //TODO error: '#pragma acc routine' names a set of overloads
#pragma omp end declare target

int main() {
  int errors = 0;
  
  S s;
  S* devPtr = s.cloneToDevice();

  std::vector<double> in(10, 0.0);
  for(int i = 0; i < 10; i++) {
    in[i] = i;
  }

  std::vector<double> out(10, 0.0);

  double* inptr = in.data();
  double* outptr = out.data();

#pragma omp target teams distribute parallel for map(inptr[:10], outptr[:10]) is_device_ptr(devPtr)
#pragma acc parallel loop copy(inptr[:10], outptr[:10]) deviceptr(devPtr)
  for(int i = 0; i < 10; i++) {
    outptr[i] = devPtr->sag(inptr[i], inptr[i]);
  }

  for(int i = 0; i < 10; i++) {
    if (out[i] != i * 2)
      {
	++errors;
	std::cerr << "ERROR: " << "i = " << i << ": " << out[i] << " != " << (i * 2) << "\n";
      }
  }

  return errors ? 1 : 0;
}

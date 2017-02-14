/*

   matmul.c : Matrix Multiplication with tiling for openmp4 example

*/

#include <stdlib.h>
#include <math.h>

#define BLOCK_SIZE 16
/*
  #define BLOCK_SIZE 32
*/
#define NSECPERSEC 1000000000L

typedef struct {
   int width;
   int height;
   int stride;
   int hpad;
   float* elements;
} Matrix;

/* Correctly extract the number of nanoseconds from the two time structures */
long int get_nanosecs( struct timespec start_time, struct timespec end_time) {
   long int nanosecs;
   if ((end_time.tv_nsec-start_time.tv_nsec)<0) nanosecs =
      ((((long int) end_time.tv_sec- (long int) start_time.tv_sec )-1)*NSECPERSEC ) +
      ( NSECPERSEC + (long int) end_time.tv_nsec - (long int) start_time.tv_nsec) ;
   else nanosecs =
      (((long int) end_time.tv_sec- (long int) start_time.tv_sec )*NSECPERSEC ) +
      ( (long int) end_time.tv_nsec - (long int) start_time.tv_nsec );
   return nanosecs;
}

void simple_sgemm_tt(const int M,const int N,const int K,const float alpha, const float* A,const int LDA,
     const float* B,const int LDB, const float beta,float* C, const int LDC) ;
void simple_sgemm_tn(const int M,const int N,const int K,const float alpha, const float* A,const int LDA,
     const float* B,const int LDB, const float beta,float* C, const int LDC) ;
void  tiled_sgemm_tt(const int M,const int N,const int K,const float alpha, const float*A, const int LDA,
     const float* B,const int LDB, const float beta,float* C, const int LDC) ;

int verify(float* v_res, float* v_ref, int len) {
    int passed = 1;
    int i;
    for (i = 0; i < len; ++i) {
        if (fabs(v_res[i] - v_ref[i]) > 0.001*v_ref[i]) {
	  __builtin_abort ();
        }
    }
    return passed;
}


int main(int argc, char* argv[]){

   Matrix A,B,Bt,C,Cref;
   int a1,a2,a3,i,j;
   struct timespec start_time1, end_time1;
   struct timespec start_time2, end_time2;
   long int nanosecs,total_ops;
   float gflopsTiled,gflopsCPU;

   a1 = 35;
   a2 = 28;
   a3 = 47;

   A.height = a1;
   A.width = a2;
   A.stride = (((A.width-1)/BLOCK_SIZE)+1) * BLOCK_SIZE;
   A.hpad = (((A.height-1)/BLOCK_SIZE)+1) * BLOCK_SIZE;
   A.elements = (float*)malloc(A.stride * A.hpad* sizeof(float));

   B.height = a2;
   B.width = a3;
   B.stride = (((B.width-1)/BLOCK_SIZE)+1) * BLOCK_SIZE;
   B.hpad = (((B.height-1)/BLOCK_SIZE)+1) * BLOCK_SIZE;
   B.elements = (float*)malloc(B.stride * B.hpad * sizeof(float));

   /* Bt is same as B but stored in column-major order */
   Bt.height = B.height;
   Bt.width = B.width;
   Bt.stride = B.stride;
   Bt.hpad = B.hpad;
   Bt.elements = (float*)malloc(Bt.stride * Bt.hpad * sizeof(float));

   C.height = a1;
   C.width = a3;
   C.stride = (((C.width-1)/BLOCK_SIZE)+1) * BLOCK_SIZE;
   C.hpad = (((C.height-1)/BLOCK_SIZE)+1) * BLOCK_SIZE;
   C.elements = (float*)malloc(C.stride * C.hpad * sizeof(float));

   Cref.height = a1;
   Cref.width = a3;
   Cref.stride = (((Cref.width-1)/BLOCK_SIZE)+1) * BLOCK_SIZE;
   Cref.hpad = (((Cref.height-1)/BLOCK_SIZE)+1) * BLOCK_SIZE;
   Cref.elements = (float*)malloc(Cref.stride * Cref.hpad * sizeof(float));

   for(i = 0; i < A.hpad ; i++)
      for(j = 0; j < A.stride; j++) {
         if (( j<A.width ) && (i<A.height)) {
            A.elements[i*A.stride + j] = (i % 3);
         } else {
            A.elements[i*A.stride + j] = 0.0;
         }
      }

   /*  Initialize B and Bt */
   for(i = 0; i < B.hpad ; i++)
      for(j = 0; j < B.stride; j++) {
         if (( j<B.width ) && (i<B.height)) {
            B.elements[i*B.stride+j] = (j % 2);
            Bt.elements[j*Bt.stride+i] = B.elements[i*B.stride+j] ;
         } else {
            B.elements[i*B.stride+j] = 0.0;
            Bt.elements[j*Bt.stride+i] = 0.0;
         }
      }

   /* zero C, and Cref */
   for(i = 0; i < C.hpad; i++)
      for(j = 0; j < C.stride; j++) {
         C.elements[i*C.stride+j] = 0.0;
         Cref.elements[i*Cref.stride+j] = 0.0;
      }

   simple_sgemm_tt(A.height,B.width,B.height,1.0,A.elements,A.stride,B.elements,B.stride,1.0,Cref.elements,Cref.stride);
   tiled_sgemm_tt(A.height,B.width,B.height,1.0,A.elements,A.stride,B.elements,B.stride,1.0,C.elements,C.stride);

   verify(C.elements, Cref.elements, C.height * C.stride);
   return 0;
}

void simple_sgemm_tt(const int M,const int N,const int K,const float alpha, const float* A,const int LDA,
const float* B,const int LDB, const float beta,float* C, const int LDC) {
   /*  A,B, and C  are in row-major order */
   int c_row,c_col,inner;
   float sum;
   for (c_col  = 0 ;  c_col<N; c_col++ ) {
      for (c_row = 0 ; c_row<M; c_row++ ) {
         sum = 0.0 ;
         for (inner = 0 ; inner<K; inner++ ) {
            sum += A[c_row*LDA + inner] * B[inner*LDB + c_col] ;
         }
         C[c_row*LDC + c_col] = alpha*sum + beta*C[ c_row*LDC + c_col] ;
      }
   }
}

/***************************

   tiled_sgemm_tt:  Tiled matrix multiplication:

***************************/

void tiled_sgemm_tt(const int M, const int N, const int K, const float alpha, const float*A, const int LDA,
   const float*B, const int LDB, const float beta, float*C, const int LDC){

#pragma omp target teams map(to:A[M*K],B[K*N]) map(from:C[M*N])
#pragma omp distribute collapse(2)
   for (int C_row_start=0 ; C_row_start < M ; C_row_start+=BLOCK_SIZE)
      for (int C_col_start=0 ; C_col_start < N ; C_col_start+=BLOCK_SIZE)
	{
//       Each team has a local copy of these mini matrices
         float As[BLOCK_SIZE][BLOCK_SIZE];
         float Bs[BLOCK_SIZE][BLOCK_SIZE];
#pragma omp parallel
	 {
         int C_row, C_col;
         float Cval = 0.0;

         for (int kblock = 0; kblock  < K ; kblock += BLOCK_SIZE )
	   {
#pragma omp for collapse(2)
	     for (int row=0 ; row < BLOCK_SIZE ; row++)
               for (int col=0 ; col < BLOCK_SIZE ; col++)
		 {
		   C_row = C_row_start + row;
		   C_col = C_col_start + col;
		   if ((C_row < M) && (kblock + col < K))
		     As[row][col] = A[(C_row*LDA)+ kblock + col];
		   else
		     As[row][col] = 0;
		   if ((kblock + row < K) && C_col < N)
		     Bs[row][col] = B[((kblock+row)*LDB)+ C_col];
		   else
		     Bs[row][col] = 0;
		 }

#pragma omp for collapse(2)
	     for (int row=0 ; row < BLOCK_SIZE ; row++)
	       for (int col=0 ; col < BLOCK_SIZE ; col++)
		 {
		   for (int e = 0; e < BLOCK_SIZE; ++e)
                     Cval += As[row][e] * Bs[e][col];
		 }
	   }  /* End for kblock .. */


#pragma omp for collapse(2)
         for (int row=0 ; row < BLOCK_SIZE ; row++)
	   for (int col=0 ; col < BLOCK_SIZE ; col++)
	     {
               C_row = C_row_start + row;
               C_col = C_col_start + col;
	       if ((C_row < M) && (C_col < N))
		 C[(C_row*LDC)+C_col] = alpha*Cval + beta*C[(C_row*LDC)+C_col];

	     }
         } /* end parallel */
      }	   /* end target teams distribute */
}

// { dg-do run { target offload_device_nonshared_as } }

#include <string.h>
#include <assert.h>

volatile int yy = 4, zz = 2, str_str = 2;

template<typename T>
void foo()
{
  T *arr;
  int x = 5;
  T arr2d[10][10];

  arr = new T[100];

  /* Update whole reshaped array.  */

  memset (arr, 0, 100 * sizeof (T));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < x; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = i ^ j;

#pragma omp target update to(([10][x]) arr)

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if (j < x)
	assert (arr[j * 10 + i] == i ^ j);
      else
	assert (arr[j * 10 + i] == 0);


  /* Strided update.  */

  memset (arr, 0, 100 * sizeof (T));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 20; j++)
    for (int i = 0; i < 5; i++)
      arr[j * 5 + i] = i + j;

#pragma omp target update to(([5][5]) arr[0:3][0:3:2])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 20; j++)
    for (int i = 0; i < 5; i++)
      if (j < 3 && (i & 1) == 0 && i < 6)
	assert (arr[j * 5 + i] == i + j);
      else
	assert (arr[j * 5 + i] == 0);


  /* Reshaped update, contiguous.  */

  memset (arr, 0, 100 * sizeof (T));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 20; j++)
    for (int i = 0; i < 5; i++)
      arr[j * 5 + i] = 2 * j + i;

#pragma omp target update to(([5][5]) arr[0:5][0:5])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 20; j++)
    for (int i = 0; i < 5; i++)
      if (j < 5 && i < 5)
	assert (arr[j * 5 + i] == 2 * j + i);
      else
	assert (arr[j * 5 + i] == 0);


  /* Strided update on actual array.  */

  memset (arr2d, 0, 100 * sizeof (T));

#pragma omp target enter data map(to: arr2d)

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr2d[j][i] = j + 2 * i;

#pragma omp target update to(arr2d[0:5:2][5:2])

#pragma omp target exit data map(from: arr2d)

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if ((j & 1) == 0 && i >= 5 && i < 7)
	assert (arr2d[j][i] == j + 2 * i);
      else
	assert (arr2d[j][i] == 0);


  /* Update with non-constant bounds.  */

  memset (arr, 0, 100 * sizeof (T));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = (2 * j) ^ i;

  x = 3;
  int y = yy, z = zz, str = str_str;
  /* This is actually [0:3:2] [4:2:2].  */
#pragma omp target update to(([10][10]) arr[0:x:2][y:z:str])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if ((j & 1) == 0 && j < 6 && (i & 1) == 0 && i >= 4 && i < 8)
	assert (arr[j * 10 + i] == (2 * j) ^ i);
      else
	assert (arr[j * 10 + i] == 0);


  /* Update with full "major" dimension.  */

  memset (arr, 0, 100 * sizeof (T));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = i + j;

#pragma omp target update to(([10][10]) arr[0:10][3:1])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if (i == 3)
	assert (arr[j * 10 + i] == i + j);
      else
	assert (arr[j * 10 + i] == 0);


  /* Update with full "minor" dimension.  */

  memset (arr, 0, 100 * sizeof (T));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = 3 * (i + j);

#pragma omp target update to(([10][10]) arr[3:2][0:10])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if (j >= 3 && j < 5)
	assert (arr[j * 10 + i] == 3 * (i + j));
      else
	assert (arr[j * 10 + i] == 0);


  /* Rectangle update.  */

  memset (arr, 0, 100 * sizeof (T));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = 5 * (i + j);

#pragma omp target update to(([10][10]) arr[3:2][0:9])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if (j >= 3 && j < 5 && i < 9)
	assert (arr[j * 10 + i] == 5 * (i + j));
      else
	assert (arr[j * 10 + i] == 0);


  /* One-dimensional strided update.  */

  memset (arr, 0, 100 * sizeof (T));

#pragma omp target enter data map(to: arr[:100])

  for (int i = 0; i < 100; i++)
    arr[i] = i + 99;

#pragma omp target update to(([100]) arr[3:33:3])

#pragma omp target exit data map(from: arr[:100])

  for (int i = 0; i < 100; i++)
    if (i >= 3 && ((i - 3) % 3) == 0)
      assert (arr[i] == i + 99);
    else
      assert (arr[i] == 0);


  /* One-dimensional strided update without explicit array shape.  */

  memset (arr, 0, 100 * sizeof (T));

#pragma omp target enter data map(to: arr[:100])

  for (int i = 0; i < 100; i++)
    arr[i] = i + 121;

#pragma omp target update to(arr[3:33:3])

#pragma omp target exit data map(from: arr[:100])

  for (int i = 0; i < 100; i++)
    if (i >= 3 && ((i - 3) % 3) == 0)
      assert (arr[i] == i + 121);
    else
      assert (arr[i] == 0);

  delete[] arr;
}

int main()
{
  int *arr;
  int x = 5;
  int arr2d[10][10];

  arr = new int[100];

  /* Update whole reshaped array.  */

  memset (arr, 0, 100 * sizeof (int));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < x; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = i ^ j;

#pragma omp target update to(([10][x]) arr)

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if (j < x)
	assert (arr[j * 10 + i] == i ^ j);
      else
	assert (arr[j * 10 + i] == 0);


  /* Strided update.  */

  memset (arr, 0, 100 * sizeof (int));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 20; j++)
    for (int i = 0; i < 5; i++)
      arr[j * 5 + i] = i + j;

#pragma omp target update to(([5][5]) arr[0:3][0:3:2])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 20; j++)
    for (int i = 0; i < 5; i++)
      if (j < 3 && (i & 1) == 0 && i < 6)
	assert (arr[j * 5 + i] == i + j);
      else
	assert (arr[j * 5 + i] == 0);


  /* Reshaped update, contiguous.  */

  memset (arr, 0, 100 * sizeof (int));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 20; j++)
    for (int i = 0; i < 5; i++)
      arr[j * 5 + i] = 2 * j + i;

#pragma omp target update to(([5][5]) arr[0:5][0:5])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 20; j++)
    for (int i = 0; i < 5; i++)
      if (j < 5 && i < 5)
	assert (arr[j * 5 + i] == 2 * j + i);
      else
	assert (arr[j * 5 + i] == 0);


  /* Strided update on actual array.  */

  memset (arr2d, 0, 100 * sizeof (int));

#pragma omp target enter data map(to: arr2d)

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr2d[j][i] = j + 2 * i;

#pragma omp target update to(arr2d[0:5:2][5:2])

#pragma omp target exit data map(from: arr2d)

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if ((j & 1) == 0 && i >= 5 && i < 7)
	assert (arr2d[j][i] == j + 2 * i);
      else
	assert (arr2d[j][i] == 0);


  /* Update with non-constant bounds.  */

  memset (arr, 0, 100 * sizeof (int));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = (2 * j) ^ i;

  x = 3;
  int y = yy, z = zz, str = str_str;
  /* This is actually [0:3:2] [4:2:2].  */
#pragma omp target update to(([10][10]) arr[0:x:2][y:z:str])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if ((j & 1) == 0 && j < 6 && (i & 1) == 0 && i >= 4 && i < 8)
	assert (arr[j * 10 + i] == (2 * j) ^ i);
      else
	assert (arr[j * 10 + i] == 0);


  /* Update with full "major" dimension.  */

  memset (arr, 0, 100 * sizeof (int));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = i + j;

#pragma omp target update to(([10][10]) arr[0:10][3:1])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if (i == 3)
	assert (arr[j * 10 + i] == i + j);
      else
	assert (arr[j * 10 + i] == 0);


  /* Update with full "minor" dimension.  */

  memset (arr, 0, 100 * sizeof (int));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = 3 * (i + j);

#pragma omp target update to(([10][10]) arr[3:2][0:10])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if (j >= 3 && j < 5)
	assert (arr[j * 10 + i] == 3 * (i + j));
      else
	assert (arr[j * 10 + i] == 0);


  /* Rectangle update.  */

  memset (arr, 0, 100 * sizeof (int));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = 5 * (i + j);

#pragma omp target update to(([10][10]) arr[3:2][0:9])

#pragma omp target exit data map(from: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      if (j >= 3 && j < 5 && i < 9)
	assert (arr[j * 10 + i] == 5 * (i + j));
      else
	assert (arr[j * 10 + i] == 0);


  /* One-dimensional strided update.  */

  memset (arr, 0, 100 * sizeof (int));

#pragma omp target enter data map(to: arr[:100])

  for (int i = 0; i < 100; i++)
    arr[i] = i + 99;

#pragma omp target update to(([100]) arr[3:33:3])

#pragma omp target exit data map(from: arr[:100])

  for (int i = 0; i < 100; i++)
    if (i >= 3 && ((i - 3) % 3) == 0)
      assert (arr[i] == i + 99);
    else
      assert (arr[i] == 0);


  /* One-dimensional strided update without explicit array shape.  */

  memset (arr, 0, 100 * sizeof (int));

#pragma omp target enter data map(to: arr[:100])

  for (int i = 0; i < 100; i++)
    arr[i] = i + 121;

#pragma omp target update to(arr[3:33:3])

#pragma omp target exit data map(from: arr[:100])

  for (int i = 0; i < 100; i++)
    if (i >= 3 && ((i - 3) % 3) == 0)
      assert (arr[i] == i + 121);
    else
      assert (arr[i] == 0);

  delete[] arr;

  foo<long> ();

  return 0;
}

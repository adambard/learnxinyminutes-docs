---
category: tool
tool: OpenMP
filename: learnopenMP.cpp
contributors:
    - ["Cillian Smith", "https://github.com/smithc36-tcd"]
---

**OpenMP** is a library used for parallel programming on shared-memory machines.
OpenMP allows you to use simple high-level constructs for parallelism,
while hiding the details, keeping it easy to use and quick to write.
OpenMP is supported by C, C++, and Fortran.

## Structure

Generally an OpenMP program will use the following structure.

- **Master**: Start a Master thread, which will be used to set up the environment and
initialize variables

- **Slave**: Slave threads are created for sections of code which are marked by a special
directive, these are the threads which will run the parallel sections.

Each thread will have its own ID which can be obtained using the function
`omp_get_thread_num()`, but more on that later.

```
          __________ Slave
         /__________ Slave
        /
Master ------------- Master
        \___________ Slave
         \__________ Slave

```

## Compiling and running OpenMP

A simple "hello world" program can be parallelized using the `#pragma omp parallel` directive

```cpp
#include <stdio.h>

int main() {
    #pragma omp parallel
    {
        printf("Hello, World!\n");
    }
    return 0;
}
```

Compile it like this

```bash
# The OpenMP flat depends on the compiler
# intel : -openmp
# gcc : -fopenmp
# pgcc : -mp
gcc -fopenmp hello.c -o Hello
```

Running it should output

```
Hello, World!
...
Hello, World!
```

The exact number of "`Hello, Worlds`" depends on the number of cores of your machine,
for example I got 12 my laptop.

## Threads and processes

You can change the default number of threads using `export OMP_NUM_THREADS=8`

Here are some useful library functions in the `omp.h` library

```cpp
// Check the number of threads
printf("Max Threads: %d\n", omp_get_max_threads());
printf("Current number of threads: %d\n", omp_get_num_threads());
printf("Current Thread ID: %d\n", omp_get_thread_num());

// Modify the number of threads
omp_set_num_threads(int);

// Check if we are in a parallel region
omp_in_parallel();

// Dynamically vary the number of threads
omp_set_dynamic(int);
omp_get_dynamic();

// Check the number of processors
printf("Number of processors: %d\n", omp_num_procs());
```

## Private and shared variables

```cpp
// Variables in parallel sections can be either private or shared.

/* Private variables are private to each thread, as each thread has its own
* private copy. These variables are not initialized or maintained outside
* the thread.
*/
#pragma omp parallel private(x, y)

/* Shared variables are visible and accessible by all threads. By default,
* all variables in the work sharing region are shared except the loop
* iteration counter.
*
* Shared variables should be used with care as they can cause race conditions.
*/
#pragma omp parallel shared(a, b, c)

// They can be declared together as follows
#pragma omp parallel private(x, y) shared(a,b,c)
```

## Synchronization

OpenMP provides a number of directives to control the synchronization of threads

```cpp
#pragma omp parallel {

    /* `critical`: the enclosed code block will be executed by only one thread
     * at a time, and not simultaneously executed by multiple threads. It is
     * often used to protect shared data from race conditions.
     */
    #pragma omp critical
    data += data + computed;


    /* `single`: used when a block of code needs to be run by only a single
     * thread in a parallel section. Good for managing control variables.
     */
    #pragma omp single
    printf("Current number of threads: %d\n", omp_get_num_threads());

    /*  `atomic`: Ensures that a specific memory location is updated atomically
     *  to avoid race conditions.  */
    #pragma omp atomic
    counter += 1;


    /* `ordered`: the structured block is executed in the order in which
     * iterations would be executed in a sequential loop     */
    #pragma omp for ordered
    for (int i = 0; i < N; ++i) {
        #pragma omp ordered
        process(data[i]);
    }


    /* `barrier`: Forces all threads to wait until all threads reach this point
     * before proceeding.  */
    #pragma omp barrier

    /* `nowait`: Allows threads to proceed with their next task without waiting
     * for other threads to complete the current one. */
    #pragma omp for nowait
    for (int i = 0; i < N; ++i) {
        process(data[i]);
    }

    /* `reduction` : Combines the results of each thread's computation into a
     * single result.  */
    #pragma omp parallel for reduction(+:sum)
    for (int i = 0; i < N; ++i) {
        sum += a[i] * b[i];
    }

}
```

Example of the use of `barrier`

```c
#include <omp.h>
#include <stdio.h>

int main() {

  // Current number of active threads
  printf("Num of threads is %d\n", omp_get_num_threads());

#pragma omp parallel
  {
      // Current thread ID
      printf("Thread ID: %d\n", omp_get_thread_num());

#pragma omp barrier <--- Wait here until other threads have returned
      if(omp_get_thread_num() == 0)
      {
          printf("\nNumber of active threads: %d\n", omp_get_num_threads());
      }
  }
  return 0;
}
```

## Parallelizing Loops

It is simple to parallelise loops using OpenMP. Using a work-sharing directive we can do the following

```c
#pragma omp parallel
{
    #pragma omp for
    // for loop to be parallelized
    for() ...
}
```

A loop must be easily parallelisable for OpenMP to unroll and facilitate the assignment amoungst threads.
If there are any data dependancies between one iteration to the next, OpenMP can't parallelise it.

## Speed Comparison

The following is a C++ program which compares parallelised code with serial code

```cpp

#include <iostream>
#include <vector>
#include <ctime>
#include <chrono>
#include <omp.h>

int main() {
    const int num_elements = 100000000;

    std::vector<double> a(num_elements, 1.0);
    std::vector<double> b(num_elements, 2.0);
    std::vector<double> c(num_elements, 0.0);

    // Serial version
    auto start_time = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < num_elements; i++) {
        c[i] = a[i] * b[i];
    }
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration_serial = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time).count();

    // Parallel version with OpenMP
    start_time = std::chrono::high_resolution_clock::now();
    #pragma omp parallel for
    for (int i = 0; i < num_elements; i++) {
        c[i] = a[i] * b[i];
    }
    end_time = std::chrono::high_resolution_clock::now();
    auto duration_parallel = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time).count();

    std::cout << "Serial execution time: " << duration_serial << " ms" << std::endl;
    std::cout << "Parallel execution time: " << duration_parallel << " ms" << std::endl;
    std::cout << "Speedup: " << static_cast<double>(duration_serial) / duration_parallel << std::endl;

    return 0;
}
```

This results in

```
Serial execution time: 488 ms
Parallel execution time: 148 ms
Speedup: 3.2973
```

It should be noted that this example is fairly contrived and the actual speedup
depends on implementation and it should also be noted that serial code may run
faster than parallel code due to cache preformace.

## Example

The following example uses OpenMP to calculate the Mandlebrot set

```cpp
#include <iostream>
#include <fstream>
#include <complex>
#include <vector>
#include <omp.h>

const int width = 2000;
const int height = 2000;
const int max_iterations = 1000;

int mandelbrot(const std::complex<double> &c) {
    std::complex<double> z = c;
    int n = 0;
    while (abs(z) <= 2 && n < max_iterations) {
        z = z * z + c;
        n++;
    }
    return n;
}

int main() {
    std::vector<std::vector<int>> values(height, std::vector<int>(width));

    // Calculate the Mandelbrot set using OpenMP
    #pragma omp parallel for schedule(dynamic)
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            double real = (x - width / 2.0) * 4.0 / width;
            double imag = (y - height / 2.0) * 4.0 / height;
            std::complex<double> c(real, imag);

            values[y][x] = mandelbrot(c);
        }
    }

    // Prepare the output image
    std::ofstream image("mandelbrot_set.ppm");
    image << "P3\n" << width << " " << height << " 255\n";

    // Write the output image in serial
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            int value = values[y][x];
            int r = (value % 8) * 32;
            int g = (value % 16) * 16;
            int b = (value % 32) * 8;

            image << r << " " << g << " " << b << " ";
        }
        image << "\n";
    }

    image.close();
    std::cout << "Mandelbrot set image generated as mandelbrot_set.ppm." << std::endl;

    return 0;
}
```

## Resources

- [Intro to parallel programming](https://tildesites.bowdoin.edu/~ltoma/teaching/cs3225-GIS/fall17/Lectures/openmp.html)
- [Tutorials currated by OpenMP](https://www.openmp.org/resources/tutorials-articles/)
- [OpenMP cheatsheet](https://www.openmp.org/wp-content/uploads/OpenMPRefCard-5-2-web.pdf)

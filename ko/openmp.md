---
category: tool
name: OpenMP
filename: learnopenMP.cpp
contributors:
    - ["Cillian Smith", "https://github.com/smithc36-tcd"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

**OpenMP**는 공유 메모리 머신에서 병렬 프로그래밍에 사용되는 라이브러리입니다.
OpenMP를 사용하면 병렬 처리를 위한 간단한 고수준 구문을 사용할 수 있으며,
세부 사항을 숨겨 사용하기 쉽고 빠르게 작성할 수 있습니다.
OpenMP는 C, C++ 및 Fortran에서 지원됩니다.

## 구조

일반적으로 OpenMP 프로그램은 다음 구조를 사용합니다.

- **마스터**: 마스터 스레드를 시작하여 환경을 설정하고 변수를 초기화합니다.

- **슬레이브**: 슬레이브 스레드는 특수 지시문으로 표시된 코드 섹션에 대해 생성되며, 이들은 병렬 섹션을 실행할 스레드입니다.

각 스레드는 `omp_get_thread_num()` 함수를 사용하여 얻을 수 있는 자체 ID를 가집니다. 자세한 내용은 나중에 설명합니다.

```
          __________ 슬레이브
         /__________ 슬레이브
        /
마스터 ------------- 마스터
        \___________ 슬레이브
         \__________ 슬레이브

```

## OpenMP 컴파일 및 실행

간단한 "hello world" 프로그램은 `#pragma omp parallel` 지시문을 사용하여 병렬화할 수 있습니다.

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

다음과 같이 컴파일하십시오.

```bash
# OpenMP 플래그는 컴파일러에 따라 다릅니다.
# intel : -openmp
# gcc : -fopenmp
# pgcc : -mp
gcc -fopenmp hello.c -o Hello
```

실행하면 다음과 같이 출력되어야 합니다.

```
Hello, World!
...
Hello, World!
```

"Hello, World!"의 정확한 수는 컴퓨터의 코어 수에 따라 다릅니다. 예를 들어, 제 노트북에서는 12개가 나왔습니다.

## 스레드 및 프로세스

`export OMP_NUM_THREADS=8`을 사용하여 기본 스레드 수를 변경할 수 있습니다.

다음은 `omp.h` 라이브러리의 유용한 함수입니다.

```cpp
// 스레드 수 확인
printf("Max Threads: %d\n", omp_get_max_threads());
printf("Current number of threads: %d\n", omp_get_num_threads());
printf("Current Thread ID: %d\n", omp_get_thread_num());

// 스레드 수 수정
omp_set_num_threads(int);

// 병렬 영역에 있는지 확인
omp_in_parallel();

// 스레드 수를 동적으로 변경
omp_set_dynamic(int);
omp_get_dynamic();

// 프로세서 수 확인
printf("Number of processors: %d\n", omp_num_procs());
```

## 비공개 및 공유 변수

```cpp
// 병렬 섹션의 변수는 private 또는 shared일 수 있습니다.

/* private 변수는 각 스레드에 대해 private이며, 각 스레드는 자체
* private 복사본을 가집니다. 이러한 변수는 스레드 외부에서 초기화되거나 유지되지 않습니다.
*/
#pragma omp parallel private(x, y)

/* shared 변수는 모든 스레드에서 볼 수 있고 액세스할 수 있습니다. 기본적으로
* 작업 공유 영역의 모든 변수는 루프 반복 카운터를 제외하고 공유됩니다.
*
* shared 변수는 경쟁 조건을 유발할 수 있으므로 주의해서 사용해야 합니다.
*/
#pragma omp parallel shared(a, b, c)

// 다음과 같이 함께 선언할 수 있습니다.
#pragma omp parallel private(x, y) shared(a,b,c)
```

## 동기화

OpenMP는 스레드 동기화를 제어하는 여러 지시문을 제공합니다.

```cpp
#pragma omp parallel {

    /* `critical`: 묶인 코드 블록은 한 번에 하나의 스레드만 실행하며,
     * 여러 스레드에서 동시에 실행되지 않습니다. 이는 종종
     * 경쟁 조건으로부터 공유 데이터를 보호하는 데 사용됩니다.
     */
    #pragma omp critical
    data += data + computed;


    /* `single`: 병렬 섹션에서 단일 스레드만 실행해야 하는 코드 블록에 사용됩니다.
     * 제어 변수를 관리하는 데 좋습니다.
     */
    #pragma omp single
    printf("Current number of threads: %d\n", omp_get_num_threads());

    /*  `atomic`: 경쟁 조건을 피하기 위해 특정 메모리 위치가 원자적으로 업데이트되도록 보장합니다.
     *  */
    #pragma omp atomic
    counter += 1;


    /* `ordered`: 구조화된 블록은 순차 루프에서 반복이 실행되는 순서대로 실행됩니다.
     *     */
    #pragma omp for ordered
    for (int i = 0; i < N; ++i) {
        #pragma omp ordered
        process(data[i]);
    }


    /* `barrier`: 모든 스레드가 진행하기 전에 모든 스레드가 이 지점에 도달할 때까지 기다리도록 강제합니다.
     *  */
    #pragma omp barrier

    /* `nowait`: 스레드가 다른 스레드가 현재 작업을 완료할 때까지 기다리지 않고 다음 작업을 진행할 수 있도록 합니다.
     *     */
    #pragma omp for nowait
    for (int i = 0; i < N; ++i) {
        process(data[i]);
    }

    /* `reduction` : 각 스레드의 계산 결과를 단일 결과로 결합합니다.
     *  */
    #pragma omp parallel for reduction(+:sum)
    for (int i = 0; i < N; ++i) {
        sum += a[i] * b[i];
    }

}
```

`barrier` 사용 예시

```c
#include <omp.h>
#include <stdio.h>

int main() {

  // 현재 활성 스레드 수
  printf("Num of threads is %d\n", omp_get_num_threads());

#pragma omp parallel
  {
      // 현재 스레드 ID
      printf("Thread ID: %d\n", omp_get_thread_num());

#pragma omp barrier <--- 다른 스레드가 반환될 때까지 여기서 기다립니다.
      if(omp_get_thread_num() == 0)
      {
          printf("\nNumber of active threads: %d\n", omp_get_num_threads());
      }
  }
  return 0;
}
```

## 루프 병렬화

OpenMP를 사용하여 루프를 병렬화하는 것은 간단합니다. 작업 공유 지시문을 사용하여 다음을 수행할 수 있습니다.

```c
#pragma omp parallel
{
    #pragma omp for
    // 병렬화할 for 루프
    for() ...
}
```

OpenMP가 스레드 간 할당을 풀고 용이하게 하려면 루프가 쉽게 병렬화될 수 있어야 합니다.
한 반복에서 다음 반복으로 데이터 종속성이 있는 경우 OpenMP는 병렬화할 수 없습니다.

## 속도 비교

다음은 병렬화된 코드와 직렬 코드를 비교하는 C++ 프로그램입니다.

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

    // 직렬 버전
    auto start_time = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < num_elements; i++) {
        c[i] = a[i] * b[i];
    }
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration_serial = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time).count();

    // OpenMP를 사용한 병렬 버전
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

결과는 다음과 같습니다.

```
직렬 실행 시간: 488 ms
병렬 실행 시간: 148 ms
속도 향상: 3.2973
```

이 예제는 다소 인위적이며 실제 속도 향상은 구현에 따라 다르며, 캐시 성능으로 인해 직렬 코드가 병렬 코드보다 더 빠르게 실행될 수 있다는 점에 유의해야 합니다.

## 예제

다음 예제는 OpenMP를 사용하여 만델브로트 집합을 계산합니다.

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

    // OpenMP를 사용하여 만델브로트 집합 계산
    #pragma omp parallel for schedule(dynamic)
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            double real = (x - width / 2.0) * 4.0 / width;
            double imag = (y - height / 2.0) * 4.0 / height;
            std::complex<double> c(real, imag);

            values[y][x] = mandelbrot(c);
        }
    }

    // 출력 이미지 준비
    std::ofstream image("mandelbrot_set.ppm");
    image << "P3\n" << width << " " << height << " 255\n";

    // 직렬로 출력 이미지 쓰기
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

## 자료

- [병렬 프로그래밍 소개](https://tildesites.bowdoin.edu/~ltoma/teaching/cs3225-GIS/fall17/Lectures/openmp.html)
- [OpenMP에서 큐레이션한 튜토리얼](https://www.openmp.org/resources/tutorials-articles/)
- [OpenMP 치트시트](https://www.openmp.org/wp-content/uploads/OpenMPRefCard-5-2-web.pdf)
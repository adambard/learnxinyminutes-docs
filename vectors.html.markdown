---
category: Algorithms & Data Structures
name: Vectors
contributors:
    - ["Abhshek Jaisingh", "http://github.com/abhishekjiitr"]
---

# Vectors

## What are they?

Vectors are sequence containers representing arrays that can change in size.

Just like arrays, vectors use contiguous storage locations for their elements, which means that their elements can also be accessed using offsets on regular pointers to its elements, and just as efficiently as in arrays. But unlike arrays, their size can change dynamically, with their storage being handled automatically by the container.

## How do vectors work internally?

Internally, vectors use a dynamically allocated array to store their elements. This array may need to be reallocated in order to grow in size when new elements are inserted, which implies allocating a new array and moving all elements to it. This is a relatively expensive task in terms of processing time, and thus, vectors do not reallocate each time an element is added to the container.

Instead, vector containers may allocate some extra storage to accommodate for possible growth, and thus the container may have an actual capacity greater than the storage strictly needed to contain its elements (i.e., its size). Libraries can implement different strategies for growth to balance between memory usage and reallocations, but in any case, reallocations should only happen at logarithmically growing intervals of size so that the insertion of individual elements at the end of the vector can be provided with amortized constant time complexity (see push_back).


## Comparison with arrays

Compared to arrays, vectors consume more memory in exchange for the ability to manage storage and grow dynamically in an efficient way.

## Vector implementation and working

### Types of functions, limits, and simplification

```
// create a vector to store int
   vector<int> vec; 
   int i;

// display the original size of vec
   cout << "vector size = " << vec.size() << endl;

// push 5 values into the vector
   for(i = 0; i < 5; i++){
      vec.push_back(i);
   }

// display extended size of vec
   cout << "extended vector size = " << vec.size() << endl;

// access 5 values from the vector
   for(i = 0; i < 5; i++){
      cout << "value of vec [" << i << "] = " << vec[i] << endl;
   }

// use iterator to access the values
   vector<int>::iterator v = vec.begin();
   while( v != vec.end()) {
      cout << "value of v = " << *v << endl;
      v++;
   }

```

### Ending Notes

Vectors are very commonly used data structures in all the languages for their extended functionality of dynamic size.
Creating your own custom vectors is a challenge for you after you complete this tutorial.


## Online Resources

* [Tutorialspoint](http://www.tutorialspoint.com/cplusplus/cpp_stl_tutorial.htm)
* [cplusplus.com](http://www.cplusplus.com/reference/vector/vector/)

---
category: Algorithms & Data Structures
name: Binary Search
contributors:
    - ["Abhishek Jaisingh", "http://github.com/abhishekjiitr"]
---

# Binary Search

## Why Binary Search?

Searching is one of the prime problems in the domain of Computer Science. Today there are more than 1 trillion searches per year, and we need algorithms that can do that very fastly. Binary search is one of the fundamental algorithms in computer science. In order to explore it, we’ll first build up a theoretical backbone, then use that to implement the algorithm properly.

## Introduction

A simple approach to implement search is to do a linear search, but this approach takes a lot of time and this time grows linearly with the amount or number of data points. i.e., start from the leftmost element of arr[] and one by one compare x with each element of arr[], if x matches with an element, return the index. If x doesn’t match with any of elements, return -1.

```
Linear Search: O (n)               Linear Time

Binary Search: O ( log(n) )		   Logarithmic Time

```
```
def search(arr, x):
 
    for i in range(len(arr)):
 
        if arr[i] == x:
            return i
 
    return -1

```
## Binary Search Algorithm

The basic requirement for binary search to work is that the data to search should be sorted (in any order).
### Algo

```
The idea of binary search is to use the information that the array is sorted and reduce the time complexity to O(Logn). We basically ignore half of the elements just after one comparison.
1) Compare x with the middle element.
2) If x matches with middle element, we return the mid index.
3) Else If x is greater than the mid element, then x can only lie in right half subarray after the mid element. So we recur for right half.
4) Else (x is smaller) recur for the left half.
Following is Recursive implementation of Binary Search.

```

### Ending Notes

There is another form of binary search that is very useful.

## Books

* [CLRS](https://mitpress.mit.edu/books/introduction-algorithms)
* [Algorithms](http://www.amazon.com/Algorithms-4th-Robert-Sedgewick/dp/032157351X)
* [Algorithm Design](http://www.amazon.com/Algorithm-Design-Foundations-Analysis-Internet/dp/0471383651)

## Online Resources

* [GeeksforGeeks](http://www.geeksforgeeks.org/the-ubiquitous-binary-search-set-1/)
* [Topcoder Tutorial](https://www.topcoder.com/community/data-science/data-science-tutorials/binary-search/)

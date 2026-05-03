# GitHub Copilot documentation

This documentation is generated with GitHub Copilot to show what the tool can do.

##
### Example 1: Generate a function to calculate the factorial of a number

```pythondef factorial(n):
    if n == 0:
        return 1
    else:
        return n * factorial(n - 1)
``` 
### Example 2: Generate a function to check if a number is prime

```pythondef is_prime(n):
    if n <= 1:
        return False
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return False
    return True
```
### Example 3: Generate a function to sort a list of numbers

```pythondef bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        for j in range(0, n-i-1):
            if arr[j] > arr[j+1]:
                arr[j], arr[j+1] = arr[j+1], arr[j]
    return arr
```
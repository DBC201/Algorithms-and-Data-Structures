
def swap_index(l, a, b):
    temp = l[a]
    l[a] = l[b]
    l[b] = temp
            
def non_recursive_hp(n, A):
    c = [0]*n
            
    print(A)
    
    i = 0
    while i < n:
        if c[i] < i:
            if i % 2 == 0:
                swap_index(A, 0, i)
            else:
                swap_index(A, c[i], i)
            print(A)
            c[i] += 1
            i = 0
        else:
            c[i] = 0
            i += 1          
            
def heap_permutation(A, n):
    if n == 1:
        print(A)
        return
    for c in range(n):
        heap_permutation(A, n-1)
        if c%2 == 0:
            swap_index(A, c, n-1)
        else:
            swap_index(A, 0, n-1)
        
def permutation(A, P):
    if len(A) == 0:
        print(P)
        return
    for c in range(len(A)):
        a = A[:c] + A[c+1:]
        p = list(P)
        p.append(A[c])
        permutation(a, p)
            
array = [1,2,3]          
non_recursive_hp(len(array), array)
print()
heap_permutation(array, len(array))
print()
permutation(array,[])
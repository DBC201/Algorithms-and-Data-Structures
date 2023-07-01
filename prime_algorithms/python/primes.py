import random
def gcd(*args):
    numbers=[]
    for arg in args:
        numbers.append(arg)
    numbers.sort()
    for c in range(numbers[0], 1, -1):
        divisor_count = 0
        for e in numbers:
            if e%c==0:
                divisor_count +=1
        if divisor_count == len(numbers):
            return c
    return False


def fermat_composite(n, k=50, r=55):
    for c in range(k):
        a = random.randint(2,r)
        while a%n == 0:
            a=random.randint(2,r)
        try:
            f = (a**(n-1)) % n
        except OverflowError:
            print("Number too big")
            continue
        if f != 1:
            return True
    return False

def euler_composite(n, k=50, r=55):
    for c in range(k):
        a = random.randint(2,r)
        while a%n == 0:
            a = random.randint(2,r)
        try:
            p = (n-1)//2
            f = (a**p) % n
        except OverflowError:
            print("Number too big")
            continue
        if f != 1 and f != n-1:
            return True
    return False

def factor_2(n):
    power = 0
    while n%2 == 0:
        n//=2
        power += 1
    return power, n

def rabin_miller_probable_prime(n, k=50, r=55):
    s, m = factor_2(n - 1)#n-1=(2^s)*m
    for c in range(k):
        prime_check = False
        a = random.randint(2,r)
        while a%n==0:
            a = random.randint(2,r)
        f = (a**m) % n
        if f == 1 or f == n-1:
            prime_check = True #serves no purpose
            continue
        for d in range(s-1):
            f = f**2 % n
            if f == n-1:
                prime_check = True
                break
        if prime_check:
            continue
        else:
            return False
    return True

if __name__ == "__main__":
    n=43
    print(fermat_composite(n))
    print(euler_composite(n))
    print(rabin_miller_probable_prime(n))

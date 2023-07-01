#include <stdio.h>
#include <stdlib.h>

int main()
{
    int c,d;
    int *numbers;
    int count;
    int prime=0;
    printf("Enter the number that you want to find the primes up to:");
    scanf(" %d", &count);
    numbers=(int*)calloc(count+1,sizeof(int));
    FILE *primes;
    primes=fopen("primes.txt","w");
    FILE *nonprimes;
    nonprimes=fopen("removed multiples.txt", "w");
    for(c=0;c<=count;c++){
        numbers[c]=c;
    }
    for(c=2;c<=count;c++)
    {
        int nProduct=0;
        for(d=c+1;d<=count;d++)
        {
            if(numbers[d]==0)
            {
                continue;
            }else if(d%c==0)
            {
                fprintf(nonprimes,"\n%d",d);
                nProduct++;
                numbers[d]=0;
            }
        }
        if(nProduct!=0){
        fprintf(nonprimes,"\nRemoved multiples of %d:%d(including %d)",c,nProduct+1,c);
        fprintf(nonprimes,"\n________________________\n");
        }
    }
    for(c=2;c<=count;c++){
        if(numbers[c]!=0){
        fprintf(primes,"%d\n",numbers[c]);
        prime++;
        }
    }
    fprintf(primes,"The number of primes found:%d", prime);
    fclose(nonprimes);
    fclose(primes);
    free(numbers);
    return 0;
}

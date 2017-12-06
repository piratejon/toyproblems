#include <stdlib.h>
#include <stdio.h>

int part1(int * input, int len) {
  int c, jmprel, i = 0;

  c = 0;
  while (i >= 0 && i < len) {
    jmprel = input[i];
    input[i] += 1;
    i += jmprel;
    c += 1;
  }

  return c;
}

int part2(int * input, int len) {
  int c, jmprel, i = 0;

  c = 0;
  while (i >= 0 && i < len) {
    jmprel = input[i];
    if (jmprel >= 3) {
      input[i] -= 1;
    } else {
      input[i] += 1;
    }
    i += jmprel;
    c += 1;
  }

  return c;
}

int main (int arfc, char ** arfv) {

  int * input = malloc(sizeof(*input)*2000);

  int i = 0;
  while(1 == scanf("%d", input+i)) {
    i += 1;
  }

  // printf("part1: %d\n", part1(input, i));
  printf("part2: %d\n", part2(input, i));

  free(input);

  return 0;
}

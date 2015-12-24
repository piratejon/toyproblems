#include <stdio.h>

char LIGHTS_A[100][100], LIGHTS_B[100][100];

int count_on(char (*lights)[100][100]) {
  int i, j, on;
  on = 0;
  for (i = 0; i < 100; i += 1) {
    for (j = 0; j < 100; j += 1) {
      on += (((*lights)[i][j] == '#') ? 1 : 0);
    }
  }

  return on;
}

void load_data_file(const char * filename, char (*lights)[100][100]) {
  int i;
  FILE * fin;

  fin = fopen(filename, "r");

  for (i = 0; i < 100; i += 1) {
    fgets((*lights)[i], 100, fin);
  }

  fclose(fin);
}

int count_on_neighbors (char (*grid)[100][100], int I, int J) {
  int i, j, count;

  count = 0;

  for (i = I - 1; i <= I + 1; i += 1) {
    for (j = J - 1; j <= J + 1; j += 1) {
      if (i >= 0 && j >= 0 && i < 100 && j < 100 && (*grid)[i][j] == '#') {
        count += 1;
      }
    }
  }

  return count;
}

void do_steps(char (*current)[100][100], char (*new)[100][100]) {
  int i, j, count;

  for (i = 0; i < 100; i += 1) {
    for (j = 0; j < 100; j += 1) {
      count = count_on_neighbors(current, i, j);
      if ((*current)[i][j] == '#') {
        if (count != 2 && count != 3) {
          (*new)[i][j] = '.';
        }
      } else {
        if (count == 3) {
          (*new)[i][j] = '#';
        }
      }
    }
  }
}

void do_a_bunch_of_steps(int i) {
  char (*lights_a)[100][100], (*lights_b)[100][100], (*lights_tmp)[100][100];
  lights_a = &LIGHTS_A;
  lights_b = &LIGHTS_B;
  while (i -- > 0) {
    do_steps(lights_a, lights_b);

    lights_tmp = lights_a;
    lights_a = lights_b;
    lights_b = lights_tmp;
  }
}

int main (int arfc, char ** arfv) {

  load_data_file(arfv[1], &LIGHTS_A);

  do_a_bunch_of_steps(100);

  printf("%d\n", count_on(&LIGHTS_A));

  return 0;
}


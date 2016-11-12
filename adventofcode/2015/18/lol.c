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
    // + 2, for the carriage return and terminal
    fgets((*lights)[i], 100 + 2, fin);
  }

  fclose(fin);
}

int count_on_neighbors (char (*grid)[100][100], int I, int J) {
  int i, j, count;

  count = 0;

  // // printf("counting neighbors for (%d,%d):\n", I, J);

  for (i = I - 1; i <= I + 1; i += 1) {
    for (j = J - 1; j <= J + 1; j += 1) {
      if ((i != I || j != J) && i >= 0 && j >= 0 && i < 100 && j < 100 && (*grid)[i][j] == '#') {
        // // printf("  (%d,%d) is on\n", i, j);
        count += 1;
      }
    }
  }

  // // printf("  (%d,%d) has %d on neighbors\n", I, J, count);

  return count;
}

void print_board(char (*g)[100][100]) {
  int i, j;

  for (i = 0; i < 100; i += 1) {
    for (j = 0; j < 100; j += 1) {
      // printf("%c", (*g)[i][j]);
    }
    // printf("\n");
  }
}

void do_steps(char (*current)[100][100], char (*new)[100][100]) {
  int i, j, count;

  for (i = 0; i < 100; i += 1) {
    for (j = 0; j < 100; j += 1) {
      count = count_on_neighbors(current, i, j);
      // printf("(%d,%d)", i, j);
      if ((*current)[i][j] == '#') {
        // printf(" is on");
        if (count == 2 || count == 3) {
          // printf(" and stays on");
          (*new)[i][j] = '#';
        } else {
          // printf(" and turns off");
          (*new)[i][j] = '.';
        }
      } else {
        // printf(" is off");
        if (count == 3) {
          // printf(" and turns on");
          (*new)[i][j] = '#';
        } else {
          // printf(" and stays off");
          (*new)[i][j] = '.';
        }
      }
      // printf(" (%d)\n", count);
    }
  }
  (*new)[0][0] = '#';
  (*new)[0][100 - 1] = '#';
  (*new)[100 - 1][0] = '#';
  (*new)[100 - 1][100 - 1] = '#';
}

void do_a_bunch_of_steps(int i) {
  char (*lights_a)[100][100], (*lights_b)[100][100], (*lights_tmp)[100][100];
  lights_a = &LIGHTS_A;
  lights_b = &LIGHTS_B;
  while (i -- > 0) {
    do_steps(lights_a, lights_b);
    // printf("step:\n");
    print_board(lights_b);
    // printf("\n");
    lights_tmp = lights_a;
    lights_a = lights_b;
    lights_b = lights_tmp;
  }
  printf("%d\n", count_on(lights_a));
}

void init_board(char (*g)[100][100]) {
  int i, j;
  for (i = 0; i < 100; i += 1) {
    for (j = 0; j < 100; j += 1) {
      (*g)[i][j] = '.';
    }
  }

  (*g)[0][0] = '#';
  (*g)[0][100 - 1] = '#';
  (*g)[100 - 1][0] = '#';
  (*g)[100 - 1][100 - 1] = '#';
}

int main (int arfc, char ** arfv) {

  load_data_file(arfv[1], &LIGHTS_A);
  LIGHTS_A[0][0] = '#';
  LIGHTS_A[0][100 - 1] = '#';
  LIGHTS_A[100 - 1][0] = '#';
  LIGHTS_A[100 - 1][100 - 1] = '#';

  init_board(&LIGHTS_B);

  // printf("initial board\n");
  print_board(&LIGHTS_A);
  // printf("\n");

  do_a_bunch_of_steps(100);

  return 0;
}


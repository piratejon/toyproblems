#include <stdio.h>

char LIGHTS_A[6][6], LIGHTS_B[6][6];

int count_on(char (*lights)[6][6]) {
  int i, j, on;
  on = 0;
  for (i = 0; i < 6; i += 1) {
    for (j = 0; j < 6; j += 1) {
      on += (((*lights)[i][j] == '#') ? 1 : 0);
    }
  }

  return on;
}

void load_data_file(const char * filename, char (*lights)[6][6]) {
  int i;
  FILE * fin;

  fin = fopen(filename, "r");

  for (i = 0; i < 6; i += 1) {
    // + 2, for the carriage return and terminal
    fgets((*lights)[i], 6 + 2, fin);
  }

  fclose(fin);
}

int count_on_neighbors (char (*grid)[6][6], int I, int J) {
  int i, j, count;

  count = 0;

  printf("counting neighbors for (%d,%d):\n", I, J);

  for (i = I - 1; i <= I + 1; i += 1) {
    for (j = J - 1; j <= J + 1; j += 1) {
      if ((i != I || j != J) && i >= 0 && j >= 0 && i < 6 && j < 6 && (*grid)[i][j] == '#') {
        printf("  (%d,%d) is on\n", i, j);
        count += 1;
      }
    }
  }

  printf("  (%d,%d) has %d on neighbors\n", I, J, count);

  return count;
}

void print_board(char (*g)[6][6]) {
  int i, j;

  for (i = 0; i < 6; i += 1) {
    for (j = 0; j < 6; j += 1) {
      printf("%c", (*g)[i][j]);
    }
    printf("\n");
  }
}

void do_steps(char (*current)[6][6], char (*new)[6][6]) {
  int i, j, count;

  for (i = 0; i < 6; i += 1) {
    for (j = 0; j < 6; j += 1) {
      count = count_on_neighbors(current, i, j);
      printf("(%d,%d)", i, j);
      if ((*current)[i][j] == '#') {
        printf(" is on");
        if (count == 2 || count == 3) {
          printf(" and stays on");
          (*new)[i][j] = '#';
        } else {
          printf(" and turns off");
          (*new)[i][j] = '.';
        }
      } else {
        printf(" is off");
        if (count == 3) {
          printf(" and turns on");
          (*new)[i][j] = '#';
        } else {
          printf(" and stays off");
          (*new)[i][j] = '.';
        }
      }
      printf(" (%d)\n", count);
    }
  }
}

void do_a_bunch_of_steps(int i) {
  char (*lights_a)[6][6], (*lights_b)[6][6], (*lights_tmp)[6][6];
  lights_a = &LIGHTS_A;
  lights_b = &LIGHTS_B;
  while (i -- > 0) {
    do_steps(lights_a, lights_b);
    printf("step:\n");
    print_board(lights_b);
    printf("\n");
    lights_tmp = lights_a;
    lights_a = lights_b;
    lights_b = lights_tmp;
  }
  printf("%d\n", count_on(lights_a));
  printf("%d\n", count_on(lights_b));
}

void init_board(char (*g)[6][6]) {
  int i, j;
  for (i = 0; i < 6; i += 1) {
    for (j = 0; j < 6; j += 1) {
      (*g)[i][j] = '.';
    }
  }
}

int main (int arfc, char ** arfv) {

  load_data_file(arfv[1], &LIGHTS_A);

  init_board(&LIGHTS_B);

  printf("initial board\n");
  print_board(&LIGHTS_A);
  printf("\n");

  do_a_bunch_of_steps(4);

  return 0;
}


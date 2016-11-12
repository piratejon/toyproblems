#include <stdlib.h>
#include <stdio.h>

int lineno;

int count_code_bytes(const char * line) {
  char * end;

  for (end = line; end && *end && *end != '\n'; end += 1);

  return end - line;
}

int is_lower_hex_ascii(char lol) {
  return ((lol >= 'a' && lol <= 'f') || (lol >= '0' && lol <= '9')) ? 1 : 0;
}

int count_raw_bytes(const char * line) {
  char * cur;
  int bytes;

  for (bytes = 0, cur = line; cur && *cur && *cur != '\n'; cur += 1) {
    switch (*cur) {
      case '\\':
        if ((cur + 1) && *(cur + 1) == '\\') {
          cur += 1;
          bytes += 1;
        } else if ((cur + 1) && *(cur + 1) == '"') {
          cur += 1;
          bytes += 1;
        } else if ((cur + 1) && *(cur + 1) == 'x') {
          if ((cur + 2) && is_lower_hex_ascii(*(cur + 2)) && is_lower_hex_ascii(*(cur + 3))) {
            cur += 3;
            bytes += 1;
          }
        } else {
          printf("invalid jank %c%c%c%c at line %d pos %ld\n", cur ? *cur : '_', (cur + 1) ? *(cur + 1) : '_', (cur + 2) ? *(cur + 2) : '_', (cur + 3) ? *(cur + 3) : '_', lineno, cur - line + 1);
          exit(1);
        }
        break;
      default:
        bytes += 1;
    }
  }

  return bytes - 2;
}

int count_quoted_bytes(const char * line) {
  char * cur;
  int bytes;

  for (bytes = 0, cur = line; cur && *cur && *cur != '\n'; cur += 1) {
    switch (*cur) {
      case '\\':
        if ((cur + 1) && *(cur + 1) == '\\') {
          cur += 1;
          // quoted "\\" -> "\\\\"
          bytes += 4;
        } else if ((cur + 1) && *(cur + 1) == '"') {
          cur += 1;
          // quoted "\\" -> "\\\\"
          bytes += 4;
        } else if ((cur + 1) && *(cur + 1) == 'x') {
          if ((cur + 2) && is_lower_hex_ascii(*(cur + 2)) && is_lower_hex_ascii(*(cur + 3))) {
            cur += 3;
            // quoted "\xAB" -> "\\xAB"
            bytes += 5;
          }
        } else {
          printf("invalid jank %c%c%c%c at line %d pos %ld\n", cur ? *cur : '_', (cur + 1) ? *(cur + 1) : '_', (cur + 2) ? *(cur + 2) : '_', (cur + 3) ? *(cur + 3) : '_', lineno, cur - line + 1);
          exit(1);
        }
        break;
      default:
        // normal char does not get quoted
        bytes += 1;
    }
  }

  // add the escaped initial and terminal double quotes
  return bytes + 4;
}

int main(int arfc, char ** arfv) {

  char line[999];
  // size_t consumed;
  FILE * fin;
  int code_bytes, raw_bytes, quoted_bytes;

  fin = fopen(arfv[1], "r");

  if (!fin) {
    fprintf(stderr, "failed to open %s, bailing\n", arfv[1]);
    exit(1);
  }

  code_bytes = raw_bytes = quoted_bytes = 0;

  lineno = 0;
  while (NULL != fgets(line, sizeof(line), fin)) {
      lineno += 1;
      code_bytes += count_code_bytes(line);
      raw_bytes += count_raw_bytes(line);
      quoted_bytes += count_quoted_bytes(line);
  }

  printf("code bytes: %d\nraw bytes: %d\ndifference: %d\nquoted bytes: %d\ndifference: %d\n", code_bytes, raw_bytes, code_bytes - raw_bytes, quoted_bytes, quoted_bytes - code_bytes);

  fclose(fin);

  return 0;
}


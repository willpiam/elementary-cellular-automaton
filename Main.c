#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>

static void die(const char *msg) {
    fprintf(stderr, "%s\n", msg);
    exit(1);
}

static char *read_line(FILE *fp) {
    char *line = NULL;
    size_t cap = 0;
    ssize_t n = getline(&line, &cap, fp);
    if (n < 0) {
        free(line);
        return NULL;
    }
    while (n > 0 && (line[n - 1] == '\n' || line[n - 1] == '\r'))
        line[--n] = '\0';
    return line;
}

static void trim(char *s) {
    char *start = s;
    size_t n;
    while (*start == ' ' || *start == '\t')
        start++;
    if (start != s)
        memmove(s, start, strlen(start) + 1);
    n = strlen(s);
    while (n > 0 && (s[n - 1] == ' ' || s[n - 1] == '\t'))
        s[--n] = '\0';
}

static int parse_long(const char *s, long *out) {
    char *end = NULL;
    long v;
    if (s == NULL || *s == '\0')
        return -1;
    errno = 0;
    v = strtol(s, &end, 10);
    if (errno != 0 || end == s || *end != '\0')
        return -1;
    *out = v;
    return 0;
}

static int valid_seed(const char *s) {
    const char *p;
    if (s == NULL || *s == '\0')
        return 0;
    for (p = s; *p; ++p) {
        if (*p != '0' && *p != '1')
            return 0;
    }
    return 1;
}

int main(void) {
    FILE *input = fopen("input.txt", "r");
    char *rule_line, *seed, *gen_line, *line, *filename;
    long rule_number, generations;
    size_t n, name_len;
    int width, g, init_len, even, gen, lo, hi, j;
    uint8_t rule[8];
    uint8_t *prev, *next, *tmp;
    FILE *out;

    if (!input)
        die("cannot read input.txt");

    rule_line = read_line(input);
    seed = read_line(input);
    gen_line = read_line(input);
    fclose(input);
    if (!rule_line || !seed || !gen_line) {
        free(rule_line);
        free(seed);
        free(gen_line);
        die("invalid input");
    }
    trim(rule_line);
    trim(gen_line);

    if (parse_long(rule_line, &rule_number) != 0 || rule_number < 0 || rule_number > 255) {
        free(rule_line);
        free(seed);
        free(gen_line);
        die("invalid rule");
    }
    if (!valid_seed(seed)) {
        free(rule_line);
        free(seed);
        free(gen_line);
        die("invalid initial conditions");
    }
    if (parse_long(gen_line, &generations) != 0 || generations < 1) {
        free(rule_line);
        free(seed);
        free(gen_line);
        die("invalid generations");
    }

    n = strlen(seed);
    if (n > (size_t)INT_MAX || generations > (INT_MAX - (long)n) / 2) {
        free(rule_line);
        free(seed);
        free(gen_line);
        die("invalid generations");
    }

    width = (int)n + 2 * (int)generations;
    g = (int)generations;
    init_len = (int)n;
    even = (rule_number & 1) == 0;
    for (j = 0; j < 8; j++)
        rule[j] = (uint8_t)((rule_number >> j) & 1);

    prev = calloc((size_t)width, 1);
    next = calloc((size_t)width, 1);
    line = malloc((size_t)width + 1);
    if (!prev || !next || !line) {
        free(prev);
        free(next);
        free(line);
        free(rule_line);
        free(seed);
        free(gen_line);
        die("out of memory");
    }
    for (j = 0; j < init_len; j++)
        prev[g + j] = (uint8_t)(seed[j] == '1');

    if (mkdir("results", 0755) != 0 && errno != EEXIST) {
        free(prev);
        free(next);
        free(line);
        free(rule_line);
        free(seed);
        free(gen_line);
        die("cannot create results");
    }

    name_len = n + 64;
    filename = malloc(name_len);
    if (!filename) {
        free(prev);
        free(next);
        free(line);
        free(rule_line);
        free(seed);
        free(gen_line);
        die("out of memory");
    }
    snprintf(filename, name_len, "results/r%ld_g%ld_i%s_c.pbm", rule_number, generations, seed);
    out = fopen(filename, "w");
    free(filename);
    if (!out) {
        free(prev);
        free(next);
        free(line);
        free(rule_line);
        free(seed);
        free(gen_line);
        die("cannot write output");
    }

    fprintf(out, "P1\n%d %d\n", width, g);
    for (gen = 0; gen < g; gen++) {
        for (j = 0; j < width; j++)
            line[j] = prev[j] ? '1' : '0';
        line[width] = '\n';
        if (fwrite(line, 1, (size_t)width + 1, out) != (size_t)width + 1) {
            fclose(out);
            free(prev);
            free(next);
            free(line);
            free(rule_line);
            free(seed);
            free(gen_line);
            die("cannot write output");
        }
        if (gen + 1 == g)
            break;

        memset(next, 0, (size_t)width);
        if (even) {
            lo = g - (gen + 1);
            hi = g + init_len + (gen + 1);
        } else {
            lo = 0;
            hi = width;
        }
        for (j = lo; j < hi; j++) {
            int left = j > 0 ? prev[j - 1] : 0;
            int right = j + 1 < width ? prev[j + 1] : 0;
            int idx = (left << 2) | (prev[j] << 1) | right;
            next[j] = rule[idx];
        }
        tmp = prev;
        prev = next;
        next = tmp;
    }

    fclose(out);
    free(prev);
    free(next);
    free(line);
    free(rule_line);
    free(seed);
    free(gen_line);
    return 0;
}

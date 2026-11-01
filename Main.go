package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func die(msg string) {
	fmt.Fprintln(os.Stderr, msg)
	os.Exit(1)
}

func parseInt(text string) (int, bool) {
	t := strings.TrimSpace(strings.TrimRight(text, "\r"))
	if t == "" {
		return 0, false
	}
	neg := false
	digits := t
	if strings.HasPrefix(t, "-") {
		neg = true
		digits = t[1:]
	}
	if digits == "" {
		return 0, false
	}
	for _, c := range digits {
		if c < '0' || c > '9' {
			return 0, false
		}
	}
	v, err := strconv.Atoi(digits)
	if err != nil {
		return 0, false
	}
	if neg {
		v = -v
	}
	return v, true
}

func validSeed(seed string) bool {
	if seed == "" {
		return false
	}
	for _, c := range seed {
		if c != '0' && c != '1' {
			return false
		}
	}
	return true
}

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		die("cannot read input.txt")
	}
	text := strings.ReplaceAll(string(data), "\r\n", "\n")
	text = strings.ReplaceAll(text, "\r", "\n")
	lines := strings.Split(text, "\n")
	if len(lines) < 3 {
		die("invalid input")
	}
	ruleNumber, ok := parseInt(lines[0])
	if !ok || ruleNumber < 0 || ruleNumber > 255 {
		die("invalid rule")
	}
	seed := lines[1]
	if !validSeed(seed) {
		die("invalid initial conditions")
	}
	generations, ok := parseInt(lines[2])
	if !ok || generations < 1 {
		die("invalid generations")
	}
	n := len(seed)
	if generations > (int(^uint(0)>>1)-n)/2 {
		die("invalid generations")
	}
	width := n + 2*generations
	even := ruleNumber&1 == 0
	var rule [8]byte
	for i := 0; i < 8; i++ {
		rule[i] = byte((ruleNumber >> i) & 1)
	}

	prev := make([]byte, width)
	next := make([]byte, width)
	for i := 0; i < n; i++ {
		if seed[i] == '1' {
			prev[generations+i] = 1
		}
	}

	if err := os.MkdirAll("results", 0755); err != nil {
		die("cannot create results")
	}
	path := fmt.Sprintf("results/r%d_g%d_i%s_go.pbm", ruleNumber, generations, seed)
	file, err := os.Create(path)
	if err != nil {
		die("cannot write output")
	}
	defer file.Close()
	writer := bufio.NewWriter(file)
	if _, err := fmt.Fprintf(writer, "P1\n%d %d\n", width, generations); err != nil {
		die("cannot write output")
	}

	line := make([]byte, width+1)
	line[width] = '\n'
	for gen := 0; gen < generations; gen++ {
		for j := 0; j < width; j++ {
			if prev[j] == 0 {
				line[j] = '0'
			} else {
				line[j] = '1'
			}
		}
		if _, err := writer.Write(line); err != nil {
			die("cannot write output")
		}
		if gen+1 == generations {
			break
		}
		for i := range next {
			next[i] = 0
		}
		lo, hi := 0, width
		if even {
			lo = generations - (gen + 1)
			hi = generations + n + (gen + 1)
		}
		for j := lo; j < hi; j++ {
			left := byte(0)
			if j > 0 {
				left = prev[j-1]
			}
			right := byte(0)
			if j+1 < width {
				right = prev[j+1]
			}
			idx := (left << 2) | (prev[j] << 1) | right
			next[j] = rule[idx]
		}
		prev, next = next, prev
	}
	if err := writer.Flush(); err != nil {
		die("cannot write output")
	}
}

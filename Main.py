import os
import sys


def die(msg: str) -> None:
    print(msg, file=sys.stderr)
    raise SystemExit(1)


def parse_int(text: str):
    t = text.strip()
    sign = 1
    digits = t
    if t.startswith('-'):
        sign = -1
        digits = t[1:]
    if not digits or not digits.isdigit():
        return None
    return sign * int(digits)


def read_inputs(path: str):
    try:
        with open(path, 'r', encoding='utf-8') as handle:
            lines = handle.read().splitlines()
    except OSError:
        die('cannot read input.txt')
    if len(lines) < 3:
        die('invalid input')
    rule = parse_int(lines[0])
    seed = lines[1]
    generations = parse_int(lines[2])
    if rule is None or rule < 0 or rule > 255:
        die('invalid rule')
    if seed == '' or any(ch not in '01' for ch in seed):
        die('invalid initial conditions')
    if generations is None or generations < 1:
        die('invalid generations')
    return rule, seed, generations


def run(rule: int, generations: int, seed: str, out) -> None:
    n = len(seed)
    width = n + 2 * generations
    table = bytes((rule >> i) & 1 for i in range(8))
    even = (rule & 1) == 0
    prev = bytearray(width)
    nxt = bytearray(width)
    for i, ch in enumerate(seed):
        if ch == '1':
            prev[generations + i] = 1

    line = bytearray(width + 1)
    line[width] = ord('\n')
    out.write(f'P1\n{width} {generations}\n'.encode('ascii'))
    for gen in range(generations):
        for j in range(width):
            line[j] = 49 if prev[j] else 48
        out.write(line)
        if gen + 1 == generations:
            break
        nxt[:] = bytes(width)
        if even:
            lo = generations - (gen + 1)
            hi = generations + n + (gen + 1)
        else:
            lo, hi = 0, width
        for j in range(lo, hi):
            left = prev[j - 1] if j > 0 else 0
            right = prev[j + 1] if j + 1 < width else 0
            nxt[j] = table[(left << 2) | (prev[j] << 1) | right]
        prev, nxt = nxt, prev


def main() -> None:
    rule, seed, generations = read_inputs('input.txt')
    os.makedirs('results', exist_ok=True)
    path = f'results/r{rule}_g{generations}_i{seed}_python.pbm'
    with open(path, 'wb') as out:
        run(rule, generations, seed, out)


if __name__ == '__main__':
    main()

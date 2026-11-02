# Elementary cellular automaton, x86-64 Linux.
#
# input.txt is three lines: rule, seed, generations. The picture is
# (seed length + 2 * generations) cells wide and generations rows tall.
# The seed is centered on the first row. A parent outside the frame is 0.
# An even rule leaves cells outside the light cone at 0. An odd rule
# updates the whole frame, because neighborhood 000 becomes 1.
#
# Rows are two byte buffers. The bytes outside a generation's write range
# stay 0: anonymous mappings start cleared, and each new cone covers the
# cells that were live in the buffer being reused.

.intel_syntax noprefix
.global _start

.text
_start:
    sub rsp, 8
    cld

    mov eax, 2
    lea rdi, [rip + input_path]
    xor esi, esi
    xor edx, edx
    syscall
    test rax, rax
    js .Lerr_input
    mov r12, rax

    mov eax, 5
    mov rdi, r12
    lea rsi, [rip + statbuf]
    syscall
    test rax, rax
    jnz .Lerr_input
    mov r13, [rip + statbuf + 48]
    test r13, r13
    jle .Lerr_bad

    mov eax, 9
    xor edi, edi
    mov rsi, r13
    mov edx, 1
    mov r10d, 2
    mov r8, r12
    xor r9d, r9d
    syscall
    cmp rax, -4096
    ja .Lerr_input
    mov rbx, rax

    mov eax, 3
    mov rdi, r12
    syscall

    lea r11, [rbx + r13]
    mov rsi, rbx
    call read_line
    jc .Lerr_bad
    mov [rip + rule_a], rax
    mov [rip + rule_b], rdx

    call read_line
    jc .Lerr_bad
    mov [rip + seed_ptr], rax
    sub rdx, rax
    mov [rip + seed_len], rdx
    mov r12, rdx

    call read_line
    jc .Lerr_bad
    mov [rip + gen_a], rax
    mov [rip + gen_b], rdx

    mov rax, [rip + rule_a]
    mov rdx, [rip + rule_b]
    call trim
    mov rdi, rax
    mov rsi, rdx
    call parse_i64
    jc .Lerr_rule
    cmp rax, 255
    ja .Lerr_rule
    mov [rip + rule_num], rax

    test r12, r12
    jz .Lerr_seed
    mov rsi, [rip + seed_ptr]
    mov rcx, r12
.Lseed:
    mov al, [rsi]
    cmp al, '0'
    je .Lseed_ok
    cmp al, '1'
    jne .Lerr_seed
.Lseed_ok:
    inc rsi
    dec rcx
    jnz .Lseed

    mov rax, [rip + gen_a]
    mov rdx, [rip + gen_b]
    call trim
    mov rdi, rax
    mov rsi, rdx
    call parse_i64
    jc .Lerr_gen
    cmp rax, 1
    jl .Lerr_gen
    mov r13, rax

    mov rax, 2147483647
    cmp r12, rax
    ja .Lerr_gen
    sub rax, r12
    shr rax, 1
    cmp r13, rax
    ja .Lerr_gen

    mov rbx, r13
    shl rbx, 1
    add rbx, r12

    mov rdi, rbx
    call mmap_anon
    jc .Lerr_mem
    mov r14, rax
    mov rdi, rbx
    call mmap_anon
    jc .Lerr_mem
    mov r15, rax
    lea rdi, [rbx + 1]
    call mmap_anon
    jc .Lerr_mem
    mov [rip + line_ptr], rax

    mov rdi, r14
    add rdi, r13
    mov rsi, [rip + seed_ptr]
    mov rcx, r12
.Lplace:
    mov al, [rsi]
    sub al, '0'
    mov [rdi], al
    inc rsi
    inc rdi
    dec rcx
    jnz .Lplace

    mov eax, 83
    lea rdi, [rip + results_dir]
    mov esi, 493                # 0755
    syscall
    test rax, rax
    jz .Ldir_ok
    cmp rax, -17
    jne .Lerr_dir
.Ldir_ok:
    mov rdi, r12
    add rdi, 64
    call mmap_anon
    jc .Lerr_mem
    mov [rip + fname_ptr], rax
    mov r10, rax

    lea rsi, [rip + prefix]
    mov edx, 9
    mov rdi, r10
    call append
    mov r10, rdi

    mov rax, [rip + rule_num]
    lea rdi, [rip + numbuf + 32]
    call u64_to_dec
    mov rdi, r10
    call append
    mov r10, rdi

    lea rsi, [rip + mid_g]
    mov edx, 2
    mov rdi, r10
    call append
    mov r10, rdi

    mov rax, r13
    lea rdi, [rip + numbuf + 32]
    call u64_to_dec
    mov rdi, r10
    call append
    mov r10, rdi

    lea rsi, [rip + mid_i]
    mov edx, 2
    mov rdi, r10
    call append
    mov r10, rdi

    mov rsi, [rip + seed_ptr]
    mov rdx, r12
    mov rdi, r10
    call append
    mov r10, rdi

    lea rsi, [rip + suffix]
    mov edx, 8
    mov rdi, r10
    call append
    mov byte ptr [rdi], 0

    mov eax, 2
    mov rdi, [rip + fname_ptr]
    mov esi, 577                # O_WRONLY | O_CREAT | O_TRUNC
    mov edx, 420                # 0644
    syscall
    test rax, rax
    js .Lerr_write
    mov [rip + out_fd], rax

    lea r10, [rip + hdrbuf]
    mov byte ptr [r10], 'P'
    mov byte ptr [r10 + 1], '1'
    mov byte ptr [r10 + 2], 10
    add r10, 3
    mov rax, rbx
    lea rdi, [rip + numbuf + 32]
    call u64_to_dec
    mov rdi, r10
    call append
    mov r10, rdi
    mov byte ptr [r10], ' '
    inc r10
    mov rax, r13
    lea rdi, [rip + numbuf + 32]
    call u64_to_dec
    mov rdi, r10
    call append
    mov r10, rdi
    mov byte ptr [r10], 10
    inc r10

    mov rdi, [rip + out_fd]
    lea rsi, [rip + hdrbuf]
    mov rdx, r10
    sub rdx, rsi
    call write_all
    jc .Lerr_write

    mov qword ptr [rip + gen_idx], 0
.Lgen:
    call write_row
    jc .Lerr_write
    mov rax, [rip + gen_idx]
    inc rax
    cmp rax, r13
    jae .Ldone

    # rax is the row being computed. For an even rule it satisfies
    # 1 <= lo and hi <= width - 1, so every parent is inside the row.
    test byte ptr [rip + rule_num], 1
    jnz .Lodd
    mov r10, r13
    sub r10, rax
    mov r9, r13
    add r9, r12
    add r9, rax
    jmp .Lslide_init
.Lodd:
    mov r11, [rip + rule_num]
    movzx ecx, byte ptr [r14]
    movzx edx, byte ptr [r14 + 1]
    lea eax, [rdx + rcx * 2]
    bt r11, rax
    setc byte ptr [r15]
    mov r10, rbx
    dec r10
    movzx eax, byte ptr [r14 + r10 - 1]
    movzx ecx, byte ptr [r14 + r10]
    shl eax, 2
    lea eax, [rax + rcx * 2]
    bt r11, rax
    setc byte ptr [r15 + r10]
    mov r10d, 1
    mov r9, rbx
    dec r9
.Lslide_init:
    mov r11, [rip + rule_num]
    cmp r10, r9
    jae .Lswap
    movzx eax, byte ptr [r14 + r10 - 1]
    movzx ecx, byte ptr [r14 + r10]
.Lslide:
    movzx edx, byte ptr [r14 + r10 + 1]
    shl eax, 2
    lea eax, [rax + rcx * 2]
    or eax, edx
    bt r11, rax
    setc byte ptr [r15 + r10]
    mov eax, ecx
    mov ecx, edx
    inc r10
    cmp r10, r9
    jb .Lslide
.Lswap:
    xchg r14, r15
    inc qword ptr [rip + gen_idx]
    jmp .Lgen

.Ldone:
    mov eax, 3
    mov rdi, [rip + out_fd]
    syscall
    xor edi, edi
    mov eax, 60
    syscall

.Lerr_input:
    lea rdi, [rip + msg_input]
    mov esi, msg_input_end - msg_input
    call die
.Lerr_bad:
    lea rdi, [rip + msg_bad]
    mov esi, msg_bad_end - msg_bad
    call die
.Lerr_rule:
    lea rdi, [rip + msg_rule]
    mov esi, msg_rule_end - msg_rule
    call die
.Lerr_seed:
    lea rdi, [rip + msg_seed]
    mov esi, msg_seed_end - msg_seed
    call die
.Lerr_gen:
    lea rdi, [rip + msg_gen]
    mov esi, msg_gen_end - msg_gen
    call die
.Lerr_mem:
    lea rdi, [rip + msg_mem]
    mov esi, msg_mem_end - msg_mem
    call die
.Lerr_dir:
    lea rdi, [rip + msg_dir]
    mov esi, msg_dir_end - msg_dir
    call die
.Lerr_write:
    lea rdi, [rip + msg_write]
    mov esi, msg_write_end - msg_write
    call die

# rdi = fd, rsi = bytes, rdx = count. CF set if the write fails.
write_all:
    push rbx
    push r12
    push r13
    push r14
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
.Lwrite:
    test r13, r13
    jz .Lwrite_ok
    mov eax, 1
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    syscall
    test rax, rax
    jle .Lwrite_bad
    add r12, rax
    sub r13, rax
    jmp .Lwrite
.Lwrite_ok:
    clc
    jmp .Lwrite_out
.Lwrite_bad:
    stc
.Lwrite_out:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

# Convert prev to a P1 row and write it. CF follows write_all.
write_row:
    sub rsp, 8
    mov rsi, r14
    mov rdi, [rip + line_ptr]
    mov r8, rbx
    shr r8, 4
    jz .Lconv_tail
    mov eax, 0x30303030
    movd xmm0, eax
    pshufd xmm0, xmm0, 0
.Lconv_vec:
    movdqa xmm1, [rsi]
    paddb xmm1, xmm0
    movdqa [rdi], xmm1
    add rsi, 16
    add rdi, 16
    dec r8
    jnz .Lconv_vec
.Lconv_tail:
    mov rcx, rbx
    and rcx, 15
    test rcx, rcx
    jz .Lconv_nl
.Lconv_byte:
    mov al, [rsi]
    add al, '0'
    mov [rdi], al
    inc rsi
    inc rdi
    dec rcx
    jnz .Lconv_byte
.Lconv_nl:
    mov byte ptr [rdi], 10
    mov rdi, [rip + out_fd]
    mov rsi, [rip + line_ptr]
    lea rdx, [rbx + 1]
    call write_all
    add rsp, 8
    ret

# rsi = cursor, r11 = end. Returns the line in rax/rdx and advances rsi.
# A line that never arrives sets CF.
read_line:
    cmp rsi, r11
    jae .Lline_none
    mov rax, rsi
.Lline_scan:
    cmp rsi, r11
    jae .Lline_eof
    cmp byte ptr [rsi], 10
    je .Lline_nl
    inc rsi
    jmp .Lline_scan
.Lline_nl:
    mov rdx, rsi
    inc rsi
    jmp .Lline_cr
.Lline_eof:
    mov rdx, rsi
.Lline_cr:
    cmp rdx, rax
    jbe .Lline_ok
    cmp byte ptr [rdx - 1], 13
    jne .Lline_ok
    dec rdx
    jmp .Lline_cr
.Lline_ok:
    clc
    ret
.Lline_none:
    stc
    ret

# Trim spaces and tabs. rax = start, rdx = end.
trim:
.Ltrim_lead:
    cmp rax, rdx
    jae .Ltrim_done
    mov cl, [rax]
    cmp cl, ' '
    je .Ltrim_lead_inc
    cmp cl, 9
    jne .Ltrim_tail
.Ltrim_lead_inc:
    inc rax
    jmp .Ltrim_lead
.Ltrim_tail:
    cmp rax, rdx
    jae .Ltrim_done
    mov cl, [rdx - 1]
    cmp cl, ' '
    je .Ltrim_tail_dec
    cmp cl, 9
    jne .Ltrim_done
.Ltrim_tail_dec:
    dec rdx
    jmp .Ltrim_tail
.Ltrim_done:
    ret

# Parse a signed integer covering [rdi, rsi). CF on overflow or junk.
parse_i64:
    cmp rdi, rsi
    jae .Lparse_bad
    xor ecx, ecx
    mov al, [rdi]
    cmp al, '+'
    je .Lparse_sign
    cmp al, '-'
    jne .Lparse_digits
    mov ecx, 1
.Lparse_sign:
    inc rdi
.Lparse_digits:
    cmp rdi, rsi
    jae .Lparse_bad
    xor eax, eax
.Lparse_loop:
    cmp rdi, rsi
    jae .Lparse_sign_fix
    movzx edx, byte ptr [rdi]
    cmp edx, '0'
    jb .Lparse_bad
    cmp edx, '9'
    ja .Lparse_bad
    imul rax, rax, 10
    jo .Lparse_bad
    sub edx, '0'
    add rax, rdx
    jo .Lparse_bad
    inc rdi
    jmp .Lparse_loop
.Lparse_sign_fix:
    test ecx, ecx
    jz .Lparse_ok
    neg rax
    jo .Lparse_bad
.Lparse_ok:
    clc
    ret
.Lparse_bad:
    stc
    ret

# Anonymous mapping of rdi bytes. CF if mmap fails.
mmap_anon:
    mov rsi, rdi
    xor edi, edi
    mov edx, 3                  # PROT_READ | PROT_WRITE
    mov r10d, 34                # MAP_PRIVATE | MAP_ANONYMOUS
    mov r8, -1
    xor r9d, r9d
    mov eax, 9
    syscall
    cmp rax, -4096
    ja .Lmmap_bad
    clc
    ret
.Lmmap_bad:
    stc
    ret

# Copy rdx bytes from rsi to rdi. rdi is left at the end.
append:
    mov rcx, rdx
    rep movsb
    ret

# rax = value, rdi = one past the end of the digit buffer.
# rsi = first digit, rdx = length.
u64_to_dec:
    mov r8, rdi
    mov r9, 10
    test rax, rax
    jnz .Ldec_loop
    dec rdi
    mov byte ptr [rdi], '0'
    jmp .Ldec_done
.Ldec_loop:
    xor edx, edx
    div r9
    add dl, '0'
    dec rdi
    mov [rdi], dl
    test rax, rax
    jnz .Ldec_loop
.Ldec_done:
    mov rsi, rdi
    mov rdx, r8
    sub rdx, rsi
    ret

# rdi = message, rsi = length. Does not return.
die:
    mov rdx, rsi
    mov rsi, rdi
    mov edi, 2
    mov eax, 1
    syscall
    mov edi, 1
    mov eax, 60
    syscall

.section .rodata
input_path:
    .asciz "input.txt"
results_dir:
    .asciz "results"
prefix:
    .ascii "results/r"
mid_g:
    .ascii "_g"
mid_i:
    .ascii "_i"
suffix:
    .ascii "_asm.pbm"
msg_input:
    .ascii "cannot read input.txt\n"
msg_input_end:
msg_bad:
    .ascii "invalid input\n"
msg_bad_end:
msg_rule:
    .ascii "invalid rule\n"
msg_rule_end:
msg_seed:
    .ascii "invalid initial conditions\n"
msg_seed_end:
msg_gen:
    .ascii "invalid generations\n"
msg_gen_end:
msg_mem:
    .ascii "out of memory\n"
msg_mem_end:
msg_dir:
    .ascii "cannot create results\n"
msg_dir_end:
msg_write:
    .ascii "cannot write output\n"
msg_write_end:

.section .bss
.balign 8
statbuf:
    .skip 144
numbuf:
    .skip 32
hdrbuf:
    .skip 64
rule_a:
    .skip 8
rule_b:
    .skip 8
seed_ptr:
    .skip 8
seed_len:
    .skip 8
gen_a:
    .skip 8
gen_b:
    .skip 8
rule_num:
    .skip 8
line_ptr:
    .skip 8
fname_ptr:
    .skip 8
out_fd:
    .skip 8
gen_idx:
    .skip 8

.section .note.GNU-stack, "", @progbits

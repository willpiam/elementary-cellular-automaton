program cellular_automaton
    use iso_fortran_env, only: error_unit, iostat_eor, iostat_end
    implicit none

    character(len=:), allocatable :: rule_line, seed, gen_line, filename, row
    character(len=32) :: rule_s, gen_s
    integer :: rule, generations, n, width, g, ios, out_unit, gen, j
    integer :: start_j, end_j, left, center, right, idx
    integer :: rule_table(0:7)
    integer(kind=1), allocatable :: prev(:), next(:)
    logical :: even

    call read_record(10, 'input.txt', rule_line, seed, gen_line)

    rule = parse_int(trim_text(rule_line), 'invalid rule')
    if (rule < 0 .or. rule > 255) call die('invalid rule')
    seed = strip_cr(seed)
    n = len(seed)
    if (n < 1 .or. .not. valid_seed(seed)) call die('invalid initial conditions')
    generations = parse_int(trim_text(gen_line), 'invalid generations')
    if (generations < 1) call die('invalid generations')
    if (generations > (huge(generations) - n) / 2) call die('invalid generations')

    g = generations
    width = n + 2 * g
    even = iand(rule, 1) == 0
    do j = 0, 7
        rule_table(j) = iand(ishft(rule, -j), 1)
    end do

    allocate(prev(width), next(width))
    prev = 0
    next = 0
    do j = 1, n
        if (seed(j:j) == '1') prev(g + j) = 1
    end do

    call execute_command_line('mkdir -p results', exitstat=ios)
    if (ios /= 0) call die('cannot create results')

    write(rule_s, '(I0)') rule
    write(gen_s, '(I0)') generations
    filename = 'results/r' // trim(rule_s) // '_g' // trim(gen_s) // '_i' // seed // '_fortran.pbm'
    open(newunit=out_unit, file=filename, status='replace', action='write', iostat=ios)
    if (ios /= 0) call die('cannot write output')

    write(out_unit, '(A)') 'P1'
    write(out_unit, '(I0,1X,I0)') width, generations
    allocate(character(len=width) :: row)

    do gen = 0, generations - 1
        do j = 1, width
            if (prev(j) == 0) then
                row(j:j) = '0'
            else
                row(j:j) = '1'
            end if
        end do
        write(out_unit, '(A)') row
        if (gen + 1 == generations) exit

        next = 0
        if (even) then
            start_j = generations - gen
            end_j = generations + n + gen + 1
        else
            start_j = 1
            end_j = width
        end if
        do j = start_j, end_j
            if (j == 1) then
                left = 0
            else
                left = prev(j - 1)
            end if
            center = prev(j)
            if (j == width) then
                right = 0
            else
                right = prev(j + 1)
            end if
            idx = left * 4 + center * 2 + right
            next(j) = int(rule_table(idx), kind=1)
        end do
        prev = next
    end do

    close(out_unit)

contains

    subroutine die(msg)
        character(len=*), intent(in) :: msg
        write(error_unit, '(A)') msg
        error stop 1
    end subroutine die

    function trim_text(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: a, b
        a = 1
        b = len(text)
        do while (a <= b .and. (text(a:a) == ' ' .or. text(a:a) == achar(9)))
            a = a + 1
        end do
        do while (b >= a .and. (text(b:b) == ' ' .or. text(b:b) == achar(9) .or. text(b:b) == achar(13)))
            b = b - 1
        end do
        if (b < a) then
            out = ''
        else
            out = text(a:b)
        end if
    end function trim_text

    function strip_cr(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: n
        n = len(text)
        if (n > 0 .and. text(n:n) == achar(13)) then
            out = text(1:n-1)
        else
            out = text
        end if
    end function strip_cr

    integer function parse_int(text, errmsg)
        character(len=*), intent(in) :: text, errmsg
        integer :: sign, i, ios
        character(len=:), allocatable :: digits
        parse_int = -1
        if (len(text) == 0) call die(errmsg)
        sign = 1
        digits = text
        if (text(1:1) == '-') then
            sign = -1
            if (len(text) == 1) call die(errmsg)
            digits = text(2:)
        end if
        do i = 1, len(digits)
            if (digits(i:i) < '0' .or. digits(i:i) > '9') call die(errmsg)
        end do
        read(digits, *, iostat=ios) parse_int
        if (ios /= 0) call die(errmsg)
        parse_int = sign * parse_int
    end function parse_int

    logical function valid_seed(text)
        character(len=*), intent(in) :: text
        integer :: i
        valid_seed = .true.
        do i = 1, len(text)
            if (text(i:i) /= '0' .and. text(i:i) /= '1') then
                valid_seed = .false.
                return
            end if
        end do
    end function valid_seed

    subroutine read_record(unit, path, line1, line2, line3)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: line1, line2, line3
        integer :: ios
        open(unit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) call die('cannot read input.txt')
        call read_line(unit, line1, ios)
        if (ios /= 0) call die('invalid input')
        call read_line(unit, line2, ios)
        if (ios /= 0) call die('invalid input')
        call read_line(unit, line3, ios)
        if (ios /= 0) call die('invalid input')
        close(unit)
    end subroutine read_record

    subroutine read_line(unit, line, status)
        integer, intent(in) :: unit
        character(len=:), allocatable, intent(out) :: line
        integer, intent(out) :: status
        character(len=1024) :: buf
        integer :: n, ios
        line = ''
        status = 0
        do
            read(unit, '(A)', advance='no', iostat=ios, size=n) buf
            if (ios == iostat_eor .or. ios == iostat_end) then
                if (n > 0) line = line // buf(1:n)
                if (ios == iostat_end .and. len(line) == 0 .and. n == 0) status = ios
                return
            else if (ios /= 0) then
                status = ios
                return
            else
                line = line // buf(1:n)
            end if
        end do
    end subroutine read_line

end program cellular_automaton

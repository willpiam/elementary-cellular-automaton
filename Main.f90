program CellularAutomaton
    implicit none
    
    ! Constants
    integer, parameter :: MAX_LENGTH_INITIAL_CONDITIONS = 1000
    
    ! Variables
    integer :: rule_number, generations, initial_length, image_width
    character(len=MAX_LENGTH_INITIAL_CONDITIONS) :: initial_conditions
    integer, dimension(:,:), allocatable :: automaton_data
    integer, dimension(8) :: rule_binary
    
    ! Read input from file
    call read_inputs_from_file(rule_number, initial_conditions, generations)
    
    ! Convert rule number to binary array
    call rule_to_binary_array(rule_number, rule_binary)
    
    ! Run cellular automaton
    initial_length = len_trim(initial_conditions)
    image_width = initial_length + 2 * generations
    allocate(automaton_data(generations, image_width))
    call run_cellular_automaton(rule_binary, generations, initial_conditions, automaton_data)
    
    ! Output results to file
    call output_to_file(automaton_data, rule_number, generations, initial_conditions)
    
    ! Clean up
    deallocate(automaton_data)
    
contains
    
    subroutine read_inputs_from_file(rule_number, initial_conditions, generations)
        integer, intent(out) :: rule_number, generations
        character(len=*), intent(out) :: initial_conditions
        integer :: io_status
        
        open(unit=10, file='input.txt', status='old', action='read', iostat=io_status)
        if (io_status /= 0) then
            print *, "Error opening input file!"
            stop
        end if
        
        read(10, *, iostat=io_status) rule_number
        read(10, *, iostat=io_status) initial_conditions
        read(10, *, iostat=io_status) generations
        
        close(10)
    end subroutine read_inputs_from_file
    
    subroutine rule_to_binary_array(rule_number, rule_binary)
        integer, intent(in) :: rule_number
        integer, dimension(8), intent(out) :: rule_binary
        integer :: i
        
        do i = 0, 7
            rule_binary(i+1) = iand(ishft(rule_number, -i), 1)
        end do
    end subroutine rule_to_binary_array
    
    subroutine run_cellular_automaton(rule_binary, generations, initial_conditions, automaton_data)
        integer, dimension(8), intent(in) :: rule_binary
        integer, intent(in) :: generations
        character(len=*), intent(in) :: initial_conditions
        integer, dimension(:,:), intent(out) :: automaton_data
        integer :: i, j, initial_length, image_width, padding_length
        
        initial_length = len_trim(initial_conditions)
        image_width = size(automaton_data, 2)
        padding_length = (image_width - initial_length) / 2
        
        ! Initialize first generation
        automaton_data = 0
        do i = 1, initial_length
            if (initial_conditions(i:i) == '1') then
                automaton_data(1, padding_length + i) = 1
            end if
        end do
        
        ! Generate subsequent generations
        do i = 2, generations
            do j = 2, image_width - 1
                automaton_data(i, j) = calculate_cell(automaton_data(i-1, j-1), &
                                                    automaton_data(i-1, j), &
                                                    automaton_data(i-1, j+1), &
                                                    rule_binary)
            end do
        end do
    end subroutine run_cellular_automaton
    
    integer function calculate_cell(left, center, right, rule_binary)
        integer, intent(in) :: left, center, right
        integer, dimension(8), intent(in) :: rule_binary
        integer :: index
        
        index = ishft(left, 2) + ishft(center, 1) + right
        calculate_cell = rule_binary(index + 1)
    end function calculate_cell
    
    subroutine output_to_file(automaton_data, rule_number, generations, initial_conditions)
        integer, dimension(:,:), intent(in) :: automaton_data
        integer, intent(in) :: rule_number, generations
        character(len=*), intent(in) :: initial_conditions
        integer :: i, j, io_status
        character(len=200) :: filename
        
        ! Create results directory if it doesn't exist
        call system('mkdir -p results')
        
        ! Create filename
        write(filename, '(a,i0,a,i0,a,a,a)') 'results/r', rule_number, '_g', generations, '_i', &
            trim(initial_conditions), '_fortran.pbm'
        
        ! Open file
        open(unit=20, file=trim(filename), status='replace', action='write', iostat=io_status)
        if (io_status /= 0) then
            print *, "Error creating output file!"
            stop
        end if
        
        ! Write PBM header
        write(20, '(a)') 'P1'
        write(20, '(i0,1x,i0)') size(automaton_data, 2), generations
        
        ! Write automaton data
        do i = 1, generations
            do j = 1, size(automaton_data, 2)
                write(20, '(i1)', advance='no') automaton_data(i, j)
            end do
            write(20, *)
        end do
        
        close(20)
    end subroutine output_to_file
    
end program CellularAutomaton 
module precision

    use, intrinsic :: iso_fortran_env, only: real32, real64, real128

    implicit none
    private

    public :: rp

    save

    !integer, parameter :: rp = real32					!.. working precision for reals
    integer, parameter :: rp = real64                    !.. working precision for reals
    !integer, parameter :: rp = real128                    !.. working precision for reals

end module precision

!!!Developer: H. E. Konokman

module input_checker_mod

    implicit none

    interface input_checker
        module procedure input_checker_real, input_checker_integer, &
            input_checker_string
    end interface

contains

    function input_checker(real_input_str, real_input, &
            integer_input_str, integer_input, &
            file_input_str, file_input, string_input_str, string_input, value_str) result(ok)

        use :: precision, only: rp

        implicit none
        character(*), intent(in), optional :: real_input_str
        real(rp), intent(out), optional :: real_input
        character(*), intent(in), optional :: integer_input_str
        integer, intent(out), optional :: integer_input
        character(*), intent(in), optional :: file_input_str
        character(len=*), intent(out), optional :: file_input
        character(*), intent(in), optional :: string_input_str
        character(len=*), intent(out), optional :: string_input
        character(len=*), intent(out), optional :: value_str
        logical :: ok

        integer :: istat

        ok = .false.

        if(present(real_input_str)) then

            read(real_input_str,*,iostat=istat) real_input
            if(istat == 0) then
                ok = .true.
                if(present(value_str)) write(value_str,*) real_input
            else
                ok = .false.
                if(present(value_str)) value_str = ""
            end if

        elseif(present(integer_input_str)) then

            read(integer_input_str,*,iostat=istat) integer_input
            if(istat == 0) then
                ok = .true.
                if(present(value_str)) write(value_str,*) integer_input
            else
                ok = .false.
                if(present(value_str)) value_str = ""
            end if

        elseif(present(file_input_str)) then
            inquire(file = trim(file_input_str), exist=ok)
            if(ok) then
                file_input = trim(file_input_str)
                !                ok = .true.
                if(present(value_str)) value_str = trim(file_input)
            else
                ok = .false.
                if(present(value_str)) value_str = ""
            end if

        elseif(present(string_input_str)) then

            if(len_trim(string_input_str) > 0) then
                string_input = trim(string_input_str)
                ok = .true.
                if(present(value_str)) value_str = trim(string_input)
            else
                ok = .false.
                if(present(value_str)) value_str = ""
            end if

        else
            write(*,*) "Check input!"
        end if

    end function

    function input_checker_real(input_str, input, value_str) result(ok)

        use :: precision, only: rp

        implicit none
        character(*), intent(in) :: input_str
        real(rp), intent(out) :: input
        character(len=*), intent(out), optional :: value_str
        logical :: ok

        integer :: istat

        ok = .false.

        read(input_str,*,iostat=istat) input
        if(istat == 0) then
            ok = .true.
            if(present(value_str)) write(value_str,*) input
        else
            ok = .false.
            if(present(value_str)) value_str = ""
        end if

    end function

    function input_checker_integer(input_str, input, value_str) result(ok)

        use :: precision, only: rp

        implicit none
        character(*), intent(in) :: input_str
        integer, intent(out) :: input
        character(len=*), intent(out), optional :: value_str
        logical :: ok

        integer :: istat

        ok = .false.

        read(input_str,*,iostat=istat) input
        if(istat == 0) then
            ok = .true.
            if(present(value_str)) write(value_str,*) input
        else
            ok = .false.
            if(present(value_str)) value_str = ""
        end if

    end function

    function input_checker_string(input_str, input, input_file, value_str) result(ok)

        use :: precision, only: rp

        implicit none
        character(*), intent(in) :: input_str
        character(*), intent(out), optional :: input
        character(*), intent(out), optional :: input_file
        character(len=*), intent(out), optional :: value_str
        logical :: ok

        integer :: istat

        ok = .false.

        if(present(input_file)) then
            inquire(file = trim(input_str), exist=ok)
            if(ok) then
                input_file = trim(input_str)
                !                ok = .true.
                if(present(value_str)) value_str = trim(input_file)
            else
                ok = .false.
                if(present(value_str)) value_str = ""
            end if

        else

            if(len_trim(input_str) > 0) then
                input = trim(input_str)
                ok = .true.
                if(present(value_str)) value_str = trim(input)
            else
                ok = .false.
                if(present(value_str)) value_str = ""
            end if

        endif

    end function

end module

module handlers

    use :: iso_c_binding, only: c_ptr
    use :: precision
    use :: input_checker_mod

    use gtk, only: gtk_label_new, gtk_label_set_text, gtk_main_quit, &
        & gtk_widget_show, gtk_widget_show_all, gtk_window_maximize, gtk_window_new,&
        & gtk_window_set_titlebar, gtk_window_set_title, gtk_box_pack_start, gtk_box_pack_end, &
        & gtk_grid_new, gtk_grid_attach, &
        & GTK_ICON_SIZE_MENU, &
        & GTK_ORIENTATION_VERTICAL, GTK_ORIENTATION_HORIZONTAL, GTK_WINDOW_TOPLEVEL

    use gtk_sup
    use g

    use ::gtk_hl_entry

    implicit none
    type(c_ptr) :: window
    type(c_ptr) :: entry_real, entry_integer, entry_file, entry_string
    type(c_ptr) :: label_real, label_integer, label_file, label_string

    integer(kind=c_int) :: run_status = TRUE

contains

    subroutine my_destroy(widget, gdata) bind(c)
        type(c_ptr), value :: widget, gdata

        call gtk_main_quit()
        run_status = FALSE

    end subroutine my_destroy

    subroutine entry_real_changed(widget, gdata) bind(c)
        type(c_ptr), value :: widget, gdata

        character(len=500) :: ftext
        real(rp) :: real_input
        character(len=500) :: value_str
        logical :: ok

        call hl_gtk_entry_get_text(widget, ftext)

        ok = input_checker(trim(ftext), real_input, value_str=value_str)

        if(ok) then
            write(*,*) "Real input is", real_input
            call gtk_label_set_text(label_real, trim(value_str)//c_null_char)
        else
            call gtk_label_set_text(label_real, ""//c_null_char)
            write(*,*) "Wrong input for real: "//trim(ftext)
        endif

    end subroutine

    subroutine entry_integer_changed(widget, gdata) bind(c)
        type(c_ptr), value :: widget, gdata

        character(len=500) :: ftext
        integer :: integer_input
        character(len=500) :: value_str
        logical :: ok

        call hl_gtk_entry_get_text(widget, ftext)

        ok = input_checker(trim(ftext), integer_input, value_str=value_str)

        if(ok) then
            write(*,*) "Integer input is", integer_input
            call gtk_label_set_text(label_integer, trim(value_str)//c_null_char)
        else
            call gtk_label_set_text(label_integer, ""//c_null_char)
            write(*,*) "Wrong input for integer: "//trim(ftext)
        endif

    end subroutine

    subroutine entry_file_changed(widget, gdata) bind(c)
        type(c_ptr), value :: widget, gdata

        character(len=500) :: ftext
        character(len=500) :: file_input
        character(len=500) :: value_str
        logical :: ok

        call hl_gtk_entry_get_text(widget, ftext)

        ok = input_checker(trim(ftext), input_file=file_input, value_str=value_str)

        if(ok) then
            write(*,*) "File input is: ", trim(file_input)
            call gtk_label_set_text(label_file, trim(value_str)//c_null_char)
        else
            call gtk_label_set_text(label_file, ""//c_null_char)
            write(*,*) "Wrong input for file (not exists): "//trim(ftext)
        endif

    end subroutine

    subroutine entry_string_changed(widget, gdata) bind(c)
        type(c_ptr), value :: widget, gdata

        character(len=500) :: ftext
        character(len=500) :: string_input
        character(len=500) :: value_str
        logical :: ok

        call hl_gtk_entry_get_text(widget, ftext)

        ok = input_checker(trim(ftext), string_input, value_str=value_str)

        if(ok) then
            write(*,*) "String input is: ", trim(string_input)
            call gtk_label_set_text(label_string, trim(value_str)//c_null_char)
        else
            call gtk_label_set_text(label_string, ""//c_null_char)
            write(*,*) "No input for string"
        endif

    end subroutine

    subroutine create_window()

        type(c_ptr) :: table, label

        window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
        call g_signal_connect(window, "delete-event"//c_null_char, c_funloc(my_destroy))
        call gtk_window_set_title(window, "input checker example"//c_null_char)


        table = gtk_grid_new()
        call gtk_container_add(window, table)

        label = gtk_label_new("Real input: "//c_null_char)
        entry_real = gtk_entry_new()
        call g_signal_connect(entry_real, "changed"//C_NULL_CHAR, c_funloc(entry_real_changed))
        label_real = gtk_label_new(""//c_null_char)
        call gtk_grid_attach(table, label, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, entry_real, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, label_real, 2_c_int, 0_c_int, 1_c_int, 1_c_int)

        label = gtk_label_new("Integer input: "//c_null_char)
        entry_integer = gtk_entry_new()
        call g_signal_connect(entry_integer, "changed"//C_NULL_CHAR, c_funloc(entry_integer_changed))
        label_integer = gtk_label_new(""//c_null_char)
        call gtk_grid_attach(table, label, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, entry_integer, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, label_integer, 2_c_int, 1_c_int, 1_c_int, 1_c_int)

        label = gtk_label_new("File input: "//c_null_char)
        entry_file = gtk_entry_new()
        call g_signal_connect(entry_file, "changed"//C_NULL_CHAR, c_funloc(entry_file_changed))
        label_file = gtk_label_new(""//c_null_char)
        call gtk_grid_attach(table, label, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, entry_file, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, label_file, 2_c_int, 2_c_int, 1_c_int, 1_c_int)

        label = gtk_label_new("String input: "//c_null_char)
        entry_string = gtk_entry_new()
        call g_signal_connect(entry_string, "changed"//C_NULL_CHAR, c_funloc(entry_string_changed))
        label_string = gtk_label_new(""//c_null_char)
        call gtk_grid_attach(table, label, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, entry_string, 1_c_int, 3_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, label_string, 2_c_int, 3_c_int, 1_c_int, 1_c_int)


        call gtk_widget_show_all(table)



    end subroutine

    subroutine show_window()

        call gtk_widget_show(window)

    end subroutine

end module

program gui_main

    use :: gtk, only: gtk_init, gtk_main
    use :: handlers

    implicit none

    call gtk_init()

    call create_window()

    call show_window()

    call gtk_main()


end program gui_main

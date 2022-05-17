! *** Subroutine to be used as checkpoint for debugging ***
! The subroutine works for scalar variables: no control for dimensions is included.
! The variable in input is a polymorphic entity, i.e. able to be of differing types during program execution.
! Arguments:
! -> debug:     logical variable, controls routine
! -> msg:       optional message to be printed
! -> var:       optional variable to be printed
! -> msg_type:  message tag, to be used in more complex subroutines
!               Can be either:
!                   + 'Error', in bold red;
!                   + 'Warning', in bold yellow;
!                   + 'OK', in bold green;
!                   + if not present, plain msg is printed
! -> stop_exec: logical variable; if true, execution is stopped

module checkpoint
    implicit none
    contains

    subroutine check(debug, msg, var, msg_type, stop_exec)

        logical :: debug
        character(*), optional :: msg
        class(*), optional :: var
        character(*), optional :: msg_type
        logical, optional :: stop_exec
        logical :: control_stop = .FALSE. ! default: execution is not stopped

        if (debug) then
            if (msg_type.EQ."Error") then
                print *, ""//achar(27)//"[1;31m"//msg_type, ": "//achar(27)//"[0m"//msg
                control_stop = .TRUE. ! control_stop is turned on in case of 'Error' msg_type
            else if (msg_type.EQ."Warning") then
                print *, ""//achar(27)//"[1;33m"//msg_type, ": "//achar(27)//"[0m"//msg
            else if (msg_type.EQ."OK") then
                print *, ""//achar(27)//"[1;32m"//msg_type, ": "//achar(27)//"[0m"//msg
            else if (.NOT.present(msg_type)) then
                print *, msg
            end if
            
            ! if present, stop_exec supersedes msg_type choice
            if (present(stop_exec)) control_stop = stop_exec

            if (present(var)) then
                select type(var) ! control on type
                    type is (logical)
                        print *, var, "type: LOGICAL"
                    type is (integer(2))
                        print *, var, "type: INTEGER(2)"
                    type is (integer(4))
                        print *, var, "type: INTEGER(4)"
                    type is (real(4))
                        print *, var, "type: REAL(4)"
                    type is (real(8))
                        print *, var, "type: REAL(8)"
                    type is (complex(4))
                        print *, var, "type: COMPLEX(4)"
                    type is (complex(8))
                        print *, var, "type: COMPLEX(8)"
                    type is (character(*))
                        print *, var, "type: CHARACTER(*)"
                end select
            end if
        end if

        if (control_stop) stop ! stop execution

    end subroutine check
end module checkpoint
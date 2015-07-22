!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: MessageHandling.f90 1950 2012-11-07 18:31:14Z noort $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/deltares_common/packages/deltares_common/src/MessageHandling.f90 $

!> Specifies the interface for MessageHandling's callback functionality.
! (A bit awkward, but including it in MessageHandling's module header
! did not make it visible in subprograms when using ifort 11.0.)
module MHCallBack
   abstract interface
      subroutine mh_callbackiface(level)
         integer, intent(in) :: level !< The severity level
      end subroutine mh_callbackiface
   end interface
end module MHCallBack

!> Diagnostics output module.
!! Prints and/or logs messages from an application.
!! Three variants:
!! -# write2screen: prints directly on stdout
!! -# useLog: writes to a specified file points.
!! -# MessageQueue: Can be emptied by any other application.
!!
!! See MessageHandling::SetMessageHandling for more details.
!!
!! Messages have a severity level: LEVEL_(DEBUG|INFO|WARN|ERROR|FATAL).
module MessageHandling
   use MHCallBack
   implicit none


   public SetMessage
   public GetMessageCount
   public SetMessageHandling
   public mess
   public err
   public GetMessage_MH
   public resetMessageCount_MH
   public getMaxErrorLevel
   public resetMaxerrorLevel


   integer,parameter, public     :: LEVEL_DEBUG = 1
   integer,parameter, public     :: LEVEL_INFO  = 2
   integer,parameter, public     :: LEVEL_WARN  = 3
   integer,parameter, public     :: LEVEL_ERROR = 4
   integer,parameter, public     :: LEVEL_FATAL = 5
   integer,parameter, public     :: LEVEL_NONE  = 6
   integer,parameter, public     :: Charln = 256
   integer,parameter, public     :: Idlen = 40
   integer,parameter, public     :: max_level = 5
   character(len=12), dimension(max_level), &
                      private    :: level_prefix = &
                                              (/'            ', '            ', '** WARNING: ', '** ERROR:   ', '** FATAL:   '/)

    interface mess
    module procedure message1string
    module procedure message2string
    module procedure message3string
    module procedure message4string
    module procedure message1char1real
    module procedure message1char2real
    module procedure message2char1real
    module procedure message2char2real
    module procedure message1char1int
    module procedure message1char2int
    module procedure message1char3int
    module procedure message2int1char
    module procedure message1char1int1double
    module procedure message1double1int1char
    end interface
    
    interface err
    module procedure error1char
    module procedure error2char
    module procedure error3char
    module procedure error4char
    module procedure error1char1real
    module procedure error1char2real
    module procedure error2char1real
    module procedure error2char2real
    module procedure error1char1int
    module procedure error1char2int
    module procedure error1char1int1double
    end interface

private   
   integer, parameter, private   :: maxMessages = 3000
   character(len=256), dimension(maxMessages), private :: Messages
   integer           , dimension(maxMessages), private :: Levels
   integer                                   , private :: messagecount 
   integer                                   , private :: maxErrorLevel = 0 
   integer                                   , public  :: thresholdLvl = 0 
   integer, save                  :: lunMess          = 0
   logical, save                  :: writeMessage2Screen = .false.
   logical, save                  :: useLogging = .true.
   logical, save                  :: alreadyInCallback=.false.                   !< flag for preventing recursive calls to callback subroutine
   !> Callback routine invoked upon any mess/err (i.e. SetMessage)
   procedure(mh_callbackiface), pointer :: mh_callback => null()

contains

!> Sets up the output of messages. All three formats are optional
!! and can be used in any combination.
subroutine SetMessageHandling(write2screen, useLog, lunMessages, callback, thresholdLevel)
   logical, optional, intent(in)       :: write2screen !< Print messages to stdout.
   logical, optional, intent(in)       :: useLog       !< Store messages in buffer.
   integer, optional, intent(in)       :: lunMessages  !< File pointer whereto messages can be written.
   integer, optional, intent(in)       :: thresholdLevel  !< Messages with level lower than the thresholdlevel
                                                          !< will be discarded.
   procedure(mh_callbackiface), optional :: callback

   if (present(write2screen) ) writeMessage2Screen = write2screen
   if (present(lunMessages) )  lunMess             = lunMessages
   if (present(useLog) )       useLogging          = useLog
   if (present(callback) ) then
       mh_callback         =>callback
   endif
   if (present(thresholdLevel) )  thresholdLvl     = thresholdLevel
   alreadyInCallback = .false.
end subroutine SetMessageHandling

!> The main message routine. Puts the message string to all output
!! channels previously set up by SetMessageHandling
recursive subroutine SetMessage(level, string)
   integer, intent(in)           :: level  !< One of: LEVEL_(DEBUG|INFO|WARN|ERROR|FATAL).
   character(len=*), intent(in)  :: string !< Complete message string.

   integer :: levelact
   levelact = max(1,min(max_level, level))

   if (level >= thresholdLvl) then

      if (writeMessage2Screen) then
         if (len_trim(level_prefix(levelact)) > 0) then
            write (*, '(a)') trim(level_prefix(levelact))
         end if
         write (*, '(a)') trim(string)
      endif
      
      if ( (lunMess > 0) .and. (level >= thresholdLvl) ) then
        if (len_trim(level_prefix(levelact)) > 0) then
           write (lunMess, '(a)') trim(level_prefix(levelact))
        end if
        write (lunMess, '(a)')  trim(string)
      end if
      if (useLogging) then
         messageCount           = messageCount + 1 
         if (level > maxErrorLevel) then
            maxErrorLevel = level
         endif
         if (messageCount > maxMessages) then
            messages(maxMessages) = 'Maximum number of messages reached'
            levels(maxMessages)   = level
            messagecount          = maxmessages
         else
            messages(messageCount) = string
            levels(messageCount)   = level
         endif
      endif
   endif
   
   ! Optional callback routine for any user-specified actions (e.g., upon error)   
   if (associated(mh_callback).and. .not. alreadyInCallback) then
      alreadyInCallback = .true.
      call mh_callback(level) !In future, possibly also error #ID
      alreadyInCallback = .false.
   end if
end subroutine

integer function getMessageCount()
   getMessageCount = messagecount
end function

integer function GetMessage_MH(imessage, message) 
   character(len=200)               :: message
   integer, intent(in)              :: imessage

   message=messages(imessage)(1:200)
   GetMessage_MH = levels(imessage)
end function

subroutine resetMessageCount_MH()

   messageCount = 0
end subroutine

integer function getMaxErrorLevel()
   getMaxErrorLevel = maxErrorLevel
end function

subroutine resetMaxerrorLevel()
   maxErrorLevel = 0
end subroutine

subroutine message1string(level, w1)
    character(*)    :: w1
    integer         :: level

    integer                        :: l1
    character(600)                 :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    write (rec(1:), '(a)') w1(:l1)
    
    call setMessage(level, rec)
end subroutine message1string

subroutine message2string(level, w1, w2)
    character(*) :: w1, w2
    integer         :: level

    integer :: l1, l2
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    l2 = max(1, len_trim(w2))
    write (rec(1:), '(a)') w1(:l1)
    write (rec(2 + l1:), '(a)') w2(:l2)
    
    call SetMessage(level, rec)
end subroutine message2string

subroutine message3string(level, w1, w2, w3)
    character(*) :: w1, w2, w3
    integer         :: level

    integer :: l1, l2, l3
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    l2 = max(1, len_trim(w2))
    l3 = max(1, len_trim(w3))
    write (rec(1:), '(a)') w1(1:l1)
    write (rec(2 + l1:), '(a)') w2(1:l2)
    write (rec(3 + l1 + l2:), '(a)') w3(1:l3)
    
    call SetMessage(level, rec)
 end subroutine message3string

subroutine message4string(level, w1, w2, w3, w4)
    character(*) :: w1, w2, w3, w4
    integer         :: level

    integer :: l1, l2, l3, l4
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    l2 = max(1, len_trim(w2))
    l3 = max(1, len_trim(w3))
    l4 = max(1, len_trim(w4))
    write (rec(1:), '(a)') w1(:l1)
    write (rec(2 + l1:), '(a)') w2(:l2)
    write (rec(3 + l1 + l2:), '(a)') w3(:l3)
    write (rec(4 + l1 + l2 + l3:), '(a)') w4(:l4)
    
    call SetMessage(level, rec)
end subroutine message4string

subroutine message2char1real(level, w1, w2, r3)

    real :: r3
    character(*) :: w1, w2
    intent (in) r3
    integer         :: level

    integer :: l1, l2
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    l2 = max(1, len_trim(w2))
    write (rec(1:), '(a)') w1(:l1)
    write (rec(2 + l1:), '(a)') w2(:l2)
    write (rec(3 + l1 + l2:), '(f14.6)') r3
    
    call SetMessage(level, rec)
end subroutine message2char1real

subroutine message2char2real(level, w1, w2, r3, r4)
    real :: r3, r4
    character(*) :: w1, w2
    intent (in) r3, r4
    integer         :: level

    integer :: l1, l2
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    l2 = max(1, len_trim(w2))
    write (rec(1:), '(a)') w1(:l1)
    write (rec(2 + l1:), '(a)') w2(:l2)
    write (rec(3 + l1 + l2:), '(2f14.6)') r3, r4
    
    call SetMessage(level, rec)
end subroutine message2char2real

subroutine message1char1real(level, w1, r2)
    real :: r2
    character(*) :: w1
    intent (in) r2
    integer         :: level

    integer :: l1
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    write (rec(1:), '(a)') w1(:l1)
    write (rec(2 + l1:), '(F14.6)') r2
    
    call SetMessage(level, rec)
end subroutine message1char1real

subroutine message1char1int(level, w1, i2)
    integer :: i2
    character(*) :: w1
    intent (in) i2
    integer         :: level

    integer :: l1
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    write (rec(1:), '(a)') w1(:l1)
    write (rec(2 + l1:), '(I14)') i2
    
    call SetMessage(level, rec)
end subroutine message1char1int

subroutine message1char2int(level, w1, i2, i3)
    integer :: i2, i3
    character(*) :: w1
    intent (in) i2, i3
    integer         :: level

    integer :: l1
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    write (rec(1:), '(a)') w1(:l1)
    write (rec(2 + l1:), '(2I14)') i2, i3
    
    call SetMessage(level, rec)

end subroutine message1char2int

subroutine message2int1char(level, i1, i2, w3)
    integer :: i1, i2
    character(*) :: w3
    intent (in) i1, i2
    integer         :: level

    integer :: l3
    character(600) :: rec

    rec = ' '
    l3 = max(1, len_trim(w3))
    write (rec( 1:28), '(2I14)') i1, i2
    write (rec(30:)  , '(a)'   ) w3(:l3)

    
    call SetMessage(level, rec)

end subroutine message2int1char

subroutine message1char3int(level, w1, i2, i3, i4)
    integer :: i2, i3, i4
    character(*) :: w1
    intent (in) i2, i3, i4
    integer        :: level
    integer        :: l1
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    write (rec(1:), '(a)') w1(:l1)
    write (rec(2 + l1:), '(3I14)') i2, i3, i4
    
    call SetMessage(level, rec)
end subroutine message1char3int

subroutine message1char2real(level, w1, r2, r3)
    real         :: r2, r3
    character(*) :: w1
    integer      :: level
    intent(in)   :: level, w1, r2, r3

    integer        :: l1
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    write (rec(1:), '(a)') w1(:l1)
    write (rec(2 + l1:), '(2F14.6)') r2, r3

    call SetMessage(level, rec)
end subroutine message1char2real

subroutine message1char1int1double(level, w1, i2, d3)
    integer          :: level
    character(*)     :: w1
    integer          :: i2
    double precision :: d3

    integer :: l1
    character(600) :: rec

    rec = ' '
    l1 = max(1, len_trim(w1))
    write (rec(1:), '(a)') w1(:l1)
    write (rec(2 + l1:), '(i14)') i2
    write (rec(16 + l1:), '(F14.6)') d3

    call SetMessage(level, rec)
end subroutine message1char1int1double

subroutine message1double1int1char(level, d1, i2, w3)
    integer          :: level
    character(*)     :: w3
    integer          :: i2
    double precision :: d1

    integer :: l3
    character(600) :: rec

    rec = ' '
    l3 = max(1, len_trim(w3))
    write (rec(1 :16), '(F16.6)') d1
    write (rec(18:31), '(i14)'  ) i2
    write (rec(33:  ), '(a)'    ) w3(:l3)

    call SetMessage(level, rec)
end subroutine message1double1int1char
!-- Error interfaces ----------------------------
subroutine error4char(w1, w2, w3, w4)
    character(*) :: w1, w2, w3, w4

    call mess(LEVEL_ERROR, w1, w2, w3, w4)
end subroutine error4char

subroutine error3char(w1, w2, w3)
    character(*) :: w1, w2, w3

    call mess(LEVEL_ERROR, w1, w2, w3)
end subroutine error3char

subroutine error2char(w1, w2)
    character(*) :: w1, w2

    call mess(LEVEL_ERROR, w1, w2)
end subroutine error2char

subroutine error1char(w1)
    character(*) :: w1

    call mess(LEVEL_ERROR, w1)
end subroutine error1char

subroutine error2char1real(w1, w2, r3)
    real :: r3
    character(*) :: w1, w2

    call mess(LEVEL_ERROR, w1, w2, r3)
end subroutine error2char1real

subroutine error2char2real(w1, w2, r3, r4)
    real :: r3, r4
    character(*) :: w1, w2

    call mess(LEVEL_ERROR, w1, w2, r3, r4)
end subroutine error2char2real

subroutine error1char1real(w1, r2)
    real :: r2
    character(*) :: w1

    call mess(LEVEL_ERROR, w1, r2)
end subroutine error1char1real

subroutine error1char1int(w1, i2)
    integer :: i2
    character(*) :: w1

    call mess(LEVEL_ERROR, w1, i2)
end subroutine error1char1int

subroutine error1char2real(w1, r2, r3)
    real :: r2, r3
    character(*) :: w1

    call mess(LEVEL_ERROR, w1, r2, r3)
end subroutine error1char2real

subroutine error1char2int(w1, i2, i3)
    integer :: i2, i3
    character(*) :: w1

    call mess(LEVEL_ERROR, w1, i2, i3)
end subroutine error1char2int

subroutine error1char1int1double(w1, i2, d3)
    character(*)     :: w1
    integer          :: i2
    double precision :: d3

    call mess(LEVEL_ERROR, w1, i2, d3)
end subroutine error1char1int1double



end module MessageHandling

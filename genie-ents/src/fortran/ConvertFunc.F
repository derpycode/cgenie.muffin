ccc
ccc	Function for converting a number into a string and padding it with front zeros,
ccc	used for forming file names; the output should be trimmed to get rid of blanks
ccc	in case of months/days
ccc

        character(len=10) function ConvertFunc(innumber,switch) result(outname)
        character buf*10,flag*10
        integer innumber, j, bd, switch

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      bd = 0
c ------------------------------------------------------------ c

           if (switch.eq.10) then
                        flag='(I10)'
                        bd=10
                        WRITE (buf,10) innumber
10        FORMAT (I10)
            elseif (switch.eq.2) then
                        flag='(I2)'
                        bd=2
                        WRITE (buf,20) innumber
20        FORMAT (I2)
            elseif (switch.eq.4) then
                        flag='(I4)'
                        bd=4
                        WRITE (buf,30) innumber
30        FORMAT (I4)
            end if

        do j=1,bd
                if (buf(j:j).eq.' ') then 
                        buf(j:j)='0' 
                end if
        end do
        outname=trim(buf)

        end function ConvertFunc

c
      subroutine extrap(mg,jgg,data_array_in,iland_in,iwork_in,xlim)

      implicit none

c     Simple extrapolation program. Any coastal points (iland=1) 
c     are replaced by 
c     their nearest neighbour.

      integer mg,jgg

      real data_array_in(*)
      integer iland_in(*),iwork_in(*)



      integer x
      integer xlim
c     xlim is the number of times this routine is passed through, 
c     i.e. the number of gridboxes through which the routine searches
c     to find a nearest neighbour.
c     xlim is passed to this routine in the header.

      integer i,j,im,ip,jm,jp

      integer, allocatable, dimension(:,:) :: ic
      integer, allocatable, dimension(:,:) :: iland,iwork
      real, allocatable, dimension(:,:) :: asum
      real, allocatable, dimension(:,:) :: data_array

      allocate(ic(mg,jgg))
      allocate(asum(mg,jgg))
      allocate(iland(mg,jgg))
      allocate(iwork(mg,jgg))
      allocate(data_array(mg,jgg))

      do j=1,jgg
        do i=1,mg          
          data_array(i,j)=data_array_in(i+mg*(j-1))
          iland(i,j)=iland_in(i+mg*(j-1))
          iwork(i,j)=iwork_in(i+mg*(j-1))
        enddo
      enddo
c
      do j=1,jgg
         do i=1,mg
            iwork(i,j)=iland(i,j)
         end do
      end do
c
      do x=1,xlim

      do j=1,jgg
         do i=1,mg
            if (iwork(i,j).eq.1) then
               im=i-1
               if (im.lt.1) im=im+mg
               ip=i+1
               if (ip.gt.mg) ip=ip-mg
               jm=j-1
               jp=j+1
               ic(i,j)=0
               asum(i,j)=0.0
               if (jm.ge.1) then
                  if (iwork(im,jm).eq.0) then
                     asum(i,j)=asum(i,j)+data_array(im,jm)
                     ic(i,j)=ic(i,j)+1
                  end if
                  if (iwork(i,jm).eq.0) then
                     asum(i,j)=asum(i,j)+data_array(i,jm)
                     ic(i,j)=ic(i,j)+1
                  end if
                  if (iwork(ip,jm).eq.0) then
                     asum(i,j)=asum(i,j)+data_array(ip,jm)
                     ic(i,j)=ic(i,j)+1
                  end if
               end if
               if (jp.le.jgg) then
                  if (iwork(im,jp).eq.0) then
                     asum(i,j)=asum(i,j)+data_array(im,jp)
                     ic(i,j)=ic(i,j)+1
                  end if
                  if (iwork(i,jp).eq.0) then
                     asum(i,j)=asum(i,j)+data_array(i,jp)
                     ic(i,j)=ic(i,j)+1
                  end if
                  if (iwork(ip,jp).eq.0) then
                     asum(i,j)=asum(i,j)+data_array(ip,jp)
                     ic(i,j)=ic(i,j)+1
                  end if
               end if
               if (iwork(im,j).eq.0) then
                  asum(i,j)=asum(i,j)+data_array(im,j)
                  ic(i,j)=ic(i,j)+1
               end if
               if (iwork(ip,j).eq.0) then
                  asum(i,j)=asum(i,j)+data_array(ip,j)
                  ic(i,j)=ic(i,j)+1
               end if
            end if
         end do
      end do
c
        do j=1,jgg
          do i=1,mg
            if (iwork(i,j).eq.1) then
              if (ic(i,j).gt.0) then
                data_array(i,j)=asum(i,j)/real(ic(i,j))
                iwork(i,j)=0
              end if

            endif
          enddo
        enddo


      enddo

      do j=1,jgg
        do i=1,mg          
          data_array_in(i+mg*(j-1))=data_array(i,j)
          iwork_in(i+mg*(j-1))=iwork(i,j)
        enddo
      enddo

      deallocate(ic)
      deallocate(asum)
      deallocate(iland)
      deallocate(iwork)
      deallocate(data_array)

      return
      end

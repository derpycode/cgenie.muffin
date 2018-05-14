c
c matinv_gold.f solve a set of n linear equations by direct inversion
c          checked for 4x4 random matrix Neil Edwards 13/5/4
c          solves amat*x = rhs, putting result into rhs
c          included in goldstein for case of multiple islands
c          split in to two parts to invert and multiply separately
c          to save a lot of cpu if matrix is constant in time
crma renamed matinv_gold (from matinv) to avoid name conflict (with igcm), 20/9/05
c SAM - 15/4/8 include ocean.cmn here and change size of arrays to be islesmax
c
      subroutine matinv_gold(nvar,amat)

#include "./ocean.cmn"

      integer nvar,i,j,k
      real amat(maxisles,maxisles)
c
c elimination
c
      do i=1,nvar-1
         do j=i+1,nvar
            do k=i+1,nvar
               amat(j,k) = amat(i,i)*amat(j,k) - amat(j,i)*amat(i,k)
            enddo
         enddo
      enddo

      end

      subroutine matmult(nvar,amat,rhs)

#include "./ocean.cmn"

      integer nvar,i,j
      real amat(maxisles,maxisles),rhs(maxisles)

      do i=1,nvar-1
         do j=i+1,nvar
            rhs(j) = amat(i,i)*rhs(j) - amat(j,i)*rhs(i)
         enddo
      enddo
c
c back substitution
c
      rhs(nvar) = rhs(nvar)/amat(nvar,nvar)
      do i=nvar-1,1,-1
         do j=i+1,nvar
            rhs(i) = rhs(i) - amat(i,j)*rhs(j)
         enddo
         rhs(i) = rhs(i)/amat(i,i)
      enddo

c     print*,(rhs(i),i=1,nvar)
c     print*
c     print*,cmat(1,1)*rhs(1) + cmat(1,2)*rhs(2) + cmat(1,3)*rhs(3) 
c    1       + cmat(1,4)*rhs(4) - orhs(1),
c    1       cmat(2,1)*rhs(1) + cmat(2,2)*rhs(2) + cmat(2,3)*rhs(3) 
c    2       + cmat(2,4)*rhs(4) - orhs(2),
c    2       cmat(3,1)*rhs(1) + cmat(3,2)*rhs(2) + cmat(3,3)*rhs(3) 
c    3       + cmat(3,4)*rhs(4) - orhs(3),
c     print*,((amat(i,j),i=1,nvar),j=1,nvar)

      end

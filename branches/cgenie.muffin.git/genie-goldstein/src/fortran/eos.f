c eos.f
c KICO 04/08/08. Sub-routine for equation of state
c KICO 28/03/11. Updated to have two separate fuctions. eos calculates 
c density. eosd calculates vertical density gradient

c--------------------------------------------------
      subroutine eos(ec,t,s,z,ieos,rho)

      implicit none

      real ec(5), t, s, z, rho
      integer ieos 

      if(ieos.eq.0)then
c No thermobaricity term
        rho = ec(1)*t + ec(2)*s + ec(3)*t**2 + ec(4)*t**3
      elseif(ieos.eq.1)then
c Thermobaricity term is in
        rho = ec(1)*t + ec(2)*s + ec(3)*t**2 + ec(4)*t**3
     1            +ec(5)*t*z
      endif

      end

c---------------------------------------------------
      subroutine eosd(ec,t,s,z,rdz,ieos,dzrho,tec)

      implicit none

      real ec(5), t(2), s(2), z, dzrho, tec
      integer ieos
      real tatw, rdz

c Calculate dzrho (vertical density gradient).

      tatw = 0.5*(t(1) + t(2))
      if(ieos.eq.0)then
c No thermobaricity term
        tec = - ec(1) - ec(3)*tatw*2 - ec(4)*tatw*tatw*3
      elseif(ieos.eq.1)then
c Thermobaricity term is in
        tec = - ec(1) - ec(3)*tatw*2 - ec(4)*tatw*tatw*3 - ec(5)*z
      endif
      dzrho = (ec(2)*(s(2)-s(1)) - tec*(t(2)-t(1)))*rdz

      end


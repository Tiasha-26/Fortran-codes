implicit NONE
integer :: i
real ::a,b 
real,dimension(10)::P,t,td,e,es,q,qs,r1,r2
do i=1,10 
read(*,*)P(i),t(i),td(i)
t(i)=t(i)+273.16
td(i)=td(i)+273.16
if (t(i).lt.273.16 .and. td(i).lt.273.16) then 
a=21.87
b=7.66
ELSE
a=17.26
b=35.86
ENDIF
es(i)=6.11*exp((a*(t(i)-273.16))/(t(i)-b))
qs(i)=(0.622*es(i))/(P(i)-es(i))
e(i)=6.11*exp((a*(td(i)-273.16))/(td(i)-b))
q(i)=(0.622*e(i))/(P(i)-e(i))
r1(i)=(q(i)/qs(i))*100
r2(i)=(e(i)/es(i))*100
write(*,*)r1(i),r2(i) 
end do 
end
implicit none 
real,dimension(40)::p,to,rh,n,pv,q,T,date
integer::i 
do i=1,40
read(*,*)p(i),to(i),rh(i),date(i)
T(i)=to(i)+273.16
q(i)=300/T(i)
pv(i)=2.408*10**8*rh(i)*(q(i)**4)*exp(-22.64*q(i))
n(i)= (77.6*(p(i)/T(i)))+64.8*(pv(i)/T(i))+(3.73*10**5)*(pv(i)/(T(i)**2))
write(*,*)n(i)
end do
end 




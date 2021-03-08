! Ushbu dastur yordamida 
! a11*x1 + a12*x2 = b1
! a21*x1 + a22*x2 = b2
! ko`rinishidagi tenglamalar sistemasini Gauss usulida yechish mumkin.
! Ushbu usulning mohiyati berilgan matritsaga teskari matritsani topgan holda tenglama ildizlarini aniqlashdir.
! Muallif: Ashurov Sindor
! Versiya: 1.0.2

IMPLICIT NONE
REAL(8),DIMENSION(2,2)::a,at  !(2x2) o`lchamli 4 ta elementdan tashkil topgan matritsa
REAL(8),DIMENSION(2)::b,x  ! ikkita elementdan tashkil topgan bir o`lchamli matritsa
REAL(8)::deta,s
INTEGER::i,j


WRITE(*,*)"Ushbu dastur Sizga chiziqli tenglamalar sistemasini teskari matritsani topish orqali yechish imkonini beradi"
WRITE(*,*)"Tenglamalar sistemasining koeffitsiyentlarini kiriting:"
WRITE(*,*)"ESLATMA!"
WRITE(*,*)"Siz kiritgan sonlar mos holda a(1,1),a(1,2),a(2,1),a(2,2),b(1),b(2) elementlar tomonidan o`zlashtiriladi."
READ(*,*)a(1,1),a(1,2),a(2,1),a(2,2),b(1),b(2)


! Asosiy determinant hisoblanadi:
! |a11   a12|
! |a21   a22|


deta=a(1,1)*a(2,2)-a(1,2)*a(2,1)

! Transponirlangan a matritsaning elementlarini hisoblaymiz:
at(1,2)=-a(1,2)/deta
at(1,1)=a(2,2)/deta
at(2,1)=-a(2,1)/deta
at(2,2)=a(1,1)/deta

!  Transponirlangan matritsa elementlarini b matritsa elementlariga ko`paytiramiz.
! Asosiy siklning boshlanishi
DO i=1,2
s=0.d0
DO j=1,2
x(i)=s+at(i,j)*b(j)
END DO
END DO

! Natijani chiqaramiz:
WRITE(*,*)x(1),x(2)

END

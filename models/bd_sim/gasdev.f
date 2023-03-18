	FUNCTION gasdev(iDummy)

c    ******************************************************************
c    ** function for the random number generator (Numerical Recipes) **
c    ******************************************************************

	  integer idummy
	  double precision gasdev
	  integer iset
	  double precision fac,gset,rsq,v1,v2,ran2
	  save iset,gset
	  data iset/0/

	  if (iset.eq.0) then
 1	     v1=2.*ran2(idummy)-1.
	     v2=2.*ran2(idummy)-1.
	     rsq=v1**2+v2**2
	     if (rsq.ge.1..or.rsq.eq.0.) goto 1
	     fac=sqrt(-2.*log(rsq)/rsq)
	     gset=v1*fac
	     gasdev=v2*fac
	     iset=1
	  else
	     gasdev=gset
	     iset=0
	  endif

	  return
	  end